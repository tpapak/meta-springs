# springs — IPD + AgD integration (multilevel network meta-regression, ML-NMR)
# on the spring / grand-canonical engine (`gc-rust/src/bin/mlnmr.rs`).
#
# Evidence as springs:
#   * every IPD patient is a Bernoulli spring  p = g^-1(mu_j + x'beta + d_k + x'beta_EM,k)
#   * every AgD arm is ONE spring whose force law is the population average of
#     the individual force laws over its covariate distribution, evaluated by
#     quadrature over integration points:  p_bar = mean_q g^-1(eta_q)
#   * random effects are springs of stiffness 1/tau^2; tau is integrated by the
#     grand-canonical sum over a tau^2 grid, and every tau prior is a
#     reweighting of that single grid.
# The equilibrium (score = 0) is the posterior mode, the curvature of the
# energy well gives the Laplace covariance. No MCMC.
#
# Input networks use the multinma `nma_data` layout (set_ipd / set_agd_arm /
# combine_network / add_integration), so an existing multinma analysis can be
# re-fitted by swapping `multinma::nma()` for `springs::mlnmr()`.

mlnmr_engine_bin <- function() {
  cand <- c(getOption("springs.mlnmr", default = ""),
            Sys.getenv("SPRINGS_MLNMR_BIN", unset = ""),
            file.path(find.package("springs", quiet = TRUE), "..", "..", "gc-rust",
                      "target", "release", "mlnmr"),
            Sys.which("mlnmr"))  # last: on PATH
  cand <- cand[nzchar(cand)]
  for (p in cand) {
    pp <- suppressWarnings(normalizePath(p, mustWork = FALSE))
    if (file.exists(pp)) return(pp)
  }
  stop("springs: could not find the 'mlnmr' engine binary. Build it with\n",
       "  cargo build --release --bin mlnmr\n",
       "and set options(springs.mlnmr=) or $SPRINGS_MLNMR_BIN.", call. = FALSE)
}

# ---------------------------------------------------------------------------
# data: long-format rows (one per IPD patient, one per AgD integration point)
# ---------------------------------------------------------------------------

.mlnmr_rows <- function(network, covs, normal = FALSE) {
  ipd <- network$ipd
  agd <- network$agd_arm
  has_ipd <- !is.null(ipd) && nrow(ipd) > 0
  has_agd <- !is.null(agd) && nrow(agd) > 0
  rows <- list(); units <- list()
  if (has_ipd) {
    d <- data.frame(.study = as.character(ipd$.study), .trt = as.character(ipd$.trt),
                    stringsAsFactors = FALSE)
    for (v in covs) d[[v]] <- as.numeric(ipd[[v]])
    d$.unit <- seq_len(nrow(d)); d$.w <- 1
    rows$ipd <- d
    units$ipd <- if (normal) data.frame(r = 0, n = 1, q = 1, y = as.numeric(ipd$.y), se = NA_real_)
                 else data.frame(r = as.numeric(ipd$.r), n = 1, q = 1, y = NA_real_, se = NA_real_)
  }
  if (has_agd) {
    n_int <- network$n_int %||% 1L
    if (length(covs) && is.null(network$n_int))
      stop("springs::mlnmr: AgD arms need integration points (multinma::add_integration).",
           call. = FALSE)
    ss <- if (!is.null(agd$.sample_size)) agd$.sample_size else agd$.n
    off <- if (has_ipd) nrow(rows$ipd) else 0
    q <- if (length(covs)) n_int else 1L
    d <- data.frame(.study = rep(as.character(agd$.study), each = q),
                    .trt = rep(as.character(agd$.trt), each = q), stringsAsFactors = FALSE)
    for (v in covs) {
      col <- agd[[paste0(".int_", v)]]
      if (is.null(col)) stop("springs::mlnmr: no integration points for covariate '", v, "'.",
                             call. = FALSE)
      d[[v]] <- as.numeric(unlist(col))
    }
    d$.unit <- off + rep(seq_len(nrow(agd)), each = q)
    d$.w <- rep(ss / q, each = q)
    rows$agd <- d
    units$agd <- if (normal) data.frame(r = 0, n = as.numeric(ss), q = q, y = as.numeric(agd$.y),
                                        se = as.numeric(agd$.se))
                 else data.frame(r = as.numeric(agd$.r), n = as.numeric(agd$.n), q = q, y = NA_real_, se = NA_real_)
  }
  list(rows = do.call(rbind, rows), units = do.call(rbind, units))
}

# Parse the regression formula into prognostic terms and effect-modifier terms.
.mlnmr_terms <- function(regression) {
  if (is.null(regression)) return(list(main = character(), em = character(), vars = character()))
  tl <- attr(terms(regression), "term.labels")
  has_trt <- vapply(strsplit(tl, ":", fixed = TRUE), function(p) ".trt" %in% p, logical(1))
  em <- vapply(strsplit(tl[has_trt], ":", fixed = TRUE),
               function(p) paste(setdiff(p, ".trt"), collapse = ":"), character(1))
  em <- em[nzchar(em)]
  list(main = tl[!has_trt], em = em, vars = setdiff(all.vars(regression), c(".trt", ".trtclass")))
}

# Design matrix for arbitrary rows (columns: mu[study], d[trt], beta[...]).
.mlnmr_design <- function(rows, spec) {
  n <- nrow(rows)
  for (v in names(spec$xbar)) rows[[v]] <- rows[[v]] - spec$xbar[[v]]
  cols <- list()
  for (s in spec$studies) cols[[sprintf("mu[%s]", s)]] <- as.numeric(rows$.study == s)
  for (t in spec$trts[-1]) cols[[sprintf("d[%s]", t)]] <- as.numeric(rows$.trt == t)
  mm <- function(term) {
    m <- model.matrix(as.formula(paste("~", term)), rows)
    m[, colnames(m) != "(Intercept)", drop = FALSE]
  }
  for (term in spec$terms$main) {
    m <- mm(term)
    for (j in seq_len(ncol(m))) cols[[sprintf("beta[%s]", colnames(m)[j])]] <- m[, j]
  }
  grp <- if (spec$class_interactions == "common") spec$trt_class[rows$.trt] else rows$.trt
  glev <- spec$em_groups
  gname <- if (spec$class_interactions == "common") ".trtclass" else ".trt"
  for (term in spec$terms$em) {
    m <- mm(term)
    for (j in seq_len(ncol(m))) for (g in glev)
      cols[[sprintf("beta[%s:%s%s]", colnames(m)[j], gname, g)]] <- m[, j] * (grp == g)
  }
  if (spec$random) for (k in seq_along(spec$re_arms)) {
    a <- spec$re_arms[[k]]
    cols[[sprintf("delta[%s: %s]", a$study, a$trt)]] <- as.numeric(rows$.study == a$study & rows$.trt == a$trt)
  }
  X <- do.call(cbind, cols)
  if (is.null(dim(X))) X <- matrix(X, nrow = n, dimnames = list(NULL, names(cols)))
  X
}

#' Multilevel network meta-regression (IPD + AgD) on the spring engine
#'
#' Fits the ML-NMR model of Phillippo et al. (2020) — IPD patients as individual
#' springs, AgD arms as covariate-averaged springs — without MCMC.
#'
#' @param network A multinma-style `nma_data` object (from `set_ipd()`,
#'   `set_agd_arm()`, `combine_network()` and, when AgD arms are present with a
#'   regression, `add_integration()`).
#' @param regression One-sided formula, multinma syntax, e.g.
#'   `~(age + weight)*.trt`. Terms with `.trt` are effect modifiers.
#' @param class_interactions `"common"` (one interaction per treatment class) or
#'   `"independent"` (one per treatment).
#' @param trt_effects `"fixed"` or `"random"`.
#' @param link `"probit"` or `"logit"`.
#' @param likelihood `"bernoulli2"` (two-parameter binomial approximation for AgD
#'   arms, multinma's ML-NMR default) or `"bernoulli"` (one-parameter).
#' @param prior_intercept,prior_trt,prior_reg Priors on the study intercepts,
#'   treatment effects and regression coefficients: a number (Normal(0, SD²) prior
#'   SD; `Inf` = flat) or a multinma prior object (`normal()`, `cauchy()`,
#'   `student_t()`, `flat()`), any location.
#' @param keep_pool Keep the weighted importance pool, so that
#'   [mlnmr_reweight()] / [mlnmr_prior_panel()] can re-score the posterior under
#'   other priors without refitting.
#' @param tau_priors Heterogeneity priors (random effects): the first is the
#'   primary analysis (draws); the rest are a free sensitivity analysis from the
#'   same grid. Specs: `hn:S`, `hc:S`, `exp:R`, `unif:U` (on τ), `ln:MU:SIGMA`
#'   (log-normal on τ²).
#' @param center Centre covariates at the network mean (as multinma).
#' @param samples Posterior draws.
#' @param seed RNG seed.
#' @param n_grid τ² grid points (random effects).
#' @param correction `"is"` (default): the Laplace spring solution is used as an
#'   importance proposal (multivariate t) and reweighted by the exact energy —
#'   exact posterior moments and partition function, with the effective sample
#'   size as a diagnostic of how harmonic the springs are. `"laplace"`: the
#'   pure equilibrium + curvature approximation.
#' @param is_per_point Importance draws per τ² grid point (random effects).
#' @return An object of class `springs_mlnmr`.
#' @export
mlnmr <- function(network, regression = NULL, class_interactions = c("common", "independent"),
                  trt_effects = c("fixed", "random"), link = c("probit", "logit", "identity"),
                  likelihood = c("bernoulli2", "bernoulli", "normal"),
                  prior_intercept = 10, prior_trt = 10, prior_reg = 10, prior_aux = 5,
                  tau_priors = "hn:2.5", center = TRUE, samples = 4000, seed = 1,
                  n_grid = 40, correction = c("is", "laplace"), is_per_point = 1000,
                  keep_pool = TRUE, .build_only = FALSE) {
  correction <- match.arg(correction)
  class_interactions <- match.arg(class_interactions)
  trt_effects <- match.arg(trt_effects); link <- match.arg(link)
  likelihood <- match.arg(likelihood)
  normal <- likelihood == "normal"
  if (normal) link <- "identity"
  if (!normal && link == "identity")
    stop("springs::mlnmr: the identity link needs likelihood = 'normal'.", call. = FALSE)
  t0 <- Sys.time()

  trts <- levels(network$treatments)
  tm <- .mlnmr_terms(regression)
  if (class_interactions == "common" && length(tm$em) && is.null(network$classes))
    stop("springs::mlnmr: class_interactions = 'common' needs treatment classes.", call. = FALSE)
  trt_class <- if (!is.null(network$classes)) setNames(as.character(network$classes), trts) else NULL
  dat <- .mlnmr_rows(network, tm$vars, normal)
  rows <- dat$rows
  studies <- unique(c(if (!is.null(network$ipd)) as.character(unique(network$ipd$.study)),
                      if (!is.null(network$agd_arm)) as.character(unique(network$agd_arm$.study))))
  xbar <- if (center && length(tm$vars))
    vapply(tm$vars, function(v) weighted.mean(rows[[v]], rows$.w), numeric(1)) else numeric()

  # random effects: one delta per arm not on the network reference treatment
  re_arms <- list()
  if (trt_effects == "random") {
    arms <- unique(rows[, c(".study", ".trt")])
    for (s in studies) {
      ts <- arms$.trt[arms$.study == s]
      ts <- ts[order(match(ts, trts))]
      # multinma convention: random effects are relative to the network reference;
      # a study without the reference arm has a delta on EVERY arm
      re_t <- if (trts[1] %in% ts) ts[-1] else ts
      for (t in re_t) re_arms[[length(re_arms) + 1]] <- list(study = s, trt = t)
    }
  }
  em_groups <- if (class_interactions == "common")
    setdiff(levels(network$classes), trt_class[[trts[1]]]) else trts[-1]
  spec <- list(studies = studies, trts = trts, trt_class = trt_class, terms = tm, xbar = xbar,
               class_interactions = class_interactions, em_groups = em_groups,
               random = trt_effects == "random", re_arms = re_arms, link = link)
  X <- .mlnmr_design(rows, spec)
  # Gaussian IPD: one residual SD per IPD arm (multinma convention), as a log σ
  # column with no design entries
  ipd_st <- character()
  if (normal && !is.null(network$ipd)) {
    ia <- unique(data.frame(s = as.character(network$ipd$.study), t = as.character(network$ipd$.trt)))
    ia <- ia[order(match(ia$s, studies), match(ia$t, trts)), ]
    ipd_st <- sprintf("%s: %s", ia$s, ia$t)
    X <- cbind(X, matrix(0, nrow(X), length(ipd_st), dimnames = list(NULL, sprintf("log_sigma[%s]", ipd_st))))
  }
  pn <- colnames(X)
  prior_spec <- list(intercept = .mlnmr_prior(prior_intercept), trt = .mlnmr_prior(prior_trt),
                     reg = .mlnmr_prior(prior_reg),
                     aux = list(dist = "halfnormal_log", loc = 0, scale = prior_aux, df = 1))
  eng_priors <- lapply(pn, function(j) {
    g <- .mlnmr_par_group(j)
    pr <- if (is.na(g)) .mlnmr_prior(Inf) else prior_spec[[g]]
    if (pr$dist == "flat") list(dist = "flat") else pr[c("dist", "loc", "scale", "df")]
  })
  re_groups <- list()
  if (spec$random) {
    di <- which(startsWith(pn, "delta["))
    st <- vapply(re_arms, `[[`, "", "study")
    re_groups <- unname(lapply(split(di - 1L, st), as.integer))   # 0-based for the engine
    re_groups <- lapply(re_groups[lengths(re_groups) > 0], I)   # keep 1-element groups as arrays
  }

  # engine units: IPD rows (q = 1), AgD arms (q = n_int rows)
  u <- dat$units
  idx <- split(seq_len(nrow(X)), rows$.unit)
  ustudy <- rows$.study[match(seq_len(nrow(u)), rows$.unit)]
  uarm <- sprintf("%s: %s", ustudy, rows$.trt[match(seq_len(nrow(u)), rows$.unit)])
  units <- lapply(seq_len(nrow(u)), function(i) {
    xi <- as.vector(t(X[idx[[i]], , drop = FALSE]))
    if (!normal) return(list(r = u$r[i], n = u$n[i], q = length(idx[[i]]), x = xi))
    if (u$q[i] == 1 && is.na(u$se[i]))      # an IPD patient: Hooke spring with σ of its study
      list(kind = "ipd_normal", y = u$y[i], sig = match(sprintf("log_sigma[%s]", uarm[i]), pn) - 1L,
           q = 1L, x = xi)
    else list(kind = "arm_normal", y = u$y[i], se = u$se[i], n = u$n[i], q = length(idx[[i]]), x = xi)
  })
  init <- numeric(length(pn))
  for (st in ipd_st) init[match(sprintf("log_sigma[%s]", st), pn)] <-
    log(max(stats::sd(u$y[uarm == st & is.na(u$se)]), 1e-3))
  if (.build_only) {
    # the design only (used to build per-site data for federated fitting)
    unit_study <- rows$.study[match(seq_len(nrow(u)), rows$.unit)]
    return(list(units = units, unit_study = unit_study, par_names = pn, prior_spec = prior_spec, init = init,
                spec = spec, xbar = xbar, rows = rows, link = link, likelihood = likelihood))
  }
  inp <- tempfile(fileext = ".json"); out <- tempfile(fileext = ".json")
  on.exit(unlink(c(inp, out)), add = TRUE)
  jsonlite::write_json(list(link = link, agd_likelihood = likelihood, par_names = pn,
                            priors = eng_priors, return_pool = keep_pool && correction == "is",
                            re_groups = re_groups, tau_priors = as.list(tau_priors),
                            units = units, samples = samples, seed = seed, n_grid = n_grid,
                            correction = correction, is_per_point = is_per_point, init = init),
                       inp, auto_unbox = TRUE, digits = NA)
  t_prep <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  errf <- tempfile(fileext = ".txt"); on.exit(unlink(errf), add = TRUE)
  st <- system2(mlnmr_engine_bin(), c("--input", inp, "--output", out), stdout = FALSE, stderr = errf)
  if (!identical(st, 0L) || !file.exists(out))
    stop("springs::mlnmr: engine failed (status ", st, "): ",
         paste(utils::tail(readLines(errf, warn = FALSE), 5), collapse = "\n"), call. = FALSE)
  rep <- jsonlite::fromJSON(out, simplifyVector = TRUE)
  draws <- rep$draws
  colnames(draws) <- rep$draw_names

  # covariate means of each study population (IPD: patients; AgD: integration points)
  pop <- if (length(tm$vars)) {
    agg <- lapply(split(rows, rows$.study), function(d)
      vapply(tm$vars, function(v) weighted.mean(d[[v]], d$.w), numeric(1)))
    as.data.frame(do.call(rbind, agg[studies]), row.names = studies)
  } else NULL

  mode <- setNames(rep$mode, rep$fixed_names)
  cov <- if (!is.null(rep$cov)) matrix(unlist(rep$cov), nrow = length(mode), byrow = TRUE,
                                       dimnames = list(rep$fixed_names, rep$fixed_names)) else NULL
  pool <- NULL
  if (!is.null(rep$pool)) {
    as_mat <- function(x) if (is.matrix(x)) x else do.call(rbind, lapply(x, unlist))
    pv <- as_mat(rep$pool$values); colnames(pv) <- rep$pool$names
    pgw <- if (length(rep$pool$prior_grid_w)) as_mat(rep$pool$prior_grid_w) else NULL
    if (!is.null(pgw)) rownames(pgw) <- tau_priors
    pool <- list(values = pv, w = rep$pool$w, grid = as.integer(rep$pool$grid) + 1L, prior_grid_w = pgw)
  }
  summ <- .mlnmr_sum_tab(rep$priors$pars[[1]])
  if (length(ipd_st)) {                      # report σ = exp(log σ) on its natural scale
    ls <- grep("^log_sigma\\[", colnames(draws), value = TRUE)
    sg <- exp(draws[, ls, drop = FALSE]); colnames(sg) <- sub("^log_", "", ls)
    summ <- rbind(summ[!startsWith(summ$parameter, "log_sigma["), ], .q_tab(sg, colnames(sg)))
  }
  res <- list(draws = draws, mode = mode, cov = cov, prior_spec = prior_spec, pool = pool,
              network_call = list(network = network), n_grid = n_grid, is_per_point = is_per_point, summary = summ,
              priors = rep$priors, tau = if (spec$random) rep$priors$tau else NULL,
              spec = spec, regression = regression, trt_effects = trt_effects,
              likelihood = likelihood, link = link, correction = correction,
              ess = rep$priors$ess, log_z_correction = rep$log_z_correction,
              xbar = xbar, pop_means = pop,
              rows = rows, n_units = rep$n_units, n_par = rep$n_par, n_re = rep$n_re,
              log_lik_mode = rep$log_lik_mode, converged = rep$converged,
              observed_information = rep$observed_information,
              grid = data.frame(tau2 = rep$grid_tau2, log_z = rep$grid_log_z,
                                log_z_laplace = rep$grid_log_z_laplace %||% numeric(),
                                ess = rep$grid_ess %||% numeric())[seq_along(rep$grid_tau2), , drop = FALSE],
              engine_sec = rep$fit_wall_sec, prep_sec = t_prep,
              wall_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  class(res) <- "springs_mlnmr"
  res
}

.mlnmr_sum_tab <- function(p) {
  data.frame(parameter = p$name, mean = p$mean, sd = p$sd, `2.5%` = p$q025, `50%` = p$q50,
             `97.5%` = p$q975, check.names = FALSE, stringsAsFactors = FALSE)
}

#' @export
print.springs_mlnmr <- function(x, digits = 3, ...) {
  cat(sprintf("springs ML-NMR: %s effects, %s link, %s likelihood, %s (no MCMC)\n",
              x$trt_effects, x$link, x$likelihood,
              if (x$correction == "is") sprintf("importance-corrected Laplace, ESS %.0f", x$ess[1])
              else "Laplace"))
  if (!is.null(x$regression)) cat("Regression:", deparse(x$regression), "\n")
  if (length(x$xbar)) { cat("Centred at:\n"); print(round(x$xbar, 4)) }
  cat(sprintf("%d evidence units (springs), %d parameters (%d random effects); fit %.2f s (engine %.2f s)\n",
              x$n_units, x$n_par, x$n_re, x$wall_sec, x$engine_sec))
  s <- x$summary
  s <- s[!startsWith(s$parameter, "mu["), ]
  if (!is.null(x$tau)) {
    tt <- x$tau[1, ]
    s <- rbind(s, data.frame(parameter = "tau", mean = tt$mean, sd = tt$sd, `2.5%` = tt$q025,
                             `50%` = tt$q50, `97.5%` = tt$q975, check.names = FALSE))
  }
  print(format(s, digits = digits), row.names = FALSE)
  invisible(x)
}

#' @export
summary.springs_mlnmr <- function(object, ...) object$summary

# contrast rows: treatment t vs reference at covariate values `x` (uncentred)
.mlnmr_contrast <- function(fit, x) {
  sp <- fit$spec; sp$random <- FALSE
  trts <- sp$trts
  r <- data.frame(.study = sp$studies[1], .trt = trts, stringsAsFactors = FALSE)
  for (v in names(x)) r[[v]] <- x[[v]]
  X <- .mlnmr_design(r, sp)
  C <- sweep(X[-1, , drop = FALSE], 2, X[1, ], "-")
  rownames(C) <- trts[-1]
  C[, colnames(fit$draws)[colnames(fit$draws) %in% colnames(C)], drop = FALSE]
}

.q_tab <- function(M, names) {
  data.frame(parameter = names, mean = colMeans(M), sd = apply(M, 2, sd),
             `2.5%` = apply(M, 2, quantile, .025), `50%` = apply(M, 2, quantile, .5),
             `97.5%` = apply(M, 2, quantile, .975), check.names = FALSE, row.names = NULL)
}

#' Population-specific relative effects (vs the network reference)
#'
#' @param fit A `springs_mlnmr` fit.
#' @param newdata Optional data frame of covariate values, one row per target
#'   population (uncentred, same scale as the data). Default: every study
#'   population in the network (IPD: patient means; AgD: integration-point means).
#' @param all_contrasts Also return all pairwise contrasts.
#' @return Data frame: population, contrast, mean, sd, quantiles.
#' @export
mlnmr_relative_effects <- function(fit, newdata = NULL, all_contrasts = FALSE) {
  pops <- if (is.null(newdata)) fit$pop_means else newdata
  if (is.null(pops)) pops <- data.frame(row.names = "network")
  if (is.null(rownames(pops))) rownames(pops) <- paste0("population ", seq_len(nrow(pops)))
  trts <- fit$spec$trts
  out <- lapply(rownames(pops), function(pn) {
    C <- .mlnmr_contrast(fit, as.list(pops[pn, , drop = FALSE]))
    D <- fit$draws[, colnames(C), drop = FALSE] %*% t(C)
    nm <- sprintf("d[%s: %s]", pn, trts[-1])
    if (all_contrasts) {
      Dall <- cbind(0, D); colnames(Dall) <- trts
      pr <- utils::combn(trts, 2)
      D <- do.call(cbind, lapply(seq_len(ncol(pr)), function(k) Dall[, pr[2, k]] - Dall[, pr[1, k]]))
      nm <- sprintf("d[%s: %s vs %s]", pn, pr[2, ], pr[1, ])
    }
    cbind(population = pn, .q_tab(D, nm))
  })
  do.call(rbind, out)
}

#' Population-average event probabilities for each study population
#'
#' For each study population and each treatment: the mean over that population's
#' covariate distribution (IPD patients / AgD integration points) of
#' g^-1(mu_j + design), i.e. the marginal probability ML-NMR predicts.
#' @param fit A `springs_mlnmr` fit.
#' @param n_draws Posterior draws used.
#' @export
mlnmr_predict <- function(fit, n_draws = 1000) {
  sp <- fit$spec; sp$random <- FALSE
  ginv <- switch(fit$link, probit = stats::pnorm, logit = stats::plogis, identity = identity)
  S <- fit$draws[seq_len(min(n_draws, nrow(fit$draws))), , drop = FALSE]
  out <- list()
  for (s in sp$studies) {
    d <- fit$rows[fit$rows$.study == s, ]
    w <- d$.w / sum(d$.w)
    for (t in sp$trts) {
      d$.trt <- t
      X <- .mlnmr_design(d, sp)
      X <- X[, colnames(S)[colnames(S) %in% colnames(X)], drop = FALSE]
      P <- ginv(X %*% t(S[, colnames(X), drop = FALSE]))   # rows x draws
      pbar <- as.vector(crossprod(w, P))
      out[[length(out) + 1]] <- data.frame(population = s, treatment = t, mean = mean(pbar),
        sd = sd(pbar), `2.5%` = quantile(pbar, .025), `50%` = quantile(pbar, .5),
        `97.5%` = quantile(pbar, .975), check.names = FALSE, row.names = NULL)
    }
  }
  do.call(rbind, out)
}

#' Heterogeneity-prior sensitivity of an RE ML-NMR (free: one grid)
#'
#' @param fit A random-effects `springs_mlnmr` fit with several `tau_priors`.
#' @param pars Parameters to report (default: treatment effects d[.]).
#' @return Data frame: prior × parameter, plus tau and log BF (RE vs FE).
#' @export
mlnmr_sensitivity <- function(fit, pars = NULL) {
  if (is.null(fit$tau)) stop("springs::mlnmr_sensitivity: needs a random-effects fit.", call. = FALSE)
  pr <- fit$priors
  do.call(rbind, lapply(seq_len(nrow(pr)), function(i) {
    tab <- .mlnmr_sum_tab(pr$pars[[i]])
    if (is.null(pars)) pars <- tab$parameter[startsWith(tab$parameter, "d[")]
    tab <- tab[tab$parameter %in% pars, ]
    tt <- pr$tau[i, ]
    rbind(cbind(prior = pr$prior[i], tab),
          data.frame(prior = pr$prior[i], parameter = "tau", mean = tt$mean, sd = tt$sd,
                     `2.5%` = tt$q025, `50%` = tt$q50, `97.5%` = tt$q975, check.names = FALSE)) |>
      transform(log_bf_re_fe = pr$log_bf_re_fe[i])
  }))
}


# ---------------------------------------------------------------------------
# priors: specification, and re-scoring the posterior under other priors
# ---------------------------------------------------------------------------

# which prior group a parameter belongs to (NA: random effect, no prior spring)
.mlnmr_par_group <- function(j) {
  if (startsWith(j, "mu[")) "intercept" else if (startsWith(j, "d[")) "trt"
  else if (startsWith(j, "beta[")) "reg" else if (startsWith(j, "log_sigma[")) "aux" else NA_character_
}

# normalise a prior: number (Normal SD; Inf = flat), multinma prior object, or list
.mlnmr_prior <- function(x) {
  flat <- list(dist = "flat", loc = 0, scale = Inf, df = 1)
  if (is.null(x)) return(flat)
  if (is.numeric(x) && length(x) == 1)
    return(if (is.finite(x)) list(dist = "normal", loc = 0, scale = x, df = 1) else flat)
  f <- sub(".*::", "", tolower(x$fun %||% x$dist))
  num <- function(v, d) if (is.null(v) || length(v) == 0 || is.na(v)) d else as.numeric(v)
  loc <- num(x$location %||% x$loc, 0); sc <- num(x$scale, Inf); df <- num(x$df, 1)
  if (!is.finite(sc)) return(flat)
  switch(f,
         normal = list(dist = "normal", loc = loc, scale = sc, df = 1),
         cauchy = list(dist = "t", loc = loc, scale = sc, df = 1),
         student_t = , t = list(dist = "t", loc = loc, scale = sc, df = df),
         flat = flat,
         stop("springs::mlnmr: unsupported prior '", f, "' (normal, cauchy, student_t, flat).",
              call. = FALSE))
}

.mlnmr_prior_label <- function(p) {
  p <- .mlnmr_prior(p)
  g <- function(x) format(signif(x, 3))
  if (identical(p$dist, "halfnormal_log")) return(sprintf("HN(%s) on σ", format(signif(p$scale, 3))))
  switch(p$dist, flat = "flat",
         normal = sprintf("N(%s, %s²)", g(p$loc), g(p$scale)),
         t = if (p$df == 1) sprintf("Cauchy(%s, %s)", g(p$loc), g(p$scale))
             else sprintf("t%s(%s, %s)", g(p$df), g(p$loc), g(p$scale)))
}

.mlnmr_prior_logd <- function(p, x) {
  switch(p$dist, flat = rep(0, length(x)),
         halfnormal_log = stats::dnorm(exp(x), 0, p$scale, log = TRUE) + log(2) + x,
         normal = stats::dnorm(x, p$loc, p$scale, log = TRUE),
         t = stats::dt((x - p$loc) / p$scale, p$df, log = TRUE) - log(p$scale))
}

# weighted summary of the columns of V
.w_tab <- function(V, w) {
  q <- function(x, p) {
    o <- order(x); x <- x[o]; cw <- cumsum(w[o]) - 0.5 * w[o]
    stats::approx(cw, x, xout = p, rule = 2, ties = "ordered")$y
  }
  do.call(rbind, lapply(colnames(V), function(j) {
    x <- V[, j]; m <- sum(w * x)
    data.frame(parameter = j, mean = m, sd = sqrt(sum(w * (x - m)^2)), `2.5%` = q(x, .025),
               `50%` = q(x, .5), `97.5%` = q(x, .975), check.names = FALSE)
  }))
}

#' Re-score an ML-NMR posterior under different priors, without refitting
#'
#' The spring fit keeps its weighted importance pool: draws θ_i with weights
#' w_i ∝ exp(−E(θ_i))/q(θ_i). The energy is a sum of data springs and prior
#' springs, so changing a prior only changes the (cheap) prior springs:
#' w_i' = w_i · π'(θ_i)/π(θ_i). The expensive part — every patient and every
#' integration point — is never touched again. For random effects, the τ prior
#' acts on the grand-canonical τ² grid (any prior in the fit's `tau_priors`), and
#' prior changes on θ and on τ combine in the same weights.
#'
#' The effective sample size of the new weights says how far the new prior moved
#' the posterior away from the pool: when it is small (`min_ess`), the function
#' refits with the new priors (an ordinary sub-second spring fit) instead.
#'
#' @param fit A `springs_mlnmr` fit made with `keep_pool = TRUE`.
#' @param prior_intercept,prior_trt,prior_reg New priors (`NULL` = unchanged);
#'   same forms as in [mlnmr()].
#' @param tau_prior One of the fit's `tau_priors` (random effects; default: the first).
#' @param samples Resampled posterior draws to return.
#' @param min_ess Refit when the reweighted ESS falls below this (`0` = never).
#' @param seed Resampling seed.
#' @return A `springs_mlnmr` object (draws, summary, tau) under the new priors,
#'   usable with [mlnmr_relative_effects()] and [mlnmr_predict()]; `$reweight`
#'   holds the ESS and whether a refit was needed.
#' @export
mlnmr_reweight <- function(fit, prior_intercept = NULL, prior_trt = NULL, prior_reg = NULL,
                           tau_prior = NULL, samples = nrow(fit$draws), min_ess = 1000, seed = 1) {
  t0 <- Sys.time()
  pl <- fit$pool
  if (is.null(pl)) stop("springs::mlnmr_reweight: refit with keep_pool = TRUE (and correction = 'is').",
                        call. = FALSE)
  new <- fit$prior_spec
  if (!is.null(prior_intercept)) new$intercept <- .mlnmr_prior(prior_intercept)
  if (!is.null(prior_trt)) new$trt <- .mlnmr_prior(prior_trt)
  if (!is.null(prior_reg)) new$reg <- .mlnmr_prior(prior_reg)
  V <- pl$values
  lr <- numeric(nrow(V))
  for (j in colnames(V)) {
    g <- .mlnmr_par_group(j)
    if (is.na(g) || identical(new[[g]], fit$prior_spec[[g]])) next
    lr <- lr + .mlnmr_prior_logd(new[[g]], V[, j]) - .mlnmr_prior_logd(fit$prior_spec[[g]], V[, j])
  }
  k <- 1L
  base <- pl$w
  if (!is.null(pl$prior_grid_w)) {
    if (!is.null(tau_prior)) {
      k <- match(tau_prior, rownames(pl$prior_grid_w))
      if (is.na(k)) stop("springs::mlnmr_reweight: tau_prior must be one of the fit's tau_priors: ",
                         paste(rownames(pl$prior_grid_w), collapse = ", "), call. = FALSE)
    }
    base <- pl$w * pl$prior_grid_w[k, pl$grid]
  } else if (!is.null(tau_prior)) stop("springs::mlnmr_reweight: tau_prior needs a random-effects fit.",
                                       call. = FALSE)
  lw <- log(base) + lr
  w <- exp(lw - max(lw[is.finite(lw)])); w[!is.finite(w)] <- 0; w <- w / sum(w)
  ess <- 1 / sum(w^2)
  tau_lab <- if (is.null(pl$prior_grid_w)) NULL else rownames(pl$prior_grid_w)[k]
  out <- fit
  if (ess < min_ess) {
    # the new prior moved the posterior too far from the pool: refit (still no MCMC)
    out <- mlnmr(fit$network_call$network, regression = fit$regression, center = length(fit$xbar) > 0,
                 class_interactions = fit$spec$class_interactions, trt_effects = fit$trt_effects,
                 link = fit$link, likelihood = fit$likelihood,
                 prior_intercept = new$intercept, prior_trt = new$trt, prior_reg = new$reg,
                 tau_priors = if (is.null(tau_lab)) "hn:1" else c(tau_lab, setdiff(rownames(pl$prior_grid_w), tau_lab)),
                 samples = samples, seed = seed, n_grid = fit$n_grid, is_per_point = fit$is_per_point)
    out$reweight <- list(ess = ess, refit = TRUE, tau_prior = tau_lab,
                         sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))
    return(out)
  }
  # systematic resampling for downstream draws
  set.seed(seed)
  u <- (stats::runif(1) + seq_len(samples) - 1) / samples
  idx <- findInterval(u, cumsum(w), left.open = TRUE) + 1L
  idx <- pmin(idx, nrow(V))[sample.int(samples)]
  tab <- .w_tab(V, w)
  out$draws <- V[idx, , drop = FALSE]
  out$summary <- tab[tab$parameter != "tau", ]
  if ("tau" %in% tab$parameter) {
    tt <- tab[tab$parameter == "tau", ]
    out$tau <- data.frame(mean = tt$mean, sd = tt$sd, q025 = tt$`2.5%`, q50 = tt$`50%`, q975 = tt$`97.5%`)
  }
  out$prior_spec <- new
  out$ess <- ess
  out$reweight <- list(ess = ess, refit = FALSE, tau_prior = tau_lab,
                       sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  out
}

#' Prior panel: the posterior under many priors from one spring fit
#'
#' @param fit A `springs_mlnmr` fit (`keep_pool = TRUE`).
#' @param panel Named list; each element a list with any of `intercept`, `trt`,
#'   `reg` (prior forms as in [mlnmr()]). Default: the fit's own priors.
#' @param tau_priors τ priors to cross with the panel (random effects; default:
#'   all priors of the fit).
#' @param pars Parameters to report (default: treatment effects and regression
#'   coefficients).
#' @param newdata Optional target populations (as in [mlnmr_relative_effects()]):
#'   their population-specific relative effects are reported too.
#' @param min_ess Passed to [mlnmr_reweight()].
#' @return Long data frame: prior, tau_prior, parameter, posterior summary, ESS,
#'   refit flag and seconds.
#' @export
mlnmr_prior_panel <- function(fit, panel = list(fit = list()), tau_priors = NULL, pars = NULL,
                              newdata = NULL, min_ess = 1000) {
  taus <- if (is.null(fit$pool$prior_grid_w)) list(NULL)
          else as.list(tau_priors %||% rownames(fit$pool$prior_grid_w))
  if (is.null(names(panel))) names(panel) <- paste0("prior ", seq_along(panel))
  rows <- list()
  for (nm in names(panel)) for (tp in taus) {
    pr <- panel[[nm]]
    rw <- mlnmr_reweight(fit, prior_intercept = pr$intercept, prior_trt = pr$trt, prior_reg = pr$reg,
                         tau_prior = tp, min_ess = min_ess)
    tab <- rw$summary
    keep <- if (is.null(pars)) startsWith(tab$parameter, "d[") | startsWith(tab$parameter, "beta[")
            else tab$parameter %in% pars
    tab <- tab[keep, ]
    if (!is.null(rw$tau)) tab <- rbind(tab, data.frame(parameter = "tau", mean = rw$tau$mean, sd = rw$tau$sd,
      `2.5%` = rw$tau$q025, `50%` = rw$tau$q50, `97.5%` = rw$tau$q975, check.names = FALSE))
    if (!is.null(newdata)) {
      re <- mlnmr_relative_effects(rw, newdata = newdata)
      tab <- rbind(tab, re[, names(tab)])
    }
    rows[[length(rows) + 1]] <- cbind(prior = nm, tau_prior = if (is.null(tp)) NA_character_ else tp,
                                      tab, ess = rw$reweight$ess, refit = rw$reweight$refit,
                                      sec = rw$reweight$sec)
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

#' How strongly each prior spring holds each parameter
#'
#' Spring reading of prior influence: the prior is one more spring on θ_j; its
#' stiffness (expected curvature, 1/scale² for a Normal prior) over the total
#' stiffness of the posterior (1/Var(θ_j | data)) is the share of the
#' restoring force that comes from the prior rather than the data. ≈0: the data
#' decide; ≈1: the prior decides. It needs only the fitted posterior.
#'
#' @param fit A `springs_mlnmr` fit.
#' @return Data frame: parameter, prior, posterior SD, prior share.
#' @export
mlnmr_prior_share <- function(fit) {
  tab <- fit$summary
  do.call(rbind, lapply(seq_len(nrow(tab)), function(i) {
    j <- tab$parameter[i]; g <- .mlnmr_par_group(j)
    if (is.na(g) || g == "aux") return(NULL)
    p <- fit$prior_spec[[g]]
    k <- switch(p$dist, flat = 0, normal = 1 / p$scale^2, t = (p$df + 1) / ((p$df + 3) * p$scale^2))
    data.frame(parameter = j, prior = .mlnmr_prior_label(p), posterior_sd = tab$sd[i],
               prior_share = min(1, k * tab$sd[i]^2))
  }))
}
