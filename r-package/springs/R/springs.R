# springs — a gemtc-compatible facade over the spring / Grand-Canonical NMA
# engine. The aim is bit-for-bit API compatibility with the slice of `gemtc`
# that MetaInsight's Bayesian path uses, so MetaInsight works by swapping the
# `gemtc::` namespace for `springs::`.
#
# Orientation convention (identical to gemtc): a basic parameter `d.A.B` is the
# effect of B relative to A. `relative.effect.table()[A, B, ]` is the effect of
# B relative to A. This matches gemtc so downstream consumers (MetaInsight,
# cinemar) need no orientation change.

# ---------------------------------------------------------------------------
# Engine binary resolution
# ---------------------------------------------------------------------------

#' Locate the `mtc` engine binary
#'
#' Resolution order: option `springs.engine`, env var `SPRINGS_MTC_BIN`, then a
#' set of conventional build locations relative to the package / repo.
#' @return Absolute path to the `mtc` binary (errors if not found).
#' @export
springs_engine_bin <- function() {
  cand <- c(
    getOption("springs.engine", default = ""),
    Sys.getenv("SPRINGS_MTC_BIN", unset = ""),
    # Dev tree: package lives at meta-springs/r-package/springs.
    file.path(find.package("springs", quiet = TRUE), "..", "..", "gc-rust",
              "target", "release", "mtc"),
    "/Users/tosku/Sync/Documents/meta-springs/gc-rust/target/release/mtc"
  )
  cand <- cand[nzchar(cand)]
  for (p in cand) {
    pp <- suppressWarnings(normalizePath(p, mustWork = FALSE))
    if (file.exists(pp)) return(pp)
  }
  stop("springs: could not find the 'mtc' engine binary. Build it with\n",
       "  cargo build --release --bin mtc\n",
       "and set options(springs.engine = '/path/to/mtc') or $SPRINGS_MTC_BIN.",
       call. = FALSE)
}

# ---------------------------------------------------------------------------
# Network / model constructors (gemtc-shaped)
# ---------------------------------------------------------------------------

#' Build a network object from arm-level data (gemtc `mtc.network` analogue)
#'
#' @param data.ab Arm-level data frame. Binary: columns `study`, `treatment`,
#'   `responders`, `sampleSize`. Continuous: `study`, `treatment`, `mean`,
#'   `std.err` (and optionally `sampleSize`/`n`).
#' @param description Optional label.
#' @param ... Ignored (accepted for gemtc call-compatibility).
#' @return An object of class `mtc.network`.
#' @export
mtc.network <- function(data.ab, description = "Network", ...) {
  stopifnot(is.data.frame(data.ab))
  nm <- names(data.ab)
  is_binary <- all(c("responders", "sampleSize") %in% nm)
  is_cont   <- "mean" %in% nm
  if (!is_binary && !is_cont) {
    stop("springs::mtc.network: data.ab needs either (responders, sampleSize) ",
         "for binary or (mean, std.err) for continuous outcomes.", call. = FALSE)
  }
  treatments <- sort(unique(as.character(data.ab$treatment)))
  net <- list(
    data.ab = data.ab,
    description = description,
    # gemtc-shaped treatments table (id, description) for downstream consumers.
    treatments = data.frame(id = treatments, description = treatments,
                            stringsAsFactors = FALSE),
    outcome = if (is_binary) "binary" else "continuous"
  )
  class(net) <- "mtc.network"
  net
}

#' @export
print.mtc.network <- function(x, ...) {
  cat("springs mtc.network:", x$description, "\n")
  cat("  outcome:", x$outcome, "| treatments:", paste(x$treatments$id, collapse = ", "),
      "| studies:", length(unique(x$data.ab$study)), "\n")
  invisible(x)
}

#' Build a model object (gemtc `mtc.model` analogue)
#'
#' @param network An `mtc.network` from [mtc.network()].
#' @param type Only `"consistency"` is supported (the MetaInsight default).
#' @param linearModel `"random"` or `"fixed"`.
#' @param likelihood `"binom"` or `"normal"` (inferred from the network if NULL).
#' @param link `"logit"`, `"log"` or `"identity"`.
#' @param dic Logical, accepted for compatibility (DIC is always attempted).
#' @param hy.prior Heterogeneity prior. `"gemtc"` (default) reproduces gemtc's
#'   default `dunif(0, om.scale)` — τ ~ Uniform(0, om.scale) with om.scale the
#'   data-derived scale. Other specs the engine understands: `"flat"`,
#'   `"hn:SIGMA"` (half-normal on τ), `"hc:SIGMA"` (half-cauchy), `"unif:UPPER"`.
#' @param ... Ignored (e.g. gemtc's `n.chain`).
#' @return An object of class `mtc.model`; the mutable `$inits` slot exists so
#'   MetaInsight's seed-setting assignment is a no-op-compatible.
#' @export
mtc.model <- function(network, type = "consistency", linearModel = "random",
                      likelihood = NULL, link = NULL, dic = TRUE,
                      hy.prior = "gemtc", n.chain = 4, ...) {
  if (!inherits(network, "mtc.network")) {
    stop("springs::mtc.model: `network` must be an mtc.network.", call. = FALSE)
  }
  if (!identical(type, "consistency")) {
    stop("springs::mtc.model: only type='consistency' is supported.", call. = FALSE)
  }
  if (is.null(likelihood)) likelihood <- if (network$outcome == "binary") "binom" else "normal"
  if (is.null(link)) link <- if (network$outcome == "binary") "logit" else "identity"
  # Supported links: logit (OR), log (RR — explicit IRLS path), identity (MD).
  if (!link %in% c("logit", "log", "identity")) {
    stop("springs::mtc.model: link='", link, "' is not supported ",
         "(use 'logit' for OR, 'log' for RR, or 'identity' for MD).", call. = FALSE)
  }
  model <- list(
    network = network,
    type = type,
    linearModel = linearModel,
    likelihood = likelihood,
    link = link,
    dic = dic,
    hy.prior = hy.prior,
    n.chain = n.chain,
    inits = NULL                      # MetaInsight assigns into this; harmless
  )
  class(model) <- "mtc.model"
  model
}

# ---------------------------------------------------------------------------
# Run: shell out to the engine, collect posterior draws
# ---------------------------------------------------------------------------

# gemtc's om.scale = guess.scale(): the largest absolute within-study relative
# MLE effect on the link scale, maximised over studies. Replicated here in pure
# R (no gemtc dependency) — continuity correction k=0.5 for binomial arms, exactly
# as gemtc's constant/forced correction. Used as the upper bound of the default
# Uniform(0, om.scale) heterogeneity-SD prior.
.om_scale <- function(net) {
  d <- net$data.ab
  binary <- net$outcome == "binary"
  arm_mean <- if (binary) {
    r <- as.numeric(d$responders); n <- as.numeric(d$sampleSize)
    log((r + 0.5) / (n - r + 0.5))            # logit arm MLE, k = 0.5
  } else {
    as.numeric(d$mean)                        # identity arm MLE
  }
  max_eff <- 0
  for (s in unique(d$study)) {
    idx <- which(d$study == s)
    if (length(idx) < 2) next
    m <- arm_mean[idx]
    rng <- max(outer(m, m, function(a, b) abs(a - b)))   # max |pairwise diff|
    if (is.finite(rng) && rng > max_eff) max_eff <- rng
  }
  if (max_eff <= 0) 1 else max_eff
}

# Map gemtc-style arm data to the engine's arm-record JSON.
.engine_records <- function(net) {
  d <- net$data.ab
  sid <- as.integer(factor(d$study, levels = unique(d$study)))
  base <- data.frame(study = sid,
                     studyname = as.character(d$study),
                     treatment = as.character(d$treatment),
                     stringsAsFactors = FALSE)
  if (net$outcome == "binary") {
    base$events <- as.integer(d$responders)
    base$n      <- as.integer(d$sampleSize)
  } else {
    # Engine uses only the arm variance v = sd^2 / n. Accept the three common
    # continuous arm layouts and reduce each to (sd, n) with the same v:
    #   * sd + n          (native)                  -> v = sd^2/n
    #   * std.dev + sampleSize (gemtc parkinson)    -> v = std.dev^2/sampleSize
    #   * std.err (MetaInsight dataform)            -> v = std.err^2  (n := 1)
    base$mean <- as.numeric(d$mean)
    if (!is.null(d$sd) && !is.null(d$n)) {
      base$sd <- as.numeric(d$sd); base$n <- as.numeric(d$n)
    } else if (!is.null(d$std.dev) && !is.null(d$sampleSize)) {
      base$sd <- as.numeric(d$std.dev); base$n <- as.numeric(d$sampleSize)
    } else if (!is.null(d$std.err)) {
      base$sd <- as.numeric(d$std.err); base$n <- rep(1, nrow(d))  # sd^2/1 == std.err^2
    } else {
      stop("springs: continuous data needs (sd + n), (std.dev + sampleSize), ",
           "or std.err.", call. = FALSE)
    }
  }
  base
}

#' Fit the model (gemtc `mtc.run` analogue)
#'
#' Shells out to the spring/GC engine, which returns posterior draws of the
#' basic contrasts and the heterogeneity SD. No MCMC: the engine forms the
#' closed-form joint posterior and samples it.
#'
#' @param model An `mtc.model` from [mtc.model()].
#' @param n.adapt,n.iter,thin gemtc-compatible arguments. `n.iter` (minus burn)
#'   sets the number of posterior draws; `n.adapt` and `thin` are accepted but
#'   not needed by the closed-form engine.
#' @param seed Integer RNG seed for the draw sampler (reproducible).
#' @param ... Ignored.
#' @return An object of class `mtc.result`.
#' @export
mtc.run <- function(model, n.adapt = 5000, n.iter = 20000, thin = 1,
                    seed = 1, ...) {
  if (!inherits(model, "mtc.model")) {
    stop("springs::mtc.run: `model` must be an mtc.model.", call. = FALSE)
  }
  net <- model$network
  recs <- .engine_records(net)

  inp <- tempfile(fileext = ".json")
  out <- tempfile(fileext = ".json")
  on.exit(unlink(c(inp, out)), add = TRUE)
  jsonlite::write_json(recs, inp, dataframe = "rows", auto_unbox = TRUE)

  n_draws <- max(1000L, as.integer(n.iter / max(1L, as.integer(thin))))

  bin <- springs_engine_bin()
  if (identical(model$link, "log")) {
    # RR (log link): the explicit study-baseline + log-RR IRLS path.
    args <- c("--input", inp, "--output", out, "--link", "log",
              "--effects", if (identical(model$linearModel, "fixed")) "fixed" else "random",
              "--samples", as.character(n_draws), "--seed", as.character(as.integer(seed)),
              "--n-grid", "60")
  } else {
    # OR (logit) / MD (identity): resolve the heterogeneity prior to an engine spec.
    hp <- model$hy.prior %||% "gemtc"
    n_grid <- 100L
    if (identical(model$linearModel, "fixed")) {
      prior <- "hn:0.0001"                          # FE: τ ≈ 0
    } else if (hp %in% c("gemtc", "default")) {
      prior <- sprintf("unif:%.10g", .om_scale(net)) # gemtc dunif(0, om.scale)
      n_grid <- 200L
    } else {
      prior <- hp                                   # explicit flat|hn:|hc:|unif:
      if (startsWith(hp, "unif:")) n_grid <- 200L
    }
    args <- c("--input", inp, "--output", out,
              "--prior", prior, "--samples", as.character(n_draws),
              "--seed", as.character(as.integer(seed)), "--n-grid", as.character(n_grid))
  }
  status <- suppressWarnings(system2(bin, args, stdout = FALSE, stderr = FALSE))
  if (!identical(status, 0L) || !file.exists(out)) {
    stop("springs::mtc.run: engine failed (status ", status, ").", call. = FALSE)
  }

  rep <- jsonlite::fromJSON(out, simplifyVector = TRUE)
  draws <- rep$draws
  if (is.null(dim(draws))) draws <- matrix(draws, nrow = rep$n_samples)
  colnames(draws) <- rep$param_names

  # gemtc-compatible posterior matrix: basic params + sd.d (random only), as a
  # one-chain coda mcmc.list. The engine's draws are iid (closed-form posterior),
  # so a single "chain" with thin=1, no burn-in is the honest representation.
  S <- rep$n_samples
  samp_mat <- if (identical(model$linearModel, "fixed")) draws
              else cbind(draws, sd.d = as.numeric(rep$sd_d))
  nchain <- max(1L, as.integer(model$n.chain %||% 4L))
  samples <- .springs_mcmc_list(samp_mat, nchain)
  model$n.chain <- nchain
  # gemtc-style monitored-parameter list (basic contrasts + sd.d), used by the
  # MCMC-diagnostics path (`model$mtcResults$model$monitors$enabled`).
  model$monitors <- list(enabled = colnames(samp_mat))

  res <- list(
    treatments  = rep$treatments,
    ref         = rep$ref,                 # engine reference (lex-first)
    param_names = rep$param_names,
    draws       = draws,                   # S x (n_t-1): d.ref.t
    sd_d        = as.numeric(rep$sd_d),
    tau2        = rep$tau2,
    likelihood  = model$likelihood,
    link        = model$link,
    linearModel = model$linearModel,
    fixed       = identical(model$linearModel, "fixed"),
    outcome     = net$outcome,
    network     = net,
    n_samples   = rep$n_samples,
    fit_wall_sec = rep$fit_wall_sec,
    samples     = samples,                 # mcmc.list (gemtc-compatible)
    model       = model
  )
  # Specific class first so our S3 methods win dispatch even when gemtc (which
  # also defines summary.springs_mtc_result etc.) is attached; "mtc.result" kept so
  # inherits()-based consumer checks still pass.
  class(res) <- c("springs_mtc_result", "mtc.result")
  res
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' One-pass prior-sensitivity analysis (a GC superpower)
#'
#' The τ² partition-function grid is prior-independent, so a whole panel of
#' heterogeneity priors is evaluated from a SINGLE fit — each prior is just a
#' cheap reweighting. With MCMC every prior would be a separate full run.
#'
#' @param model An `mtc.model` from [mtc.model()].
#' @param priors Character vector of prior specs: `"gemtc"` (the default
#'   `dunif(0, om.scale)`), `"flat"`, `"hn:SIGMA"`, `"hc:SIGMA"`, `"unif:UPPER"`.
#' @param n.iter Posterior draws per prior (for the credible intervals).
#' @param seed RNG seed.
#' @return A data frame: one row per (prior × comparison) with `effect`/`sd`/
#'   `lo`/`hi`, plus a `tau2_mean` column — ready for a sensitivity table/forest.
#' @export
mtc.sensitivity <- function(model, priors = c("gemtc", "flat", "hn:0.5", "hn:1", "hc:0.5"),
                            n.iter = 4000, seed = 1) {
  if (!inherits(model, "mtc.model")) stop("springs::mtc.sensitivity: need an mtc.model.", call. = FALSE)
  net <- model$network
  recs <- .engine_records(net)
  inp <- tempfile(fileext = ".json"); out <- tempfile(fileext = ".json")
  on.exit(unlink(c(inp, out)), add = TRUE)
  jsonlite::write_json(recs, inp, dataframe = "rows", auto_unbox = TRUE)
  # resolve "gemtc"/"default" to the data-derived uniform prior
  om <- NULL
  spec <- vapply(priors, function(p) {
    if (p %in% c("gemtc", "default")) { if (is.null(om)) om <<- .om_scale(net); sprintf("unif:%.10g", om) }
    else p
  }, character(1))
  bin <- springs_engine_bin()
  st <- suppressWarnings(system2(bin, c("--input", inp, "--output", out,
        "--priors", paste(spec, collapse = ","),
        "--samples", as.character(max(1000L, as.integer(n.iter))),
        "--seed", as.character(as.integer(seed)), "--n-grid", "200"),
        stdout = FALSE, stderr = FALSE))
  if (!identical(st, 0L) || !file.exists(out)) stop("springs::mtc.sensitivity: engine failed.", call. = FALSE)
  rep <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  rows <- list()
  for (i in seq_along(rep$priors)) {
    pr <- rep$priors[[i]]
    for (e in pr$effects) {
      rows[[length(rows) + 1]] <- data.frame(
        prior = priors[i], comparison = paste0(rep$ref, ":", e$to),
        effect = e$mean, sd = e$sd, lo = e$lo, hi = e$hi,
        tau2_mean = pr$tau2$mean, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

# Build a coda-shaped mcmc.list from a draws matrix, without a hard coda
# dependency (we replicate its attribute/class layout). The engine's draws are
# iid (closed-form posterior); we partition them into `nchain` equal blocks so
# convergence diagnostics (Gelman-Rubin) and trace plots run. Because the blocks
# are iid from the same posterior, R-hat ≈ 1 by construction — correctly
# signalling "no convergence concern" rather than implying real MCMC chains.
.springs_mcmc_list <- function(mat, nchain = 4) {
  m <- as.matrix(mat)
  S <- nrow(m)
  nchain <- max(1L, min(as.integer(nchain), S))
  per <- S %/% nchain
  chains <- lapply(seq_len(nchain), function(k) {
    rows <- ((k - 1) * per + 1):(k * per)
    ch <- m[rows, , drop = FALSE]
    attr(ch, "mcpar") <- c(1, per, 1)    # start, end, thin
    class(ch) <- "mcmc"
    ch
  })
  class(chains) <- "mcmc.list"
  chains
}

#' SUCRA from a rank-probability table (gemtc `sucra` analogue)
#'
#' @param ranks A treatments x ranks matrix/data frame of rank probabilities
#'   (rows = treatments, summing to 1), as from [rank.probability()].
#' @param ... Ignored.
#' @return A named numeric vector of SUCRA values in `[0, 1]`, one per treatment.
#' @export
sucra <- function(ranks, ...) {
  m <- as.matrix(ranks)
  K <- ncol(m)
  if (K < 2) return(stats::setNames(rep(1, nrow(m)), rownames(m)))
  out <- apply(m, 1, function(p) sum(cumsum(p)[1:(K - 1)]) / (K - 1))
  stats::setNames(as.numeric(out), rownames(m))
}

# ---------------------------------------------------------------------------
# Posterior accessors
# ---------------------------------------------------------------------------

# Full per-draw effect matrix vs a chosen reference, columns = all treatments
# (the reference column is identically 0). Re-referencing is just a column
# subtraction because the engine ref cancels.
.effects_vs_ref <- function(result, ref = NULL) {
  if (is.null(ref)) ref <- result$ref
  trts <- result$treatments
  S <- nrow(result$draws)
  # Effects vs engine ref for every treatment (engine ref col = 0).
  E <- matrix(0.0, nrow = S, ncol = length(trts), dimnames = list(NULL, trts))
  for (j in seq_along(result$param_names)) {
    # param_names are d.<engref>.<t> in treatments[-1] order.
    t <- sub("^d\\.[^.]+\\.", "", result$param_names[j])
    E[, t] <- result$draws[, j]
  }
  if (!identical(ref, result$ref)) {
    if (!ref %in% trts) stop("springs: reference '", ref, "' not in network.", call. = FALSE)
    E <- E - E[, ref]
  }
  E
}

#' Re-reference the result (gemtc `relative.effect` analogue, t1 only)
#'
#' @param result An `mtc.result`.
#' @param t1 New reference treatment.
#' @param ... Ignored.
#' @return An `mtc.result` whose basic parameters are d.t1.t.
#' @export
relative.effect <- function(result, t1, ...) {
  if (!inherits(result, "mtc.result")) {
    stop("springs::relative.effect: need an mtc.result.", call. = FALSE)
  }
  E <- .effects_vs_ref(result, ref = t1)
  others <- setdiff(result$treatments, t1)
  draws <- E[, others, drop = FALSE]
  pn <- paste0("d.", t1, ".", others)
  colnames(draws) <- pn
  out <- result
  out$ref <- t1
  out$param_names <- pn
  out$draws <- draws
  out
}

#' All-pairwise league table (gemtc `relative.effect.table` analogue)
#'
#' @param result An `mtc.result`.
#' @param ... Ignored.
#' @return A `treatments x treatments x c("2.5%","50%","97.5%")` array;
#'   cell `[A, B, ]` is the effect of B relative to A (gemtc orientation).
#' @export
relative.effect.table <- function(result, ...) {
  trts <- result$treatments
  k <- length(trts)
  E <- .effects_vs_ref(result)            # S x k, common ref cancels in diffs
  qs <- c("2.5%", "50%", "97.5%")
  arr <- array(NA_real_, dim = c(k, k, 3), dimnames = list(trts, trts, qs))
  for (a in seq_len(k)) {
    for (b in seq_len(k)) {
      if (a == b) next
      d <- E[, b] - E[, a]                # effect of B relative to A
      arr[a, b, ] <- quantile(d, c(0.025, 0.5, 0.975), names = FALSE)
    }
  }
  class(arr) <- "mtc.relative.effect.table"
  arr
}

#' Summary (gemtc `summary.springs_mtc_result` analogue)
#'
#' @param object An `mtc.result`.
#' @param ... Ignored.
#' @return A list with `$summaries$statistics`, `$summaries$quantiles` (rows =
#'   basic params d.ref.t plus `sd.d`) and `$DIC`.
#' @export
summary.springs_mtc_result <- function(object, ...) {
  draws <- object$draws
  params <- colnames(draws)
  # Fixed-effect models have no heterogeneity parameter (as in gemtc): omit sd.d.
  if (isTRUE(object$fixed)) {
    mat <- draws
    rn <- params
  } else {
    mat <- cbind(draws, sd.d = object$sd_d)
    rn <- c(params, "sd.d")
  }

  stat_cols <- c("Mean", "SD", "Naive SE", "Time-series SE")
  statistics <- matrix(NA_real_, nrow = length(rn), ncol = length(stat_cols),
                       dimnames = list(rn, stat_cols))
  q_probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  q_cols <- c("2.5%", "25%", "50%", "75%", "97.5%")
  quantiles <- matrix(NA_real_, nrow = length(rn), ncol = length(q_cols),
                      dimnames = list(rn, q_cols))
  S <- nrow(mat)
  for (i in seq_along(rn)) {
    x <- mat[, i]
    statistics[i, "Mean"] <- mean(x)
    statistics[i, "SD"] <- sd(x)
    statistics[i, "Naive SE"] <- sd(x) / sqrt(S)
    statistics[i, "Time-series SE"] <- sd(x) / sqrt(S)  # iid draws
    quantiles[i, ] <- quantile(x, q_probs, names = FALSE)
  }

  out <- list(
    summaries = list(statistics = statistics, quantiles = quantiles,
                     start = 1, end = S, thin = 1,
                     nchain = as.integer(object$model$n.chain %||% 1L)),
    DIC = .springs_dic(object)
  )
  class(out) <- "summary.springs_mtc_result"
  out
}

# Best-effort DIC. The closed-form GC integrates the study baselines out, so a
# gemtc-identical residual-deviance DIC is not yet computed; report NA with the
# data-point count so downstream `as.data.frame(...$DIC)` still works.
.springs_dic <- function(object) {
  n_arms <- nrow(object$network$data.ab)
  c(Dbar = NA_real_, pD = NA_real_, DIC = NA_real_, `data points` = n_arms)
}

#' Rank probabilities (gemtc `rank.probability` analogue)
#'
#' @param result An `mtc.result`.
#' @param preferredDirection `1` if higher effect is better, `-1` if lower.
#' @param ... Ignored.
#' @return A `treatments x ranks` matrix of probabilities (rows sum to 1),
#'   class `mtc.rank.probability`.
#' @export
rank.probability <- function(result, preferredDirection = 1, ...) {
  trts <- result$treatments
  k <- length(trts)
  E <- .effects_vs_ref(result)                  # S x k
  S <- nrow(E)
  counts <- matrix(0.0, nrow = k, ncol = k, dimnames = list(trts, paste0(seq_len(k))))
  score <- E * preferredDirection
  for (s in seq_len(S)) {
    # rank 1 = best (largest preferred score). ties broken by order.
    ord <- order(score[s, ], decreasing = TRUE)
    for (r in seq_len(k)) counts[ord[r], r] <- counts[ord[r], r] + 1
  }
  probs <- counts / S
  class(probs) <- "mtc.rank.probability"
  probs
}

#' Forest plot of relative effects (gemtc `forest` analogue)
#'
#' Draws a blobbogram of every non-reference treatment versus the result's
#' reference, on the model's link scale (the caller exponentiates the axis for
#' OR/RR by passing a log `xlim`, as MetaInsight does). Base graphics, so it
#' composes with `title()`/`mtext()` exactly like `gemtc::forest`.
#'
#' @param x An `mtc.result` (typically from [relative.effect()]).
#' @param digits Annotation decimals.
#' @param xlim Optional x-axis limits (link scale).
#' @param ... Ignored.
#' @return `x`, invisibly (called for the plot).
#' @export
forest <- function(x, digits = 3, xlim = NULL, ...) {
  if (!inherits(x, "mtc.result")) {
    stop("springs::forest: need an mtc.result.", call. = FALSE)
  }
  E <- .effects_vs_ref(x)
  others <- setdiff(x$treatments, x$ref)
  q <- t(vapply(others, function(t)
    quantile(E[, t], c(0.025, 0.5, 0.975), names = FALSE), numeric(3)))
  lo <- q[, 1]; med <- q[, 2]; hi <- q[, 3]
  k <- length(others)
  if (is.null(xlim)) xlim <- range(c(lo, hi, 0), na.rm = TRUE)
  ypos <- rev(seq_len(k))
  op <- graphics::par(mar = c(4, 8, 2, 8)); on.exit(graphics::par(op))
  plot(NA, xlim = xlim, ylim = c(0.5, k + 0.5), yaxt = "n", xlab = "", ylab = "",
       bty = "n")
  graphics::abline(v = 0, lty = 2, col = "grey50")
  graphics::segments(pmax(lo, xlim[1]), ypos, pmin(hi, xlim[2]), ypos, lwd = 2)
  graphics::points(med, ypos, pch = 15, cex = 1.4)
  graphics::axis(2, at = ypos, labels = others, las = 1, tick = FALSE)
  graphics::axis(1)
  fmt <- paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)")
  graphics::mtext(sprintf(fmt, med, lo, hi), side = 4, at = ypos, las = 1,
                  line = 0.2, cex = 0.8)
  invisible(x)
}

#' @export
print.springs_mtc_result <- function(x, ...) {
  cat("springs mtc.result (", x$outcome, ", ", x$linearModel, ")\n", sep = "")
  cat("  ref:", x$ref, "| treatments:", paste(x$treatments, collapse = ", "), "\n")
  cat("  draws:", x$n_samples, "| sd.d mean:", round(mean(x$sd_d), 4),
      "| fit:", round(x$fit_wall_sec, 3), "s\n")
  invisible(x)
}
