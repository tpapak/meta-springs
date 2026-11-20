# Plaque psoriasis ML-NMR network, exactly as in the multinma vignette (shared by examples 05, 06).
source("examples/vs-multinma/common.R")
suppressPackageStartupMessages({ library(dplyr) })

# ---- data, exactly as in the multinma vignette --------------------------------
pso_ipd <- filter(plaque_psoriasis_ipd, studyc %in% c("UNCOVER-1", "UNCOVER-2", "UNCOVER-3")) %>%
  mutate(bsa = bsa / 100, prevsys = as.numeric(prevsys), psa = as.numeric(psa),
         weight = weight / 10, durnpso = durnpso / 10,
         trtclass = case_when(trtn == 1 ~ "Placebo", trtn %in% c(2, 3, 5, 6) ~ "IL blocker",
                              trtn == 4 ~ "TNFa blocker"),
         complete = complete.cases(durnpso, prevsys, bsa, weight, psa)) %>%
  filter(complete)
pso_agd <- filter(plaque_psoriasis_agd, studyc == "FIXTURE") %>%
  mutate(bsa_mean = bsa_mean / 100, bsa_sd = bsa_sd / 100, prevsys = prevsys / 100,
         psa = psa / 100, weight_mean = weight_mean / 10, weight_sd = weight_sd / 10,
         durnpso_mean = durnpso_mean / 10, durnpso_sd = durnpso_sd / 10,
         trtclass = case_when(trtn == 1 ~ "Placebo", trtn %in% c(2, 3, 5, 6) ~ "IL blocker",
                              trtn == 4 ~ "TNFa blocker"))
pso_net <- combine_network(
  set_ipd(pso_ipd, study = studyc, trt = trtc, r = pasi75, trt_class = trtclass),
  set_agd_arm(pso_agd, study = studyc, trt = trtc, r = pasi75_r, n = pasi75_n,
              trt_class = trtclass))
pso_net <- suppressMessages(add_integration(pso_net,
  durnpso = distr(qgamma, mean = durnpso_mean, sd = durnpso_sd),
  prevsys = distr(qbern, prob = prevsys),
  bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight = distr(qgamma, mean = weight_mean, sd = weight_sd),
  psa = distr(qbern, prob = psa), n_int = 64))
reg <- ~(durnpso + prevsys + bsa + weight + psa) * .trt
