library("here")
library("dplyr")
library("tidyr")
library("epimixr")

ab <- readRDS(here::here("output", "ab.rds"))
latest_ab <- ab %>%
  filter(date == max(date)) %>%
  select(age, estimate) %>%
  ## kids half as susceptible as adults
  mutate(adj_estimate = if_else(age < 18,
                            estimate + (1 - estimate) / 2,
                            estimate)) %>%
  complete(age = seq(0, max(age))) %>%
  fill(estimate, adj_estimate, .direction = "up")

## values with polymod
mixing <- socialmixr::contact_matrix(survey = socialmixr::polymod,
                                     age.limits = unique(latest_ab$age))

## baseline estimate
adjust_immunity(mixing, latest_ab$estimate)
## <18 year olds half as likely to be infected when without antibodies
adjust_immunity(mixing, latest_ab$adj_estimate)

## end of vaccine rollout: what if all adults had antibodies of the oldest age group
immunity <- latest_ab$estimate
immunity[19:86] <- immunity[86]
adjust_immunity(mixing, immunity)

## kls = kids less susceptible
kls_immunity <- latest_ab$adj_estimate
kls_immunity[19:86] <- kls_immunity[86]
adjust_immunity(mixing, kls_immunity)

## how much more immunity do you need in <18 year olds to achieve 87.5% immunity,
## if all adults had antibodies of the oldest age group
kids_immunity <- function(x, baseline) {
  immunity <- baseline
  immunity[1:18] <- immunity[1:18] + x
  abs(adjust_immunity(mixing, immunity) - 0.875)
}

optim(par = list(x = 0.5),
      lower = list(x = 0),
      upper = list(x = 1),
      fn = kids_immunity,
      baseline = immunity,
      method = "Brent")
## -> 54%

optim(par = list(x = 0.5),
      lower = list(x = 0),
      upper = list(x = 1),
      fn = kids_immunity,
      baseline = kls_immunity,
      method = "Brent")
## -> 20%
