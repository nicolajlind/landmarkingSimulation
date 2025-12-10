#If `sully` package is not currently installed, then install from Github
#devtools::install_github("https://github.com/stojl/sully")

require("sully")
require("tidyverse")
require("collapse")
require("purrr")
require("tibble")
require("tidyr")

#Notice that the `sully`-package uses hazards (and not log-hazards)
mu01_other  <- function(t,ts,ys,idx) 10^(0.03*(t+60)-3)
mu01_cph    <- function(t,ts,ys,idx) 2 * mu01_other(t,ts,ys,idx)
mucph_other <- function(t,ts,ys,idx) 0.01
muother_cph <- function(t,ts,ys,idx) 0.02

transMatrix <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = list(
  NULL, mu01_cph, mucph_other, NULL,
  NULL, NULL, NULL, NULL,
  muother_cph, NULL, NULL, mu01_other,
  NULL, NULL, NULL, NULL
))

transRates <- sully::build_rates(transMatrix)
transProbs <- sully::build_probs(transMatrix) 
domRates   <- list(
  function(t,ts,ys,idx) mu01_cph(10,NA,NA,NA) + mucph_other(NA,NA,NA,NA),
  function(t,ts,ys,idx) 0,
  function(t,ts,ys,idx) mu01_other(10,NA,NA,NA) + muother_cph(NA,NA,NA,NA),
  function(t,ts,ys,idx) 0
)

n <- 5e4
df <- sully::rmpp(
  n = n,
  rates = transRates,
  probs = transProbs,
  drates = domRates,
  t0 = 0,
  tn = 10,
  y0 = rep(c(1,3), n/2)
) %>% as.data.frame()

#Full estimation data
occ_exp <- to_occ_exp_split(df)


#Stacked landmarking data using Q=20
delta = data.frame(delta = runif(n, 0, 10), path = 0:(n-1))
delta_data = truncate(delta, occ_exp)
for(i in 1:19){
  delta = data.frame(delta = runif(n, 0, 10), path = 0:(n-1))
  df_trunc <- truncate(delta, occ_exp)
  delta_data <- rbind(delta_data, df_trunc)
}