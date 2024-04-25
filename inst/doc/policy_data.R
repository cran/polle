## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(polle)

## ----single stage data--------------------------------------------------------
(d <- sim_single_stage(n = 5e2, seed=1)) |> head()

## ----pdss---------------------------------------------------------------------
pd <- policy_data(d, action="A", covariates=list("Z", "B", "L"), utility="U")
pd

## ----gethistoryss-------------------------------------------------------------
get_history(pd)$H |> head()
get_history(pd)$A |> head()

## ----get----------------------------------------------------------------------
get_utility(pd) |> head()

## ----cleanup, include=FALSE---------------------------------------------------
rm(list = ls())

## ----simtwostage--------------------------------------------------------------
d <- sim_two_stage_multi_actions(n=2e3, seed = 1)
colnames(d)

## ----pdtwostage---------------------------------------------------------------
pd <- policy_data(d,
                  action = c("A_1", "A_2"),
                  baseline = c("B", "BB"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))
pd

## ----getactionsets------------------------------------------------------------
get_action_set(pd)
get_stage_action_sets(pd)

## ----gethistwostage-----------------------------------------------------------
get_history(pd, stage = 1, full_history = TRUE)$H |> head()
get_history(pd, stage = 2, full_history = TRUE)$H |> head()

## -----------------------------------------------------------------------------
get_history(pd, stage = 1, full_history = TRUE)$A |> head()
get_history(pd, stage = 2, full_history = TRUE)$A |> head()

## ----gethisstate--------------------------------------------------------------
get_history(pd, full_history = FALSE)$H |> head()
get_history(pd, full_history = FALSE)$A |> head()

## ----getutiltwo---------------------------------------------------------------
get_utility(pd) |> head()

## ----sim_data-----------------------------------------------------------------
d <- sim_multi_stage(2e3, seed = 1)

## ----view_data----------------------------------------------------------------
d$stage_data[, -(9:10)] |> head()

## ----view_b_data--------------------------------------------------------------
d$baseline_data |> head()

## ----pd-----------------------------------------------------------------------
pd <- policy_data(data = d$stage_data,
                  baseline_data = d$baseline_data,
                  type = "long",
                  id = "id",
                  stage = "stage",
                  event = "event",
                  action = "A",
                  utility = "U")
pd

## ----partial------------------------------------------------------------------
pd3 <- partial(pd, K = 3)
pd3

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

