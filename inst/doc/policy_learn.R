## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----lib, message = FALSE-----------------------------------------------------
library("data.table")
library("polle")

## ----simdata------------------------------------------------------------------
d <- sim_two_stage(n = 2e3, seed = 1)
pd <- policy_data(d,
                  action = c("A_1", "A_2"),
                  baseline = c("B", "BB"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))
pd

## ----plblip-------------------------------------------------------------------
pl_blip <- policy_learn(
  type = "blip",
  control = control_blip(
    blip_models = q_glm(formula = ~ BB + L + C)
  )
)

## ----plblipout----------------------------------------------------------------
pl_blip

## ----plblipapply--------------------------------------------------------------

(po_blip <- pl_blip(
  pd,
  g_models = list(g_glm(), g_glm()),
  q_models = list(q_glm(), q_glm())
 ))

## ----plcross------------------------------------------------------------------
pl_blip_cross <- policy_learn(
  type = "blip",
  control = control_blip(
    blip_models = q_glm(formula = ~ BB + L + C)
  ),
  L = 2,
  save_cross_fit_models = TRUE
)
po_blip_cross <- pl_blip_cross(
   pd,
   g_models = list(g_glm(), g_glm()),
   q_models = list(q_glm(), q_glm())
 )

## ----plcrossinspect-----------------------------------------------------------
po_blip_cross$g_functions_cf

## ----pl_alpha-----------------------------------------------------------------
pl_blip_alpha <- policy_learn(
  type = "blip",
  control = control_blip(
    blip_models = q_glm(formula = ~ BB + L + C)
  ),
  alpha = 0.05,
  L = 2
)
po_blip_alpha <- pl_blip_alpha(
   pd,
   g_models = list(g_glm(), g_glm()),
   q_models = list(q_glm(), q_glm())
 )

## ----viewalpha----------------------------------------------------------------
po_blip_alpha$alpha
po_blip_alpha$g_functions

## -----------------------------------------------------------------------------
pf_blip <- get_policy_functions(po_blip, stage = 2)

## -----------------------------------------------------------------------------
pf_blip(
  H = data.table(BB = c("group2", "group1"),
                 L = c(1, 0),
                 C = c(1, 2))
)

## ----inspectblip--------------------------------------------------------------
po_blip$blip_functions$stage_1$blip_model

## -----------------------------------------------------------------------------
get_policy(po_blip)(pd) |> head(4)

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

