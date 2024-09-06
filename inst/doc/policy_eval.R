## ----lib----------------------------------------------------------------------
library(polle)

## ----simdata------------------------------------------------------------------
d <- sim_two_stage(n = 2e3, seed = 1)
pd <- policy_data(d,
                  action = c("A_1", "A_2"),
                  baseline = c("B", "BB"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))
pd

## ----poldef-------------------------------------------------------------------
p1 <- policy_def(policy_functions = '1', reuse = TRUE, name = "(A=1)")

## ----polevalp1----------------------------------------------------------------
(pe1 <- policy_eval(policy_data = pd,
                    policy = p1,
                    type = "dr"))

## ----coef---------------------------------------------------------------------
coef(pe1)
vcov(pe1)

## ----ic-----------------------------------------------------------------------
IC(pe1) |> head()

## ----estimate_merge-----------------------------------------------------------
p0 <- policy_def(policy_functions = 0, reuse = TRUE, name = "(A=0)")
pe0 <- policy_eval(policy_data = pd,
                   policy = p0,
                   type = "dr")

(est <- merge(pe0, pe1))
estimate(est, function(x) x[2]-x[1], labels="ATE-difference")
estimate(est, function(x) x[2]/x[1], labels="ATE-ratio")

## ----get_nuisance-------------------------------------------------------------
(gf <- get_g_functions(pe1))

## ----pe_pol-------------------------------------------------------------------
policy_eval(policy_data = pd,
            g_functions = gf,
            policy = p0,
            type = "dr")

## ----pred_gfun----------------------------------------------------------------
predict(gf, new_policy_data = pd) |> head(6)

## ----get_qfun-----------------------------------------------------------------
get_q_functions(pe1)

## ----gq_models, cache = TRUE--------------------------------------------------
pe1 <- policy_eval(pd,
            policy = p1,
            g_models = list(
              g_sl(formula = ~ BB + L_1, SL.library = c("SL.glm", "SL.ranger")),
              g_sl(formula = ~ BB + L_1 + C_2, SL.library = c("SL.glm", "SL.ranger"))
            ),
            g_full_history = TRUE,
            q_models = list(
              q_glm(formula = ~ A * (B + C_1)), # including action interactions
              q_glm(formula = ~ A * (B + C_1 + C_2)) # including action interactions
            ),
            q_full_history = TRUE)

## ----history_names------------------------------------------------------------
get_history_names(pd) # state/Markov history
get_history_names(pd, stage = 1) # full history
get_history_names(pd, stage = 2) # full history

## ----pe_pl--------------------------------------------------------------------
policy_eval(pd,
            policy_learn = policy_learn(type = "ql"))

## ----pe_manual_pol_eval-------------------------------------------------------
p_ql <- policy_learn(type = "ql")(pd, q_models = q_glm())
policy_eval(pd,
            policy = get_policy(p_ql))

## ----crossfits----------------------------------------------------------------
pe_cf <- policy_eval(pd,
                     policy_learn = policy_learn(type = "ql"),
                     M = 2)

## ----pe_crossfits-------------------------------------------------------------
pe_cf$folds$fold_1 |> head()
pe_cf$cross_fits$fold_1

## ----demo_future, eval = FALSE------------------------------------------------
#  library(future.apply)
#  plan("multisession") # local parallel procession
#  library("progressr") # progress bar
#  handlers(global = TRUE)
#  
#  policy_eval(pd,
#              policy_learn = policy_learn(type = "ql"),
#              q_models = q_rf(),
#              M = 20)
#  
#  plan("sequential") # resetting to sequential processing

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

