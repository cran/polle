## ----lib, message = FALSE-----------------------------------------------------
library(polle)
library(data.table)

## -----------------------------------------------------------------------------
par0 <- list(a = 1, b = 0, c = 3)
sim_d <- function(n, par=par0, potential_outcomes = FALSE) {
  W <- runif(n = n, min = -1, max = 1)
  L <- runif(n = n, min = -1, max = 1)
  A <- rbinom(n = n, size = 1, prob = 0.5)
  U1 <- W + L + (par$c*W + par$a*L + par$b) # U^1
  U0 <- W + L # U^0
  U <- A * U1 + (1 - A) * U0 + rnorm(n = n)
  out <- data.table(W = W, L = L, A = A, U = U)
  if (potential_outcomes == TRUE) {
    out$U0 <- U0
    out$U1 <- U1
  }
  return(out)
}

## ----single stage data--------------------------------------------------------
set.seed(1)
d <- sim_d(n = 200)
pd <- policy_data(
    d,
    action = "A",
    covariates = list("W", "L"),
    utility = "U"
)

## -----------------------------------------------------------------------------
pl1 <- policy_learn(
  type = "blip",
  control = control_blip(blip_models = q_glm(~ W + L)),
  threshold = 1
)

## -----------------------------------------------------------------------------
po1 <- pl1(
  policy_data = pd,
  g_models = g_glm(~ 1),
  q_models = q_glm(~ A * (W + L))
)
pf1 <- get_policy(po1)
pa <- pf1(pd)

## ----pa_plot, echo = FALSE----------------------------------------------------
library("ggplot2")
plot_data <- data.table(d_N = factor(pa$d), W = d$W, L = d$L)
ggplot(plot_data) +
  geom_point(aes(x = W, y = L, color = d_N)) +
  geom_abline(slope = -3, intercept = 1) +
  theme_bw()

## -----------------------------------------------------------------------------
get_action_set(pd)[1] # reference action
pl1_ptl <- policy_learn(
    type = "ptl",
    control = control_ptl(policy_var = c("W", "L")),
    threshold = 1
)
po1_ptl <- pl1_ptl(
  policy_data = pd,
  g_models = g_glm(~ 1),
  q_models = q_glm(~ A * (W + L))
)
po1_ptl$ptl_objects

## ----pa_plot_ptl, echo = FALSE------------------------------------------------
pf1_ptl <- get_policy(po1_ptl)
pa_ptl <- pf1_ptl(pd)
library("ggplot2")
plot_data <- data.table(d_N = factor(pa_ptl$d), W = d$W, L = d$L)
ggplot(plot_data) +
  geom_point(aes(x = W, y = L, color = d_N)) +
  geom_abline(slope = -3, intercept = 1) +
  theme_bw()

## ----sate, cache = TRUE-------------------------------------------------------
set.seed(1)
approx <- sim_d(n = 1e7, potential_outcomes = TRUE)
(sate <- with(approx, mean((U1 - U0)[(U1 - U0 >= 1)])))
rm(approx)

## ----pe-----------------------------------------------------------------------
(pe <- policy_eval(
  policy_data = pd,
  policy_learn = pl1,
  target = "subgroup"
 ))

## -----------------------------------------------------------------------------
pl_set <- policy_learn(
  type = "blip",
  control = control_blip(blip_models = q_glm(~ W + L)),
  threshold = c(0, 1)
)

policy_eval(
  policy_data = pd,
  g_models = g_glm(~ 1),
  q_models = q_glm(~ A * (W + L)),
  policy_learn = pl_set,
  target = "subgroup"
)

## -----------------------------------------------------------------------------
IC(pe) |> head()

