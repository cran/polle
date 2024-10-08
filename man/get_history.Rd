% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policy_data_functions.R
\name{history}
\alias{history}
\alias{get_history}
\title{Get History Object}
\usage{
get_history(object, stage = NULL, full_history = FALSE)
}
\arguments{
\item{object}{Object of class \link{policy_data}.}

\item{stage}{Stage number. If NULL, the state/Markov-type history across
all stages is returned.}

\item{full_history}{Logical. If TRUE, the full history is returned
If FALSE, only the state/Markov-type history is returned.}
}
\value{
Object of class \link{history}. The object is a list
containing the following elements:
\item{\code{H}}{\link[data.table:data.table]{data.table::data.table} with keys id and stage and with variables
\{\eqn{B}, \eqn{X_k}\} (state history) or
\{\eqn{B}, \eqn{X_1}, \eqn{A_1}, ..., \eqn{X_k}\}
(full history), see details.}
\item{\code{A}}{\link[data.table:data.table]{data.table::data.table} with keys id and stage and variable \eqn{A_k}, see
details.}
\item{action_name}{Name of the action variable in \code{A}.}
\item{action_set}{Sorted character vector defining the action set.}
\item{U}{(If \code{stage} is not NULL) \link[data.table:data.table]{data.table::data.table} with keys id and stage
and with variables U_bar and U_Aa for every a in the actions set.
U_bar is the accumulated rewards up till and including the given
stage, i.e., \eqn{\sum_{j=1}^k U_j}. U_Aa is the deterministic
reward of action a.}
}
\description{
\code{get_history} summarizes the history and action at a given stage from a
\link{policy_data} object.
}
\details{
Each observation has the sequential form
\deqn{O= {B, U_1, X_1, A_1, ..., U_K, X_K, A_K, U_{K+1}},}
for a possibly stochastic number of stages K.
\itemize{
\item{} \eqn{B} is a vector of baseline covariates.
\item{} \eqn{U_k} is the reward at stage k (not influenced by the action \eqn{A_k}).
\item{} \eqn{X_k} is a vector of state covariates summarizing the state at stage k.
\item{} \eqn{A_k} is the categorical action at stage k.
}
}
\examples{
library("polle")
### Single stage:
d1 <- sim_single_stage(5e2, seed=1)
# constructing policy_data object:
pd1 <- policy_data(d1, action="A", covariates=list("Z", "B", "L"), utility="U")
pd1

# In the single stage case, set stage = NULL
h1 <- get_history(pd1)
head(h1$H)
head(h1$A)

### Two stages:
d2 <- sim_two_stage(5e2, seed=1)
# constructing policy_data object:
pd2 <- policy_data(d2,
                  action = c("A_1", "A_2"),
                  baseline = c("B"),
                  covariates = list(L = c("L_1", "L_2"),
                                    C = c("C_1", "C_2")),
                  utility = c("U_1", "U_2", "U_3"))
pd2
# getting the state/Markov-type history across all stages:
h2 <- get_history(pd2)
head(h2$H)
head(h2$A)

# getting the full history at stage 2:
h2 <- get_history(pd2, stage = 2, full_history = TRUE)
head(h2$H)
head(h2$A)
head(h2$U)

# getting the state/Markov-type history at stage 2:
h2 <- get_history(pd2, stage = 2, full_history = FALSE)
head(h2$H)
head(h2$A)

### Multiple stages
d3 <- sim_multi_stage(5e2, seed = 1)
# constructing policy_data object:
pd3 <- policy_data(data = d3$stage_data,
                   baseline_data = d3$baseline_data,
                   type = "long",
                   id = "id",
                   stage = "stage",
                   event = "event",
                   action = "A",
                   utility = "U")
pd3

# getting the full history at stage 2:
h3 <- get_history(pd3, stage = 2, full_history = TRUE)
head(h3$H)
# note that not all observations have two stages:
nrow(h3$H) # number of observations with two stages.
get_n(pd3) # number of observations in total.
}
