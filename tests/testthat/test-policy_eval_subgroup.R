test_that("policy_eval with target = 'sub_effect' checks inputs.", {
    d <- sim_single_stage(1e2, seed = 1)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(1)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            target = "test"
        ),
        "target must be either 'value' or 'subgroup'."
    )

    d <- sim_single_stage_multi_actions(1e2, seed = 1)
    pd <- policy_data(d, action = "a", covariates = c("z"), utility = "u")
    p <- policy_def(1)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            g_models = g_empir(),
            target = "sub_effect"
        ),
        "subgroup average treatment effect evaluation is not implemented for more than two actions."
    )

    d <- sim_two_stage(1e2, seed = 1)
    pd <- policy_data(
        data = d,
        action = c("A_1", "A_2"),
        covariates = list(L = c("L_1", "L_2")),
        utility = "U_3"
    )
    p <- policy_def(1, reuse = TRUE)

    expect_error(
        policy_eval(
            policy_data = pd,
            policy = p,
            g_models = g_empir(),
            target = "sub_effect"
        ),
        "subgroup average treatment effect evaluation is not implemeted for multiple stages."
    )
})


test_that("policy_eval with target 'subgroup' agrees with targeted::cate.", {
    n <- 1e3
    Z <- rnorm(n = n)
    A <- rbinom(size = 1, n = n, prob = 0.5)
    U <- rnorm(mean = Z + Z * A, n = n)
    d <- data.table(Z = Z, A = A, U = U)
    rm(Z, A, U)
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(function(Z) (Z > 0) * 1)

    ##
    ## no cross-fitting:
    ##

    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "sub_effect",
        M = 1
    )

    ## implementation from the targeted package:
    d$d <- p(pd)$d
    library(targeted)

    ca <- cate(
        treatment = A ~ factor(d) - 1,
        response = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 1,
        type = "dml2"
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)["factor(d)1"])
    )

    expect_equal(
        IC(pe),
        ca$estimate$IC[, "factor(d)1"] |> unname() |> matrix()
    )

    ##
    ## cross-fitting: pooled estimate and variance
    ##

    set.seed(1)
    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "sub_effect",
        cross_fit_type = "pooled",
        variance_type = "pooled",
        M = 2
    )

    set.seed(1)
    ca <- cate(
        treatment = A ~ factor(d) - 1,
        response = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 2,
        type = "dml2"
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)["factor(d)1"])
    )

    expect_equal(
        IC(pe),
        ca$estimate$IC[, "factor(d)1"] |> unname() |> matrix()
    )

    ##
    ## cross-fitting: stacked estimate and pooled variance
    ##

    set.seed(1)
    pe <- policy_eval(
        policy_data = pd,
        policy = p,
        g_models = g_glm(~1),
        q_models = q_glm(~ A * Z),
        target = "sub_effect",
        cross_fit_type = "stacked",
        variance_type = "pooled",
        M = 2
    )

    set.seed(1)
    ca <- cate(
        treatment = A ~ factor(d) - 1,
        response = U ~ A * Z,
        propensity_model = A ~ 1,
        data = d,
        nfolds = 2,
        type = "dml1"
    )

    expect_equal(
        unname(coef(pe)),
        unname(coef(ca)["factor(d)1"])
    )

    ## in the targeted package, the stacked estimate is used
    ## to centralized the pooled variance estimate:
    ## expect_equal(
    ##     pe$IC,
    ##     ca$estimate$IC[, "factor(d)1"] |> unname()
    ## )
})

test_that("policy_eval with target 'sub_effect' has the correct outputs: test1.", {
    test_output <- function(pe) {
        ## value_estimate
        expect_true(
            !is.null(coef(pe)) && is.numeric(coef(pe))
        )

        ## IC should be zero for Z < 0
        expect_true(
            all(IC(pe)[d$Z <= 0] == 0)
        )

        ## id
        expect_true(
            all(pe$id == 1:1e2)
        )

        ## type
        expect_equal(
            pe$type,
            "dr"
        )

        ## target
        expect_equal(
            pe$target,
            "subgroup"
        )
    }

    d <- sim_single_stage(1e2, seed = 1)
    d$A <- c(rep(0, 50), rep(1, 50))
    pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
    p <- policy_def(function(Z) (Z > 0) * 1)

    ## no cross-fitting
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "subgroup"
        )
    )
    test_output(pe)

    ## cross-fitting: stacked estimator
    set.seed(1)
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "subgroup",
            M = 2,
            cross_fit_type = "stacked",
            variance_type = "stacked"
        )
    )
    test_output(pe)

    ## cross-fitting: pooled estimator
    set.seed(1)
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "sub_effect",
            M = 2,
            cross_fit_type = "pooled",
            variance_type = "pooled"
        )
    )
    test_output(pe)

    ## cross-fitting: pooled estimator, complete variance estimate
    expect_no_error(
        pe <- policy_eval(
            policy_data = pd,
            policy = p,
            target = "subgroup",
            M = 2,
            cross_fit_type = "pooled",
            variance_type = "complete"
        )
    )
    test_output(pe)
})

test_that("policy_eval with target 'subgroup' has the correct outputs: test2.", {
    z <- 1:1e2
    a <- c(rep(1, 50), rep(2, 50))
    y <- a * 2
    p <- c(rep(1, 25), rep(2, 25), rep(1, 25), rep(2, 25))
    d <- data.table(z = z, a = a, y = y, p = p)
    rm(a, z, y)
    pd <- policy_data(
        data = d,
        action = "a",
        covariates = c("z", "p"),
        utility = c("y")
    )

    ## his <- get_history(pd, stage = 1)
    ## qfun <- fit_Q_function(history = his, Q = d$y, q_degen(var = "z"))
    ## predict.Q_function(qfun, new_history = his)

    p <- policy_def(function(p) p)

    ref_Z <- cbind(
        (d$a == 1) / 0.5 * (d$y - d$z) + d$z,
        (d$a == 2) / 0.5 * (d$y - d$z) + d$z
    )
    ref_blip <- ref_Z[, 2] - ref_Z[, 1]
    ref_sub <- mean(ref_blip[d$p == 2])
    ref_IC <- 2 * (d$p == 2) * (ref_blip - ref_sub)

    ##
    ## no cross-fitting
    ##

    sub <- policy_eval(
        target = "subgroup",
        policy_data = pd,
        policy = p,
        q_models = q_degen(var = "z"),
        g_models = g_glm(~1)
    )

    expect_equal(
        coef(sub),
        ref_sub
    )

    expect_equal(
        IC(sub),
        matrix(ref_IC)
    )

    ##
    ## cross-fitting
    ##

    ## in each training, the empirical propensity is no longer 0.5
    ## instead a g_model is fitted on the complete data:
    gf <- fit_g_functions(pd, g_models = g_glm(~1))

    sub <- policy_eval(
        target = "sub_effect",
        policy_data = pd,
        policy = p,
        q_models = q_degen(var = "z"),
        g_functions = gf,
        M = 2,
    )

    expect_equal(
        coef(sub),
        ref_sub
    )

    expect_equal(
        IC(sub),
        matrix(ref_IC)
    )
})

## test_that("asymptotics of the subgroup effect estimate", {
##     onerun <- function() {
##         n <- 1e3
##         Z <- rnorm(n = n)
##         A <- rbinom(size = 1, n = n, prob = 0.5)
##         U <- rnorm(mean = Z + Z * A, n = n)
##         d <- data.table(Z = Z, A = A, U = U)
##         pd <- policy_data(d, action = "A", covariates = c("Z"), utility = "U")
##         p <- policy_def(function(Z) (Z > 0) * 1)

##         pe <- policy_eval(
##             policy_data = pd,
##             policy = p,
##             g_models = g_glm(~1),
##             q_models = q_glm(~ A * Z),
##             target = "subeffect"
##         )

##         c(coef(pe), sqrt(vcov(pe)))
##     }

##     ## true value of E[U^{(1)} - U^{(0)}| Z > 0]
##     target <- dnorm(0) / (0.5)

##     res <- lava::sim(onerun, R = 2e3, seed = 1)

##     summary(res, estimate = c(1), se = c(2), true = target)
## })

test_that("policy_eval with target 'subgroup' returns NA no subjects are in the subgroup.", {

  z <- 1:1e2
    a <- c(rep(1, 50), rep(2, 50))
    y <- a * 2
        p1 <- c(rep(1, 50), rep(2, 50))
    p2 <- c(rep(1, 100))

    d <- data.table(z = z, a = a, y = y, p1 = p1, p2 = p2)
    rm(a, z, y)
    pd <- policy_data(
        data = d,
        action = "a",
        covariates = c("z", "p1", "p2"),
        utility = c("y")
    )

    ref_Z <- cbind(
        (d$a == 1) / 0.5 * (d$y - d$z) + d$z,
        (d$a == 2) / 0.5 * (d$y - d$z) + d$z
    )
    ref_blip <- ref_Z[, 2] - ref_Z[, 1]
    ref_sub_eta50 <- mean(ref_blip[d$p1 == 2])
    ref_IC_eta50 <- 2 * (d$p1 == 2) * (ref_blip - ref_sub_eta50)

    p <- policy_def(1)

    sub <- policy_eval(
        target = "subgroup",
        policy_data = pd,
        policy = p,
        q_models = q_degen(var = "z"),
        g_models = g_glm(~1)
    )


    expect_equal(
        coef(sub),
        as.numeric(NA)
    )

    expect_equal(
        IC(sub),
        rep(as.numeric(NA), 1e2) |> matrix()
    )

    expect_no_error(
        tmp <- capture.output(print(sub))
    )

    pl <- policy_learn(
        type = "blip",
        threshold = c(50, 101),
        control = control_blip(blip_models = q_degen(var = "z"))
    )

    sub <- policy_eval(
        target = "subgroup",
        policy_data = pd,
        policy_learn = pl,
        q_models = q_degen(var = "z"),
        g_models = g_glm(~1)
    )

    expect_no_error(
      tmp <- capture.output(print(sub))
    )

    expect_equal(
        coef(sub),
        c(ref_sub_eta50, as.numeric(NA))
    )

    expect_equal(
        IC(sub),
        cbind(ref_IC_eta50, rep(as.numeric(NA), 1e2)) |> unname()
    )
})
