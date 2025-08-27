# Double Machine Learning Estimator

# 1. Load Libraries ------
set.seed(831213)
library(tidyverse)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)

# 2. Load & Inspect Data ------
data("lalonde", package = "MatchIt")
df <- lalonde

# 3. Prepare DoubleML Data ------
#    DoubleMLData$new(): data, y_col, d_cols, x_cols
dml_data <- DoubleMLData$new(
    data = df,
    y_col = "re78",
    d_cols = "treat",
    x_cols = c("age", "educ", "race", "re74", "re75")
)

# 4. Specify Machine Learners ------
#    lrn(): mlr3 learner; here random forest via ranger for regression
learner <- lrn("regr.ranger")

# 5. Initialize DML Model: Partial Linear Regression ------
dml_plr <- DoubleMLPLR$new(dml_data,
                           ml_l = learner,
                           ml_m = learner)

# 6. Tune Hyperparameters  ------
param_set <- ps(
    num.trees       = p_int(lower = 100L, upper = 1500L, init = 100),
    max.depth       = p_int(lower = 1L, upper = 5L, init = 3),
    sample.fraction = p_dbl(lower = 0.1, upper = 0.8, init = 0.3)
)

dml_plr$tune(
    param_set = list(
        ml_l  = param_set,
        ml_m  = param_set
    ),
    tune_settings = list(
        n_folds_tune = 5,
        rsmp_tune    = mlr3::rsmp("cv", folds = 5),
        terminator   = mlr3tuning::trm("evals", n_evals = 20),
        algorithm    = mlr3tuning::tnr("grid_search"), resolution = 5
    )
)

# 7. Fit Model ------
dml_plr$fit(store_models = TRUE)

# 8. Summary of Estimates ------
dml_plr$summary()


