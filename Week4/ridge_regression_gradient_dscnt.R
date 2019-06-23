# a function that use gradient descent to solve for ridge regression
# it do not penalize intercept, it stops with a maximum iteration, update weight before access stopping cirteria
# input: feature matrix (with constant 1 column), output vector, initial weight vector, numeric step size eta, 
#        numeric L2 penalty lambda, maximum number of iteration
# output:

make_regression_matrix <- function(feature_dtf){
  
  X <- as.matrix(feature_dtf)
  X <- cbind(rep(1, nrow(X)), X)
  return(X)
}

ridge_regression_gradient_descent <- function(feature_matrix, output, initial_weights, 
                                              step_size, l2_penalty, max_iterations=100){
  weights <- as.matrix(initial_weights)
  l2_penalty <- as.matrix(c(0, rep(l2_penalty, ncol(feature_matrix) - 1)))
  
  for (i in 1:max_iterations) {
    gradient <- -2 * t(feature_matrix) %*% (output - feature_matrix %*% weights) + 2 * l2_penalty * weights
    weights <- weights - step_size * gradient 
  }
  
  return(weights)
}

# ============ example 1 =========
dt <- read.csv("week4/kc_house_train_data.csv")
feature_matrix <- make_regression_matrix(dt$sqft_living)
head(feature_matrix)

# no regularization
simple_weights_0_penalty <- ridge_regression_gradient_descent(feature_matrix, dt$price, c(0, 0), 1e-12, 0, max_iterations =  1000)

# big regularization 1e11

simple_weights_high_penalty <- ridge_regression_gradient_descent(feature_matrix, dt$price, c(0, 0), 1e-12, 1e11, max_iterations =  1000)

# What are the RSS on the test data for each of the set of weights above (initial, no regularization, high regularization)?
dt_test <- read.csv("week4/kc_house_test_data.csv")
X_test <- make_regression_matrix(dt_test$sqft_living)

rss_1 <- sum((dt_test$price)^2)
rss_2 <- sum((dt_test$price - X_test %*% simple_weights_0_penalty)^2)
rss_3 <- sum((dt_test$price - X_test %*% simple_weights_high_penalty)^2)


# ============ example 2 =========

feature_matrix <- make_regression_matrix(dt[, c("sqft_living", "sqft_living15")])
multiple_weights_0_penalty <- ridge_regression_gradient_descent(feature_matrix, dt$price, c(0, 0, 0), 1e-12, 0, max_iterations =  1000)
multiple_weights_high_penalty <- ridge_regression_gradient_descent(feature_matrix, dt$price, c(0, 0, 0), 1e-12, 1e11, max_iterations =  1000)

X_test <- make_regression_matrix(dt_test[, c("sqft_living", "sqft_living15")])
rss_2_1 <- sum((dt_test$price)^2)
rss_2_2 <- sum((dt_test$price - X_test %*% multiple_weights_0_penalty)^2)
rss_2_3 <- sum((dt_test$price - X_test %*% multiple_weights_high_penalty)^2)

# error in predicting the first house in test set
error_0_penalty <- dt_test$price[1] - X_test[1, ] %*% multiple_weights_0_penalty

error_high_penalty <- dt_test$price[1] - X_test[1, ] %*% multiple_weights_high_penalty



