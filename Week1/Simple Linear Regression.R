simple_linear_regression <- function(input_feature, output){
  slope <- sum((input_feature - mean(input_feature)) * (output - mean(output)))/sum((input_feature - mean(input_feature))^2)
  intercept <- mean(output) - slope * mean(input_feature)
  return(list(intercept = intercept, slope = slope))
}



get_regression_predictions <- function(input_feature, intercept, slope){
  predicted_output <- slope * input_feature + intercept
  return(predicted_output)
}

get_residual_sum_of_squares <- function(input_feature, output, intercept, slope){
  RSS <- sum((output - (input_feature * slope + intercept))^2)
  return(RSS)
}


inverse_regression_predictions <- function(output, intercept, slope){
  estimated_input <- (output - intercept)/slope
  return(estimated_input)
}






# eg. House price get slope and intercept
library(data.table)
house_train <- fread("week1/kc_house_train_data.csv")
est <- simple_linear_regression(input_feature = house_train$sqft_living, output = house_train$price)

# What is the predicted price for a house with 2650 sqft?
get_regression_predictions(input_feature = house_train$sqft_living, intercept = est$intercept, slope = est$slope)
get_regression_predictions(input_feature = c(2650), intercept = est$intercept, slope = est$slope)
md <- lm(price ~ sqft_living, data = house_train)
summary(md)

# What is the RSS for the simple linear regression using squarefeet to predict prices on TRAINING data?
get_residual_sum_of_squares(input_feature =  house_train$sqft_living, 
                            output = house_train$price, 
                            intercept = est$intercept,
                            slope = est$slope)
# what is the estimated square-feet for a house costing $800,000
inverse_regression_predictions(output = 800000, intercept = est$intercept, slope = est$slope)

# use ‘bedrooms’ (a count of the number of bedrooms in the house) to estimate prices
est_bedroom <- simple_linear_regression(input_feature = house_train$bedrooms, output = house_train$price)


# Which model (square feet or bedrooms) has lowest RSS on TEST data? 
house_test <- fread("week1/kc_house_test_data.csv")
get_residual_sum_of_squares(input_feature = house_test$bedrooms, 
                            output = house_test$price, 
                            slope = est_bedroom$slope,
                            intercept = est_bedroom$intercept)

get_residual_sum_of_squares(input_feature = house_test$sqft_living, 
                            output = house_test$price, 
                            slope = est$slope,
                            intercept = est$intercept)





