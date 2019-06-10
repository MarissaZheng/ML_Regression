# closed form function
# input feature names: vector, output: character or vector, the data set: data frame
regression_closed_form <- function(features, output, data){
  feature_matrix <- cbind(all_one = 1, data[, features])
  output_vector <- data[, output]
  
  weight <- solve(t(feature_matrix) %*% feature_matrix) %*% t(feature_matrix) %*% output_vector
  return(weight)
  
}


# gradient descent function
# input feature names: vector, output: character or vector, the data set: data frame, initial_weights: vector, step_size: numeric, tolerance: numeric
regression_gradient_descent <- function(features, output, data, initial_weights, step_size, tolerance){
  feature_matrix <- as.matrix(cbind(all_one = rep(1, nrow(data)), data[, features]))
  output_vector <- as.matrix(data[, output])
  converge <- FALSE
  
  weight <- as.matrix(initial_weights) 
  while (!converge) {
    gradient <- 2 * t(t(feature_matrix %*% weight - output_vector) %*% feature_matrix)
    weight <- weight - step_size * gradient

    if (sqrt(sum(gradient^2)) < tolerance) {
      converge = TRUE
    }

  }
  
  rownames(weight) <- c("intercept", features)
  return(weight)
  
}

# --------------- eg. use gradient descent
library(data.table)
library(tidyverse)
house_train <- fread("week2/kc_house_train_data.csv")
house_test <- fread("week2/kc_house_test_data.csv")
house_train <- as.data.frame(house_train)
house_test <- as.data.frame(house_test)

# What is the value of the weight for sqft_living -- the second element of ‘simple_weights’ (rounded to 1 decimal place)?
md <- regression_gradient_descent(features = c("sqft_living"), output = "price", house_train, c(-47000, 1), 7e-12, 2.5e7)

# What is the predicted price for the 1st house in the Test data set for model 1 (round to nearest dollar)?
as.matrix(cbind(rep(1, 1), house_test[1, c("sqft_living")])) %*% md
# RSS
rss1 <- sum((as.matrix(cbind(rep(1, nrow(house_test)), house_test[, c("sqft_living")])) %*% md - house_test[, "price"])^2)

# add one more feature sqft_living15
md2 <- regression_gradient_descent(features = c("sqft_living", "sqft_living15"), output = "price", house_train, c(-100000, 1, 1), 4e-12, 1e9)
# What is the predicted price for the 1st house in the TEST data set for model 2 (round to nearest dollar)?
as.matrix(cbind(rep(1, 1), house_test[1, c("sqft_living", "sqft_living15")])) %*% md2
# RSS
rss2 <- sum((as.matrix(cbind(rep(1, nrow(house_test)), house_test[, c("sqft_living", "sqft_living15")])) %*% md2 - house_test[, "price"])^2)
# -------------- eg using R function lm
library(data.table)
library(tidyverse)

house_train <- as.tibble(house_train)
house_train <- house_train %>% 
  mutate(bedrooms_squared = bedrooms^2, 
         bed_bath_rooms = bedrooms * bathrooms,
         log_sqft_living = log(sqft_living),
         lat_plus_long = lat + long)

house_test <- as.tibble(house_test)
house_test <- house_test %>% 
  mutate(bedrooms_squared = bedrooms^2, 
         bed_bath_rooms = bedrooms * bathrooms,
         log_sqft_living = log(sqft_living),
         lat_plus_long = lat + long)


# what are the mean (arithmetic average) values of your 4 new variables on TEST data? (round to 2 digits)
result <- house_test %>% summarise(m1 = mean(bedrooms_squared),
                         m2 = mean(bed_bath_rooms),
                         m3 = mean(log_sqft_living),
                         m4 = mean(lat_plus_long)) %>% 
  mutate(m1 = round(m1, 2),
         m2 = round(m2, 2),
         m3 = round(m3, 2),
         m4 = round(m4, 2))


# Model 1: ‘sqft_living’, ‘bedrooms’, ‘bathrooms’, ‘lat’, and ‘long’
md1 <- lm(price ~ sqft_living + bedrooms + bathrooms + lat + long, data = house_train)


# Model 2: ‘sqft_living’, ‘bedrooms’, ‘bathrooms’, ‘lat’,‘long’, and ‘bed_bath_rooms’
md2 <- lm(price ~ sqft_living + bedrooms + bathrooms + lat + long +　bed_bath_rooms, data = house_train)

# Model 3: ‘sqft_living’, ‘bedrooms’, ‘bathrooms’, ‘lat’,‘long’, ‘bed_bath_rooms’, ‘bedrooms_squared’, ‘log_sqft_living’, and ‘lat_plus_long’
md3 <- lm(price ~ sqft_living + bedrooms + bathrooms + lat + long +　bed_bath_rooms　+　bedrooms_squared + log_sqft_living　+　lat_plus_long, data = house_train)


#
md1_rss_test <- sum((predict(md1, newdata = house_test %>% select(sqft_living, bedrooms, bathrooms, lat, long)) - house_test$price)^2)

md2_rss_test <- sum((predict(md2, newdata = house_test %>% select(sqft_living, bedrooms, bathrooms, lat, long, bed_bath_rooms)) - house_test$price)^2)

md3_rss_test <- sum((predict(md3, newdata = house_test %>% select(sqft_living, bedrooms, bathrooms, lat, long, bed_bath_rooms, bedrooms_squared, log_sqft_living,　lat_plus_long)) - house_test$price)^2)

