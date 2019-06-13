# accepts an array ‘feature’ and a maximal ‘degree’ and returns an data frame (e.g. SFrame) with the first column 
# equal to ‘feature’ and the remaining columns equal to ‘feature’ to increasing integer powers up to ‘degree’.
# feature as a vector, assume degree >=1
library("tidyverse")

polynomial_dtframe <- function(feature, degree){
  
  poly_dtframe <- tibble("power_1" = feature)
  for (i in 2:degree) {
    poly_dtframe <- poly_dtframe %>%  
      mutate(!!paste0("power_", i) := feature^i)
  }
  return(poly_dtframe)
}

polynomial = function(feature, power){
  return(data.frame(sapply(1:power, function(x){(feature)**x})))
}


#
set_1 = read.csv("week3/wk3_kc_house_set_1_data.csv",
                 sep = ",",
                 header = T)
set_2 = read.csv("week3/wk3_kc_house_set_2_data.csv",
                 sep = ",",
                 header = T)
set_3 = read.csv("week3/wk3_kc_house_set_3_data.csv",
                 sep = ",",
                 header = T)
set_4 = read.csv("week3/wk3_kc_house_set_4_data.csv",
                 sep = ",",
                 header = T)

p1_dat = polynomial_dtframe(set_1$sqft_living, 15)
p2_dat = polynomial_dtframe(set_2$sqft_living, 15)
p3_dat = polynomial_dtframe(set_3$sqft_living, 15)
p4_dat = polynomial_dtframe(set_4$sqft_living, 15)

p1_dat$price = set_1$price
p2_dat$price = set_2$price
p3_dat$price = set_3$price
p4_dat$price = set_4$price

m1 = lm(price ~ ., data = p1_dat)
m2 = lm(price ~ ., data = p2_dat)
m3 = lm(price ~ ., data = p3_dat)
m4 = lm(price ~ ., data = p4_dat)

summary(m1); summary(m2); summary(m3); summary(m4)

set_1$group = 1; set_2$group = 2; set_3$group = 3; set_4$group = 4
set_1$pred = m1$fitted.values; set_2$pred = m2$fitted.values; set_3$pred = m3$fitted.values; set_4$pred = m4$fitted.values

all_dat = rbind(set_1, set_2, set_3, set_4)

# Plot all 4 models
ggplot(all_dat, aes(
  x = sqft_living,
  y = price,
  color = as.factor(group)
)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(x = sqft_living, y = pred)) +
  theme_bw() +
  facet_wrap( ~ group)


test_set = read.csv("week3/wk3_kc_house_test_data.csv",
                    sep = ",",
                    header = T)
train_set = read.csv("week3/wk3_kc_house_train_data.csv",
                     sep = ",",
                     header = T)
valid_set = read.csv("week3/wk3_kc_house_valid_data.csv",
                     sep = ",",
                     header = T)


# The lowest RSS (equals to 5.89182477809e+14 ) on validation set of degree = 6
for (i in 1:15) {
  # Train model using training data
  dat = polynomial(train_set$sqft_living, i)
  dat$price = train_set$price
  model = lm(price ~ ., data = dat)
  
  # Compute RSS using test/validation set
  valid_dat = polynomial(valid_set$sqft_living, i)
  rss = sum((valid_set$price - predict(model, newdata = valid_dat)) ^ 2)
  print(paste(c("Degree: ", i, " RSS: ", rss), collapse = ""))
}


# 1.25529337848e+14
dat = polynomial(train_set$sqft_living, 5)
dat$price = train_set$price
m5 = lm(price ~ ., data = dat)

rss = sum((test_set$price - predict(m5, newdata = polynomial(
  test_set$sqft_living, 5
))) ** 2)
rss
