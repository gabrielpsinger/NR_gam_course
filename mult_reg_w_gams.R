# Fri Jun 14 09:21:26 2019 -----------------------------
library(tidyverse)
library(mgcv)
library(gamair)
data("mpg", package="gamair")
# work with the mpg data set
# build model to predict mpg


# first, a simple one
# hwy fuel effic, as a smooth function of vehicle wt
model <- gam(hw.mpg ~ s(weight), data = mpg, method = "REML")
plot(model, residual = T, pch = 1)

# add additional variable, length
model2 <- gam(hw.mpg ~ s(weight) + s(length), data = mpg, method = "REML")
par(mfrow = c(1, 2))
plot(model2, residuals = T, pch = 1)

# not every term in a GAM has to be nonlinear, just don't wrap it in s()
# same model as above
model2 <- gam(hw.mpg ~ s(weight) + length, data = mpg, method = "REML")
plot(model2, residuals = T, pch = 1)

# can make the same model if you set the smoothing parm very high
model2b <- gam(hw.mpg ~ s(weight) + s(length, sp = 1000), data = mpg,
               method = "REML")
plot(model2b, residuals = T, pch = 1)


# linear terms are useful when there are categorical predictors
model3 <- gam(hw.mpg ~ s(weight) + fuel, data = mpg,
              method = "REML")
plot(model3, residuals = T, all.terms = T, shade = T, se = T, pch = 1, pages = 1)

# gam with different smooths for diff cat variables
model4 <- gam(hw.mpg ~ s(weight, by = fuel), data = mpg,
              method = "REML")
plot(model4, residuals = T, shade = T, pages = 1)  

# when you have a smooth factor interactio you propbably want to inlcude varying interecepts
model4b <- gam(hw.mpg ~ s(weight, by = fuel) + fuel, data = mpg,
               method = "REML")
plot(model4b, pages = 1, all.terms = T)


# Practice with multi gams ------------------------------------------------

# Examine the data
head(mpg)
str(mpg)


# Fit the model
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price),  
                data = mpg, method = "REML")

# Plot the model
plot(mod_city, pages = 1)


# now include cat. variables
# Fit a GAM to the mpg data, modeling city.mpg as a sum of smooth functions of weight, length, and price, 
# and also include the categorical terms fuel, drive, and style.

mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style, 
                 data = mpg, method = "REML")
plot(mod_city2, all.terms = T, pages = 1)

# now, add different smooths for different levels of cat var
# Fit a model to predict city fuel efficiency (city.mpg) with smooth terms of weight,
# length, and price, but make each of these smooth terms depend on the drive categorical 
# variable using by= in the smooth terms.
# Include a separate linear term for the drive variable.

mod_city3 <- gam(city.mpg ~ s(weight, by = drive) + s(length, by = drive) + s(price, by = drive) + drive, 
                 data = mpg, method = "REML")
plot(mod_city3, all.terms = T, pages = 1)
