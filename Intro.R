# Fri Jun 14 08:30:49 2019 ------------------------------
# Working through Noam Ross' GAM tutorial
# https://noamross.github.io/gams-in-r-course/

library(mgcv)

mcycle <- MASS::mcycle

head(mcycle)
plot(mcycle)

# make a linear model of the data
lm_mod <- lm(accel~times, data = mcycle)

# make a linear model of the data
termplot(lm_mod, partial.resid = T, se = T) 

# now make a nonlinear model
gam_mod <- gam(accel ~ s(times), data = mcycle)

# plot the results
plot(gam_mod, residuals = T, pch = 1)


# extract the coeffs of the basis function
coef(gam_mod) # has 9 coefs that make up the basis function!!!

# the flexiblity is appealing, but also makes it easy to overfit the model.  Below is how we deal with overfitting
# lambda value balanaces over fitting vs underfitting
# usually the mgcv package chooses the value of lambda, but we can fix it using the 'sp = ' argument
# NR reccomends using the Restricted Max. Likelihood Method (REML) for selecting smoothing parms
# the 'k = ' argument limits the number of basis functions

# play with the k argument
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# plot the two models
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = T, pch = 1) 
plot(gam_mod_k20, residuals = T, pch = 1)


# now explore the smooth parms
# run model and extract lambda (sp)
gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp

gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

# now see how model complexity and smoothing work together
# Fit a GAM models to the mcycle data with accel as a smooth function of times with 50 basis 
# functions and a smoothing parameter of 0.0001
# Visualize the model

gam_mod_sk <- gam(accel ~ s(times, k = 50), data = mcycle, sp = 0.0001)
plot(gam_mod_sk, residuals = T, pch = 1)
