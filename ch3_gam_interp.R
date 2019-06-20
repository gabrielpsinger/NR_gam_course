# Fri Jun 14 11:50:20 2019 ------------------------------
#deeper dive to interp and visualize gams

mod_hwy <- gam(hw.mpg ~ s(weight) + s(rpm) + 
                 s(price) + s(comp.ratio) +
                 s(width) + fuel + cylinders,
               data = mpg, method = "REML")

summary(mod_hwy)


# family refers to the distribution of errors (e.g. normal/gaussian)
# link refers to the transformation of predictions (e.g. identity aka no transformation)
# parametric coefs, means that models have a predetermined form (here refers to the linear terms)
# edf = 'Effective Degrees of Freedom', represents the complexity of the smooth
# edf = 1 ==> straight line, edf = 2 ==> quadratic curve edf >>>2 more wiggly curve


