
library(boot)
library(lmeresampler)

# Generate some poisson-distributed hierarchical data, along with two factors and a time variable.
set.seed(123)
x<-data.frame("id"=rep(1:100, each=5),
              "f1"=sample(0:1,250, replace=T),
              "f2"=sample(0:1,250, replace=T),
              "time"=rep(1:5,100),
              "y"=rpois(500,1)
)


# Using nlme::lme
library(nlme)
myModel<-lme(y ~ time*f1*f2, data=x, random= ~ time|id, na.action = na.omit)

# Using lme4::lmer
library(lme4)
myModel<-lmer(y ~ time*f1*f2+(time|id), x, na.action=na.omit)


#====================
### Run the bootstrap
#====================

#Using  lmeresampler::case_bootstrap
# Error: this only seem to work with lmer, not lme.
myBootstrapData<-case_bootstrap(model=myModel,
                                fn=function(x=myModel){fixef(x)},
                                B=100,
                                resample = c(TRUE,FALSE)
)

# Using lmeresampler::bootstrap
myBootstrapData<-bootstrap(model=myModel,
                           fn=function(x=myModel){fixef(x)},
                           type="case",
                           B=100,
                           resample=c(TRUE, FALSE)
)

#============================================
### Extract statistics from bootstrapped data
#============================================

# Extract CI:s
boot.ci(myBootstrapData, index = 1,type=c("norm", "basic", "perc"))

# Plot a specific statistic
plot(myBootstrapData, index = 1)
