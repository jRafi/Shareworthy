

library(lme4)
library(nlme)
library(boot)
library(lmeresampler)


set.seed(123)
x<-data.frame("id"=rep(1:100, each=5),
              "f1"=sample(0:1,250, replace=T),
              "f2"=sample(0:1,250, replace=T),
              "time"=rep(1:5,100),
              "y"=rpois(500,1)
              )

hist(x$y) # Display the distribution

# Model using nlme
m1<-lme(y ~ time*f1*f2, data=x, random= ~ time|id, na.action = na.omit)

# Model using lme4
m2<-lmer(y ~ time*f1*f2+(time|id), x, na.action=na.omit)


### Prepare the bootstrap

# Create function that is needed in the case_bootsrap function to extract statistics
myfn<-function(x=m2){fixef(x)}

# Bootstrap and store results in m2boot
m2boot<-case_bootstrap(model=m2, B=100, fn=myfn, resample = c(TRUE,FALSE))
summary(m2boot)


### Extract bootstrap parameters

# Extract CI:s
boot.ci(m2boot, index = 1,type=c("norm", "basic", "perc"))

# Plot a specific statistic
plot(m2boot, index = 1)

