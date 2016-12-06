#When you only have an estimate of the population R2 (safeguard)
##Calculate the confidence interval for R2
library(MBESS)
ci.R2(R2=.20, p=2, N=100,Random.Predictors=FALSE)

#use lower bound CI to represent f2
my.f2 <- 0.0676 / (1-0.0676)
print(my.f2)

library(pwr)
pwr.f2.test(u=2, f2=0.07250, power=.85)

#N = u+v+1
N=2+151+1
print(N)
#= 154



#Incremental predictions
##accounting for variance above and beyond - specific hypothesis DETERMINE F2
##sr2 / 1-R2
my.f2 <- .10 /(1-.20)
print(my.f2)
# = 0.125

#calculate power
library(pwr)
pwr.f2.test(u=1, f2=0.125, power=.85)
#N=u+v+1
N=1+72+1
print(N)
