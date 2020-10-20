library(tidyverse)
setwd("C:/Users/Sanaa Mironov/Documents/UMBC/CMSC491/CourseDataSets")

library(ggfortify)
ggdistribution(dnorm, seq(-10, 30, 0.01), mean = 10, sd = 5)


N = 10 
x_array = seq(0,N,1)
#Poisson Distribution

poisson_density = dpois(x_array, 5)
poisson_cumul = ppois(x_array, 5)
poisson_density  
poisson_cumul  


ggdistribution(dpois, seq(0, 10), lambda = 5)

binom_density = dbinom(x_array,N, .5)
binom_cumul = pbinom(x_array, N, .5)
binom_density  
binom_cumul  


