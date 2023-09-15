library(sp)
install.packages('tidyverse')
library(tidyverse)
library(cowplot)

##---------------------------------------------------------------------------------------------------------------------------------------------------
# Process-based model I: geometric population growth
# simulate population growth and plot for three cases:
# 1) b > d
# 2) b == d
# 3) b < d

# initial population size
N0 = 100
# per-capita birth rate
b = 1.2
# per-capita mortality (e.g., annual plant)
d = 1
# initialise vector to store population size each year, 
# and assign initial population size as first value
N <- NULL
N[1] <- N0

# simulate population growth for 10 year
for(t in 1:9) {
  N[t+1] = N[t] + (b-d) * N[t]
}

# b > d results
N_growing <- N
rm(N)

# b == d results
b = 1; d = 1
N <- NULL
N[1] <- N0

# simulate population growth for 10 year
for(t in 1:9) {
  N[t+1] = N[t] + (b-d) * N[t]
}
N_static <-  N
rm(N)

# b < d results
b = 0.9; d = 1
N <- NULL
N[1] <- N0

# simulate population growth for 10 year
for(t in 1:9) {
  N[t+1] = N[t] + (b-d) * N[t]
}
N_shrinking <-  N
rm(N)

N_growing
N_static
N_shrinking

# plot results
plot(x = 1:length(N_growing), y = N_growing, type = 'l',
     ylim = c(0, 600),
     xlab = 'Time [years]',
     ylab = 'Population size or density')
lines(1:length(N_static), N_static, type = 'l')
lines(1:length(N_shrinking), N_shrinking, type = 'l')

# reformulate geometric growth model with one parameter
# r = b - d

r = 5
N <- c()
N[1] <- N0

for(i in 1:19){
  N[i+1] = N[i] + r * N[i]
}

plot(x = 1:length(N),
     y = N, type = 'l',
     xlab = 'Time [years]',
     ylab = 'Population size or density')

##---------------------------------------------------------------------------------------------------------------------------------------------------
# Stochasticity: extending models to include structured randomness.
# - Dennis et al 1995 Ecological Monographs; Hilborn & Mangel 1997 The Ecological Detective

# Demographic and environmental stochasticity are most commonly discussed forms of stochasticity in Ecology
# other types: catastrophes (e.g., Lande 1993 Am Nat); measurement or observation error (e.g., Denis et al 1991 Ecol Monographs;
# Knape & de Valpine 2011 Ecol Lett);  process error (e.g., Denis et al 1991 Ecol Monographs; Thibaut & Connolly 2020 Ecology)

# Demographic stochasticity describes realised variability in demographic processes such as births, deaths and migration due
# to their probabilistic nature (see also Clark 2009 TREE for argument that stochastic forces exist only in models; e.g., "...processes perceived
# as stochastic at one level of abstraction have explanations at another.").

# Environmental stochasticity describes variablity in extrinsic environmental conditions (e.g., due to variation in temperature,
# precipitation, wind, etc)

# Extend geometric population model to include demographic stochasticity

# We have to decide how to change our model to include stochasticity. For example, we could assume that the parameter values of both the birth
# and death processes in the geometric growth model as random draws from probability distributions.
# Here, we'll instead use the approach taken in the metacommunity model
# that we will study later today (see also Adler & Drake 2008 Am Nat, Shoemaker et al. 2020 Ecology).

# We assume N[t+1] ~ Pois(lambda = N[t] + (b-d)*N[t]),
# N[t+1], the population size next year, is a random draw from a
# Poisson distribution with mean or lambda (i.e., the one and only parameter of the Poisson distribution)
# equal to the expected population size under our model of geometric growth.

# visual inspection of poisson distribution
?rpois()

rpois(n = 1000,
      lambda = 10)
plot(density(rpois(n = 1000,
                   lambda = 10)))
par(mfrow=c(1,2))
hist(rpois(n = 1000,
                   lambda = 10))
hist(rpois(n = 1000,
           lambda = 100))

plot(density(rpois(n = 1000,
                   lambda = 10)))
lines(density(rpois(n = 1000,
                   lambda = 100)))

# set seed for reproducibility
set.seed(123)
# define per-capita birth rate
b = 1.2
# and per-capita mortality
d = 1

# initialise vector to store population size
N <- NULL
N[1] <- N0

# simulate population growth for 10 years
for(t in 1:9) {

  N[t+1] = rpois(n = 1,
                 lambda = N[t] + (b-d) * N[t])
}

plot(1:length(N_growing), N_growing, type = 'l',
     ylim = c(0, 600),
     xlab = 'Time [years]',
     ylab = 'Population size or density')
lines(1:length(N), N,
      col = 'grey')

# do multiple simulations to visualise variability more clearly
nsims = 20
nyears = 9
N_matrix = matrix(data = NA,
                  nrow = nsims,
                  ncol = nyears + 1,
                  byrow = TRUE)
# initial population
N_matrix[,1] = N0

for(sim in 1:nsims){
  # print counter to screen
  print(paste('simulation ', sim, 'of ', nsims))
  for(t in 1:nyears){
    N_matrix[sim, t+1] = rpois(n = 1,
                               lambda = N_matrix[sim, t] + (b-d) * N_matrix[sim, t])
  }
}

# plot
matplot(t(N_matrix), type = 'l')
lines(1:length(N_growing), N_growing, lwd = 3)

## Environmental stochasiticity

# To include environmental stochasticity in our model of geometric population growth, we include an extra
# term, specifically,
# N[t+1] = N[t] + (b - d)*N[t] + sigma[t] * N[t],


# sigma[t] is a time series of environmental variation (e.g., temperature), and follows
# sigma[t+1] = a*sigma[t] + c*phi[t],
# where c scales the magnitude of the environmental variation phi[t] ~ N(mean = 0, sd = 1).

# set c = (1 - a^2)^0.5, which means var(sigma) is equal for all values of a.
# a controls autocorrelation:
# 0 < a < 1 environmental fluctuations are positively correlated,
# -1 < a < 0 environmental fluctuations are negatively correlated,
# a = 0, environmental fluctuations are uncorrelated.

phi = rnorm(nyears, mean = 0, sd = 1)
a = 0 # can to control autocorrelation of environmental fluctuations
c = (1 - a^2)^0.5
# initial vector to store environmental fluctuations
sigma <- NULL
sigma[1] = 0
# loop to create environmental fluctuations
for(t in 1:nyears){
  sigma[t+1] = a * sigma[t] + c*phi[t]
}

# visual inspection of environmental variation
plot(1:(nyears+1), sigma,
     xlab = 'Time [years]',
     ylab = 'Environmental condition\n(deviation from mean)',
     type = 'b')
abline(c(0,0), lty = 2)

# create plot with positive, negative and no autocorrelation
uncorrelated_sigma = sigma;
rm(sigma)

# positive autocorrelation
a = 1
sigma <- NULL
sigma[1] = 0
# loop to create environmental fluctuations
for(t in 1:nyears){
  sigma[t+1] = a * sigma[t] + c*phi[t]
}
pos_corr_sigma = sigma
rm(sigma)

# negative autocorrelation
a = -0.8
sigma <- NULL
sigma[1] = 0
# loop to create environmental fluctuations
for(t in 1:nyears){
  sigma[t+1] = a * sigma[t] + c*phi[t]
}
neg_corr_sigma = sigma
rm(sigma)

# plot
plot(1:(nyears+1), uncorrelated_sigma,
     xlab = 'Time [years]',
     ylab = 'Environmental condition\n(deviation from mean)',
     type = 'b',
     ylim = c(min(c(uncorrelated_sigma, pos_corr_sigma, neg_corr_sigma)) - 0.1,
              max(c(uncorrelated_sigma, pos_corr_sigma, neg_corr_sigma)) + 0.1))
lines(1:(nyears+1), pos_corr_sigma, col = 2, lty = 2)
lines(1:(nyears+1), neg_corr_sigma, col = 3, lty = 3)
abline(c(0,0), lty = 2)   

# simulate geometric growth with environmental stochasticity
N_uncorrelated <- NULL
N_uncorrelated[1] <- N0

# first uncorrelated
for(t in 1:nyears){
  N_uncorrelated[t+1] = N_uncorrelated[t] + (b - d)*N_uncorrelated[t] +
    N_uncorrelated[t] * N_uncorrelated[t]
}

plot(x = 1:length(N_uncorrelated),
     y = N_uncorrelated, type = 'l',
     xlab = 'Time [years]',
     ylab = 'Population size or density')


# Exercises:
# 1) Extend model to include demographic and environmental stochasticity (see Shoemaker et al. 2020 Ecology).

# 2) How would you measure the impact of including stochasticity in a model of geometric growth using simulations?
# Hint: it can be done using linear (statistical) models; see e.g., Blowes & Connolly 2012 Ecological Applications.
# Use simulations and linear models to quantify the impact of including different combinations of demographic and environmental stochasticity.

# 3) Reformulate geometric population growth model to include stochastic variation in births and deaths as separate processes.


