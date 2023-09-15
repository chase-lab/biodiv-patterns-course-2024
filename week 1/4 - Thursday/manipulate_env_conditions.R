
library(mcomsimr)
library(ggplot2)
library(dplyr)

# Number of patches
P = 20
# Number of species
S = 20

# set up landscape for experiment (want the same landscape for both treatments)
meta_landscape <- landscape_generate(patches = P)

# generate the time series of the environmental conditions for each patch (same for each treatment)
env_conditions <- env_generate(landscape = meta_landscape,
                               env1Scale = 500, # temporal autocorrelation in the environment (here the environment is temporally uncorrelated)
                               timesteps = 1000)
head(env_conditions)
summary(env_conditions)

# assume the environmental conditions are shifted to higher values (warming)  
env_conditions_warm <- env_conditions
# one choice: assuming all values of environmental conditions across time and space are increased by 0.1
env_conditions_warm$env1 <- env_conditions_warm$env1 + 0.1
head(env_conditions_warm)
summary(env_conditions_warm)
# another choice: assuming the shifts of environmental conditions follow a norm distribution with mean value of 0.1
env_conditions_warm$env1 <- env_conditions_warm$env1 + rnorm(nrow(env_conditions_warm), 0.1, 0.1)
head(env_conditions_warm)
summary(env_conditions_warm)


## then env_conditions and env_conditions_warm can be used to simulating metacommunities before/after environmental changes (warmings)
## in the simulation, I recommend to use stabilized biotic interaction, rather than equal-interaction, because very few species can usually
## coexist in patches for metacommunities  with equal intraspecific and interspecific interactions


# As Shane mentioned in the discussion, temperate and tropical species may show different response into climate warming
# we can simulate two species groups with overall lower and higher optimal niche
# tropical species tends to have higher thermal niche; while temperate species have lower thermal niche
# we can change species' optimal niche to simulate temperate and tropical species groups
# niche for temperate species
densInd_niche_temperate <- env_traits(species = S,
                            max_r = 5,                         # max growth rate
                            min_env = 0, max_env = 0.7,          # lower values of optimal niche
                            env_niche_breadth = 0.2,       # niche breadth
                            optima_spacing = 'even')           # spacing of z_i (optima)
# niche for tropical species
densInd_niche_tropical <- env_traits(species = S,
                                      max_r = 5,                         # max growth rate
                                      min_env = 0.3, max_env = 1,          # higher values of optimal niche
                                      env_niche_breadth = 0.2,       # niche breadth
                                      optima_spacing = 'even')           # spacing of z_i (optima)




