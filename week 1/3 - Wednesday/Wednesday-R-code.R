library(tidyverse)
library(cowplot)
library(mcomsimr)

devtools::install_github("plthompson/mcomsimr")


S = 10 # number of species
env_traits(species = S,
           max_r = 5,                         # max growth rate
           min_env = 0, max_env = 1,          # min and max of species environmental optima
           env_niche_breadth = runif(n = S, min = 0.01, max = 0.1),       # niche breadth
           optima_spacing = 'even')           # spacing of z_i (optima)


##--------simulate metacommumity dynamics with the Thompson model
# Number of patches
P = 20
# Number of species
S = 20

# set up landscape for experiment (want the same landscape for both treatments)
meta_landscape <- landscape_generate(patches = P, plot = FALSE)

# dispersal matrix
# set rate of distance decay
d = 0.1
disp_mat <- dispersal_matrix(landscape = meta_landscape,
                             torus = TRUE,
                             kernel_exp = d,
                             plot = FALSE)

# generate the time series of the environmental conditions for 
# each patch (same for each treatment)
# Project idea: How does temporal environmental autocorrelation 
# impact population, community or metacommunity dynamics / patterns?
env_conditions <- env_generate(landscape = meta_landscape,
                               env1Scale = 1, # temporal autocorrelation 
                                              # in the environment 
                                              # (a value of 1 means the environment is 
                                              # temporally uncorrelated)
                               timesteps = 1000)

# density independent component of model
densInd_niche <- env_traits(species = S,
                            max_r = 5,                         # max growth rate
                            min_env = 0, max_env = 1,          # min and max of species niche optima
                            env_niche_breadth = 0.2,       # niche breadth
                            optima_spacing = 'even')           # spacing of z_i (optima)

# species interaction matrix:
# Project idea: examine the impact of alternate competition scenarios
# e.g., create  matrices for other dynamics in the paper 
# (mixed, competitive dominance, destabilising competition)
equal_interaction_mat <- species_int_mat(species = S,
                                         intra = 1,
                                         min_inter = 1,
                                         max_inter = 1)

stabilising_interaction_mat <- species_int_mat(species = S,
                                               intra = 1,
                                               min_inter = 0,
                                               max_inter = 0.8)

# use simulateMC() function to simulate dynamics
sim_equal_comp <- simulate_MC(patches=P, species=S,
                              dispersal = 0.1,
                              landscape = meta_landscape,
                              disp_mat = disp_mat,
                              env.df = env_conditions,
                              max_r = densInd_niche$max_r,
                              env_niche_breadth = densInd_niche$env_niche_breadth,
                              env_optima = densInd_niche$optima,
                              int_mat = equal_interaction_mat,
                              initialization=100, burn_in=300, timesteps=600)
?simulate_MC

sim_stabil_comp <- simulate_MC(patches=P, species=S,
                               dispersal = 0.1,
                               landscape = meta_landscape,
                               disp_mat = disp_mat,
                               env.df = env_conditions,
                               max_r = densInd_niche$max_r,
                               env_niche_breadth = densInd_niche$env_niche_breadth,
                               env_optima = densInd_niche$optima,
                               int_mat = stabilising_interaction_mat,
                               initialization=100, burn_in=300, timesteps=600)

str(sim_equal_comp)
str(sim_equal_comp)

# extract data
sim_equalC_dat <- sim_equal_comp$dynamics.df %>%
  as_tibble() %>%   
  # reduce to last 100 time steps
  filter(time > 499 & time < 601)


sim_stabilC_dat <- sim_stabil_comp$dynamics.df %>%
  as_tibble() %>%
  # reduce to last 100 time steps
  filter(time > 499 & time < 601)

# visual inspection

# environmental conditions:
ggplot() +
  geom_line(data = sim_equalC_dat,
            aes(x = time, y = env, colour = env,
                group = patch)) +
  scale_colour_viridis_c()

# what is the average environmental condition in each patch
sim_equalC_dat %>% 
  group_by(patch) %>% 
  summarise(mean_env = mean(env)) %>% 
  ggplot() +
  geom_point(aes(x = patch, y = mean_env))

# patch level population dynamics: equal comp
ggplot() +
  facet_wrap(~patch) +
  geom_line(data = sim_equalC_dat,
            aes(x = time, y = N, colour = species,
                group = interaction(species, patch))) +
  scale_colour_viridis_c()

ggplot() +
  facet_wrap(~patch) +
  geom_line(data = sim_stabilC_dat,
            aes(x = time, y = N, colour = species,
                group = interaction(species, patch))) +
  scale_colour_viridis_c()

# calculate multiplicative diversity partition (species richness only for now) 
# patch scale (alpha or local diversity)
equalC_alpha_S <- sim_equalC_dat %>% 
  # first remove species with zero abundance
  filter(N > 0) %>% 
  group_by(patch, time) %>% 
  summarise(S = n_distinct(species)) %>% 
  ungroup()

# all patches combined (gamma or regional diversity)
equalC_gamma_S <- sim_equalC_dat %>% 
  # first remove species with zero abundance
  filter(N > 0) %>% 
  # need to accumulate species across patches
  group_by(time, species) %>% 
  summarise(total_N = sum(N)) %>% 
  group_by(time) %>% 
  summarise(S = n_distinct(species)) %>% 
  ungroup()

# beta diversity (==gamma/mean(alpha))
# first calculate mean alpha diversity at each time step
equalC_mean_alpha <- equalC_alpha_S %>% 
  group_by(time) %>% 
  summarise(alpha_bar = mean(S)) 
# combine gamma and mean alpha, and calculate beta-diversity
equalC_beta_S <- left_join(equalC_gamma_S %>% 
                             rename(gamma_S = S),
                           equalC_mean_alpha) %>% 
  mutate(beta_S = gamma_S / alpha_bar)

# total abundance at the patch scale (all species combined, 
# e.g., as proxy for ecosytem function or productivity)
equalC_alpha_N <- sim_equalC_dat %>% 
  group_by(patch, time) %>% 
  summarise(alpha_N = sum(N)) %>% 
  ungroup()

# repeat for stabilising competition
stabilC_alpha_S <- sim_stabilC_dat %>% 
  # first remove species with zero abundance
  filter(N > 0) %>% 
  group_by(patch, time) %>% 
  summarise(S = n_distinct(species)) %>% 
  ungroup()

# all patches combined (gamma or regional diversity)
stabilC_gamma_S <- sim_stabilC_dat %>% 
  # first remove species with zero abundance
  filter(N > 0) %>% 
  # need to accumulate species across patches
  group_by(patch, time, species) %>% 
  summarise(total_N = sum(N)) %>% 
  group_by(time) %>% 
  summarise(S = n_distinct(species)) %>% 
  ungroup()

# beta diversity (==gamma/mean(alpha))
# first calculate mean alpha diversity at each time step
stabilC_mean_alpha <- stabilC_alpha_S %>% 
  group_by(time) %>% 
  summarise(alpha_bar = mean(S)) 
# combine gamma and mean alpha, and calculate beta-diversity
stabilC_beta_S <- left_join(stabilC_gamma_S %>% 
                             rename(gamma_S = S),
                           stabilC_mean_alpha) %>% 
  mutate(beta_S = gamma_S / alpha_bar)

# total abundance at the patch scale (all species combined, 
# e.g., as proxy for ecosytem function or productivity)
stabilC_alpha_N <- sim_stabilC_dat %>% 
  group_by(patch, time) %>% 
  summarise(alpha_N = sum(N)) %>% 
  ungroup()



# combine and plot
# local richness
alpha_S <- bind_rows(equalC_alpha_S %>% 
                       mutate(competition = 'equal'),
                     stabilC_alpha_S %>% 
                       mutate(competition = 'stabilising'))

ggplot() +
  facet_wrap(~patch) + 
  geom_point(data = alpha_S,
            aes(x = time, y = S, colour = competition)) +
  stat_smooth(data = alpha_S,
              aes(x = time, y = S, colour = competition),
              method = 'lm', se = F)

# regional richness
gamma_S <- bind_rows(equalC_gamma_S %>% 
                       mutate(competition = 'equal'),
                     stabilC_gamma_S %>% 
                       mutate(competition = 'stabilising'))

ggplot() +
  geom_point(data = gamma_S,
             aes(x = time, y = S, colour = competition)) +
  stat_smooth(data = gamma_S,
              aes(x = time, y = S, colour = competition))

# spatial beta-diversity
beta_S <- bind_rows(equalC_beta_S %>% 
                     mutate(competition = 'equal'),
                   stabilC_beta_S %>% 
                     mutate(competition = 'stabilising'))

ggplot() +
  geom_point(data = beta_S,
             aes(x = time, y = beta_S, colour = competition)) +
  stat_smooth(data = beta_S,
              aes(x = time, y = beta_S, colour = competition))

# local total abundance (e.g., ecosystem function)
alpha_N <- bind_rows(equalC_alpha_N %>% 
                      mutate(competition = 'equal'),
                    stabilC_alpha_N %>% 
                      mutate(competition = 'stabilising'))

ggplot() +
  geom_point(data = alpha_N,
             aes(x = time, y = alpha_N, colour = competition)) +
  stat_smooth(data = alpha_N,
              aes(x = time, y = alpha_N, colour = competition))

# BEF (biodiversity ecosystem functioning)
left_join(alpha_S, 
          alpha_N) %>% 
  ggplot() +
  facet_wrap(~patch) +
  geom_point(aes(x = S, y = alpha_N, colour = competition)) +
  stat_smooth(aes(x = S, y = alpha_N))

# Exercises:
# Simulate dynamics according to the classic metacommunity paradigms (Fig 2 in paper) and plot
