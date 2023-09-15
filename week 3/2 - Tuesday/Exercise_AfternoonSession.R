######################################################
# Estimations of Functional Trait Diversity II
# Trait-value-based approach sensu Blonder et al 
# Code: Paola Barajas: paola.barajas@idiv.de
######################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(alphahull)
library(hypervolume)
library(BAT)
library(tidyverse)

# Load native species Hawaiian forest and species by trait matrix

setwd("C:/_Teaching/Lecture_Jan_Msc/Class_2023/")

datt_trait <- read.csv("datt_trait.csv")
traits     <- read.csv("Trait_imputed_104.csv")

#_______________________________________________________________________________
# ** QUESTION I **: Do single traits differ along rainfall gradients?
# TO DO: Single trait analysis: dry and  wet areas across islands

# First explore the data set
colnames(datt_trait)
unique(datt_trait$prec_range)
datt_trait$prec_range <- gsub("superwet",    "wet",       datt_trait$prec_range)

tmp <-dplyr::filter(datt_trait, prec_range == "dry" | prec_range == "wet")

SLA <-ggplot(data= tmp, aes(x = SLA, group = prec_range, fill= prec_range)) + geom_density(adjust=1.5, alpha=.4)
WD  <-ggplot(data= tmp, aes(x = WD, group = prec_range, fill= prec_range)) + geom_density(adjust=1.5, alpha=.4)
H   <-ggplot(data= tmp, aes(x = H, group = prec_range, fill= prec_range)) + geom_density(adjust=1.5, alpha=.4)
N   <-ggplot(data= tmp, aes(x = N, group = prec_range, fill= prec_range)) + geom_density(adjust=1.5, alpha=.4)

# TASK I: Do single traits differ across islands, i.e., age?
# ...

# Hawai'i plot WD
hawaii_tmp <- dplyr::filter(tmp, geo_entity2 == "Hawai'i Island")
oahu_tmp <- dplyr::filter(tmp, geo_entity2 == "O'ahu Island" )

h = ggplot(data= hawaii_tmp, aes(x = WD, group = prec_range, fill= prec_range)) + geom_density(adjust=1.5, alpha=.4)
o = ggplot(data= oahu_tmp, aes(x = WD, group = prec_range, fill= prec_range)) + geom_density(adjust=1.5, alpha=.4)

ggpubr::ggarrange(h, o)

#_______________________________________________________________________________
# ** QUESTION II **: What are the species trait combination and 
# what is the ecological meaning of them
# TO DO: multivariate trait analysis

head(traits[2:5])

traits <- data.frame(species =  traits$Scientific_name, 
                    SLA      =  log10(traits$SLA),     # mm^2 / mg^1  (area/mass)
                    WD       =  log10(traits$WD + 10), # g / cm^3 (mass/vol)
                    H        =  log10(traits$H),       # m 
                    N        =  log10(traits$N),       # mg / g  (mass/mass)
                    Scientific_name = traits$Scientific_name)
row.names(traits) = traits$Scientific_name

PCA         <- prcomp(traits[2:5], scale. = TRUE)
biplot(PCA)
summary(PCA)

# Interesting website about PCA: https://builtin.com/data-science/step-step-explanation-principal-component-analysis

PCAvalues   <- data.frame(Scientific_name = traits$Scientific_name, PCA$x)    # Extract PC axes for plotting
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)   # Extract loadings of the variables

# Plot trait space of all species
all_species <- PCAvalues %>% ggplot(aes(PC1,  PC2), size = 1)+ # Plot PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, aes(fill = after_stat(level)),  
                  colour = "gray", bins = 10) +
  scale_fill_distiller(palette = "BuGn", direction = 1) +
  geom_jitter(alpha= 0.6,  size = 2  , colour = "turquoise4") +     #  Display the points, i.e., species 
  geom_text(data = PCAloadings, aes(x = PC1*4.7, y = PC2*4.7, label = Variables), size = 2.3) +
  geom_segment(data = PCAloadings, size = 0.2,    # Plots the loadings, i.e., traits 
               aes(x = 0, xend = PC1*4.2, y = 0, yend = PC2*4.2),
               arrow = arrow(length = unit(0.1, "cm")),colour = "black")  +
  ylim(-5, 5.5) + 
  theme_minimal()
all_species

# TASK II: Let's chat
#  1. Can you identify plant strategies at the archipelago level according to the species trait combinations mapped in the PCA?
#  2. Identify acquisitive (fast growing) and conservative (slow growing) plants in the trait space of Hawaiian trees

#_______________________________________________________________________________
# ** QUESTION III **: Do the trait space between dry and wet ecosystems differs?
# TO DO: Visualization of trait space of dry and wet species

datt_trait <- dplyr::left_join(datt_trait, PCAvalues, by =  "Scientific_name") 

dry <- dplyr::filter(datt_trait, prec_range == "dry") # Separating the data into dry and wet  
wet <- dplyr::filter(datt_trait, prec_range == "wet")

# Check how many species are in each precipitation range, i.e., dry and wet
length(unique(dry$species))  #30
length(unique(wet$species))  #60
length(intersect(dry$species, wet$species)) #19

colnames(dry)

# Summarize / aggregate trait values per species 
dry1 <- summarize(group_by(dry, Scientific_name,  prec_range),
                         PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))
wet1 <- summarize(group_by(wet, Scientific_name,  prec_range),
                         PC1=max(PC1), PC2=max(PC2), PC3=max(PC3))

# Plots trait space
dry_traitspace <- dry1  %>% ggplot(aes(PC1,  PC2))+ 
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)), colour = "gray", bins = 5) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  geom_jitter(alpha=0.5,  size = 2  , colour = "gold2") +                      #  Display the points
  ylim(-5, 5.5) + 
  theme_minimal()

dry_traitspace
ggExtra:: ggMarginal(dry_traitspace, type = "density", fill="transparent", size = 15) # add marginal distribution of the PCs

# TASK III: Plot the trait space for wet
wet_traitspace <- wet1  %>% ggplot(aes(PC1,  PC2))+ 
  stat_density_2d(geom = "polygon", contour = TRUE, 
                  aes(fill = after_stat(level)), colour = "gray", bins = 5) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  geom_jitter(alpha=0.5,  size = 2  , colour = "blue") +                      #  Display the points
  ylim(-5, 5.5) + 
  theme_minimal()

wet_traitspace
ggExtra:: ggMarginal(wet_traitspace, type = "density", fill="transparent", size = 15) # add marginal distribution of the PCs

# ...
ggpubr::ggarrange(all_species, wet_traitspace, dry_traitspace, ncol = 3)

#_______________________________________________________________________________
# ** QUESTION IV **:Do traits combinations of Hawaiian forest species overlap across the rainfall gradient?
# TO DO: compute trait spaces overlap using hypervolume

##  https://benjaminblonder.org/hypervolume_faq.html
set.seed(3)

overall_bandwidth <- estimate_bandwidth(PCAvalues[,2:4])

dry_hv <-hypervolume_gaussian(dry1[,3:5], name = "Dry volume",
                                   kde.bandwidth=overall_bandwidth, quantile.requested=0.95)

wet_hv <-hypervolume_gaussian(wet1[,3:5], name = "Wet volume",
                                 kde.bandwidth=overall_bandwidth, quantile.requested=0.95)

# calculating overlap statistics
HVs <- hypervolume_join (dry_hv, wet_hv)

HV_set <- hypervolume_set(dry_hv, wet_hv, num.points.max = NULL,
                          verbose = TRUE, check.memory = F, distance.factor = 1)

plot(HV_set, contour.lwd= 2, cex.random=0, cex.data=2)

hypervolume_overlap_statistics(HV_set)

#_______________________________________________________________________________
# ** QUESTION V **: are wet ecosystems of the Hawaiian forest functionally richer than dry ecosystems? 
# TO DO: compute functional diversity metrics using hypervolume
# that is, functional richness, evenness, dispersion (sensu Blonder et al approach)

# Dry
dry_i <- data.frame(status = "DRY",
                      rich = kernel.alpha(dry_hv),
                      eve = kernel.evenness(dry_hv),
                      div = kernel.dispersion(dry_hv))
# Wet
wet_i <- data.frame(status = "WET",
                    rich = kernel.alpha(wet_hv),
                    eve = kernel.evenness(wet_hv),
                    div = kernel.dispersion(wet_hv))

results_FD <- rbind(dry_i, wet_i)
results_FD

# TAKS 
# Let's think about what are the potential biases in our analysis?
# What other environmental variables are important? 
