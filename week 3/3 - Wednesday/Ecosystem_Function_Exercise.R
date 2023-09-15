

library(tidyverse)
library(ggplot2)
devtools::install_github("ctkremer/priceTools")
library(priceTools)
library(patchwork)

# Paper: https://doi.org/10.1111/ele.13566
# Code for the paper: https://github.com/emma-ladouceur/SeedAdditionSynthesis
# Data from the paper (Download data here): https://figshare.com/articles/dataset/Reducing_dispersal_limitation_via_seed_addition_increases_species_richness_but_not_aboveground_biomass/12319682


# Download data from above links
# Set your working directory
setwd("~/Downloads/")
# species level data from 12 seed addition experiments
sp <- read.csv("SeedAdd_Sp_level.csv", header= TRUE)

View(sp)
head(sp)
colnames(sp)

# Rename Experiment short names to match the paper
 sp <- sp %>% mutate( Experiment = case_when(Experiment_ == "ASGA_Michigan" ~ "Michigan",
                                             Experiment_ == "California_Invade" ~ "California.1",
                                             Experiment_ == "California_Prop_Limi" ~ "California.2",
                                             Experiment_ == "CCR_04" ~ "Cedar.Creek.4",
                                             Experiment_ ==  "CCR_093" ~ "Cedar.Creek.93",
                                             Experiment_ == "Germany_Montane" ~ "Montane",
                                             Experiment_ == "Halle" ~ "Halle",
                                             Experiment_ ==  "Jena" ~ "Jena",
                                             Experiment_ == "Jena2" ~ "Jena.2",
                                             Experiment_ ==  "Kansas_KUFS_LTER_Hay_Meadow_Exp_2" ~ "Kansas.Hay.Meadow",
                                             Experiment_ == "Kansas_KUFS_LTER_Old_Field_Exp_1" ~ "Kansas.Old.Field",
                                             Experiment_ == "Texas_Temple_Prarie" ~ "Texas.Temple.Prairie")) %>%
   mutate( trt = case_when( trt == "Seed" ~ "Seeds",
                            TRUE ~ as.character(trt)))


# select only the plot-level data
plot <- sp %>% select(Experiment, site, block, plot, yr.trt, trt,
                      seed.rich, biomass.plot, biomass.measure, rich.plot) %>%
  distinct() %>% mutate(Treatment = trt) %>% select(-trt)


head(plot)


# let's have a look at the raw data

plota <- ggplot(data = plot, aes(x = Treatment, y = rich.plot)) +
  geom_point(position = position_jitter(0.2), alpha= 0.5, color = "#C0C0C0") +
  geom_boxplot(alpha = 0.2) +
  labs(y = "Plot Richness", subtitle = "a) Species Richness") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


plotb <- ggplot(data = plot, aes(x = Treatment, y = biomass.plot)) +
  geom_point(position = position_jitter(0.2), alpha= 0.5, color = "#C0C0C0") +
  geom_boxplot(alpha = 0.2) +
  labs(y =  expression(paste('Biomass (g/',m^2, ')')), subtitle= 'b) Biomass') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

(plota + plotb)


# fitting just a 'ggsmooth' line for each treatment, does realised richness predict aboveground biomass`?
plotc <- ggplot(data = plot, aes(x = rich.plot, y = biomass.plot)) +
  geom_point(aes(color = Experiment
                 ) , alpha = 0.5, size = 1.2, position = position_jitter(width = 0.95, height = 0.95) ) +
 geom_smooth( aes(linetype= Treatment), color = "black", method = lm, se=FALSE, fullrange=TRUE ) +
  labs(x = 'Realised Richness',
       y = expression(paste('Biomass (g/',m^2, ')')), title= "",
       subtitle="") + ylim(0,700)+
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF",
                                  "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" )) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

(plota + plotb) / (plotc)


head(plot)

# emulate the paper main results fitting a smooth line

sr_plot <- ggplot(data = plot, aes(x = seed.rich, y = rich.plot)) +
  geom_point(aes(color = Experiment
  ) , alpha = 0.5, size = 1.2, position = position_jitter(width = 0.95, height = 0.95) ) +
  geom_smooth( aes(linetype= Treatment), color = "black", method = lm, se=FALSE, fullrange=TRUE ) +
  labs(x = 'Seeded Richness',
       y = 'Realised Species Richness', title= "",
       subtitle="") +
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF",
                                              "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" )) +
                                                theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

bm_plot <- ggplot(data = plot, aes(x = seed.rich, y = log(biomass.plot))) +
  geom_point(aes(color = Experiment
  ) , alpha = 0.5, size = 1.2, position = position_jitter(width = 0.95, height = 0.95) ) +
  geom_smooth( aes(linetype= Treatment), color = "black", method = lm, se=FALSE, fullrange=TRUE ) +
  labs(x = 'Seeded Richness',
       y = expression(paste('Biomass (g/',m^2, ')')), title= "",
       subtitle="") +
  scale_colour_manual(values = c( "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#0C5BB0FF",
                                              "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#FA6B09FF","#A1C720FF","#9A703EFF" )) +
                                                theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

seed_legend <- g_legend(sr_plot)

(sr_plot + theme(legend.position="none") + bm_plot) / (seed_legend) + plot_layout(heights = c(10,2)) 

# This shows a positive relationship between seeded richness and biomass
# but the paper did not.
# why?


# This paper has biomass per species data. We can investigate per species dynamics using these data.
# The paper did not do this. What can we learn?
treat.vars <- c( "Experiment_", "trt", "block",  "plot", "seed.rich")
head(sp)

# Jena was one of the only experminets that showed a positive relationship between biomass and seed richness
# why?
grouped.seed.dat <- sp %>% select(Experiment_, yr.trt, trt, species, biomass.sp, seed.rich, site, block, plot) %>%
  filter(Experiment_ == "Jena") %>%
  group_by(.dots=c(treat.vars)) 

head(grouped.seed.dat)

# use the question mark to find out more about a function
# ?pairwise.price

# This takes 10 minutes to run
res.seed <- pairwise.price(grouped.seed.dat, species = "species", func = "biomass.sp")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp.s <- res.seed

pp.s <- group.columns(pp.s , gps=c(treat.vars), drop=T)

head(pp.s)

pp.s$block <- as.factor(pp.s$block)
levels(pp.s$block)
pp.s$seed.rich <- as.factor(as.character(pp.s$seed.rich))
levels(pp.s$seed.rich)

# prune to only a comparison we want
pp.wrangle <- pp.s %>%
   separate(seed.rich, c("seed.rich.x" , "seed.rich.y")) %>%
  filter( trt %in% c('Control Seeds'),
          seed.rich.x %in% c(0),
  block %in% c('87 87','88 88', '89 89', '90 90')
  ) %>%
  mutate(s.loss = (x.rich - c.rich), # calculate species loss
         s.gain = (y.rich - c.rich)) %>% # calculate species gain
  mutate(seed.rich.y = fct_relevel(seed.rich.y, c("1", "2", "4", "8", "16", "60")))

head(pp.wrangle)

# plot!

# CAFE style with gains and losses
s1 <- leap.zig(pp.wrangle,type='cafe',standardize = FALSE, raw.points = T)+
  annotate("text", x = mean(pp.wrangle$x.rich), y = mean(pp.wrangle$x.func),
           label = "*",size=8) + ggtitle('Control Seeds')+theme_classic()
s1


s2 <- leap.zig(pp.wrangle,type='bef',standardize = FALSE, raw.points = T)+
  annotate("text", x = mean(pp.wrangle$x.rich), y = mean(pp.wrangle$x.func),
           label = "*",size=8)+ggtitle('Control Seeds')+theme_classic()
s2


head(pp.wrangle)


sg_plot <- ggplot(data= pp.wrangle, aes(x = seed.rich.y, y = SG)) +
  geom_point(position = position_jitter(0.2), alpha= 0.5, color = "#C0C0C0") +
  geom_boxplot(aes(color = seed.rich.y), alpha = 0.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

sl_plot <- ggplot(data= pp.wrangle, aes(x = seed.rich.y, y = SL)) +
  geom_point(position = position_jitter(0.2), alpha= 0.5, color = "#C0C0C0") +
  geom_boxplot(aes(color = seed.rich.y), alpha = 0.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom",)

cde_plot <- ggplot(data= pp.wrangle, aes(x = seed.rich.y, y = CDE)) +
  geom_point(position = position_jitter(0.2), alpha= 0.5, color = "#C0C0C0") +
  geom_boxplot(aes(color = seed.rich.y), alpha = 0.2) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


(sg_plot / sl_plot / cde_plot)



head(pp.wrangle)
pp.wrangle_l <- pp.wrangle %>% mutate(seed.rich.y = as.numeric(as.character(seed.rich.y)))

head(pp.wrangle_l)

sg_line <- ggplot(data = pp.wrangle_l, aes(x = seed.rich.y, y = SG)) +
  geom_point(color = "#8F2F8BFF", alpha = 0.5, size = 1.2, position = position_jitter(width = 0.45, height = 0.45) ) +
  geom_smooth( color = "black", method = lm, se=FALSE, fullrange=TRUE ) +
  labs(x = 'Seeded Richness',
       y = expression(paste('Biomass gain (g/',m^2, ') associated with species gains (SG)')), title= "",
       subtitle="") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sg_line
sl_line <- ggplot(data = pp.wrangle_l, aes(x = seed.rich.y, y = SL)) +
  geom_point(color = "#8F2F8BFF", alpha = 0.5, size = 1.2, position = position_jitter(width = 0.45, height = 0.45) ) +
  geom_smooth( color = "black", method = lm, se=FALSE, fullrange=TRUE ) +
  labs(x = 'Seeded Richness',
       y = expression(paste('Biomass loss (g/',m^2, ') associated with species loss (SL)')), title= "",
       subtitle="") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         strip.background = element_rect(colour="black", fill="white"),legend.position="none")


cde_line <- ggplot(data = pp.wrangle_l, aes(x = seed.rich.y, y = CDE)) +
  geom_point(color = "#8F2F8BFF", alpha = 0.5, size = 1.2, position = position_jitter(width = 0.45, height = 0.45) ) +
  geom_smooth( color = "black", method = lm, se=FALSE, fullrange=TRUE ) +
  labs(x = 'Seeded Richness',
       y = expression(paste('Biomass change (g/',m^2, ') associated with context dependent change (CDE)')), title= "",
       subtitle="") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         strip.background = element_rect(colour="black", fill="white"),legend.position="none")

(sg_line + sl_line + cde_line)

# Concepts: https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12895

# Example Paper:  https://doi.org/10.1111/ele.14126
