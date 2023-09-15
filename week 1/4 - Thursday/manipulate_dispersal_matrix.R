
library(mcomsimr)
library(ggplot2)
library(dplyr)

# Number of patches
P = 20
# Number of species
S = 20

# set up landscape for experiment (want the same landscape for both treatments)
meta_landscape <- landscape_generate(patches = P)

# we can assume parches are separated into two groups due to a road. 
meta_landscape_frag <- meta_landscape
# define how patches are separated. change it in the way you want 
meta_landscape_frag$group <- ifelse(meta_landscape$x < 60 & meta_landscape$y > 20, "1", "2") 

# check how patches are separated
ggplot(data = meta_landscape_frag) +
  geom_point(aes(x, y, color = group))


# dispersal matrix
# set rate of distance decay
d = 0.05
disp_mat_intact <- dispersal_matrix(landscape = meta_landscape,
                             torus = FALSE,
                             kernel_exp = d,
                             plot = TRUE)

# now we will manipulate the dispersal matrix for the fragmented landscapes
# define whether a pair of patches are from different groups: 0-same; 1-different
group_diff <- as.matrix(dist(meta_landscape_frag$group))
# assume the pairs of patches in the same group have the same values of dispersal as that in the intact landscape, while those from different groups have no dispersal or low dispersal (0 -> a small value than 1 )
disp_mat_frag <- disp_mat_intact*(group_diff==0)*1 + disp_mat*(group_diff==1)*0

# check the dispersal structure for the the fragmented landscapes
g <- as.data.frame(disp_mat_frag) %>% dplyr::mutate(to.patch = rownames(disp_mat)) %>% 
  tidyr::gather(key = from.patch, value = dispersal, 
                -to.patch) %>% dplyr::mutate(from.patch = as.numeric(as.character(from.patch)), 
                                             to.patch = as.numeric(as.character(to.patch))) %>% 
  ggplot2::ggplot(ggplot2::aes(x = from.patch, y = to.patch, 
                               fill = dispersal)) + ggplot2::geom_tile() + scale_fill_viridis_c()
g

#### then disp_mat_intact and disp_mat_frag can be used to simulating metacommunities in intact and fragmented landscapes
## in the simulation, I recommend to use stabilized biotic interaction, rather than equal-interaction, because very few species can usually
## coexist in patches for metacommunities  with equal intraspecific and interspecific interactions



