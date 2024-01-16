
####### BCI Results ##########


##################################################################
##       R Practical for MLU Block Course: Modelling            ##
##      Species Distribution and Biodiversity Patterns          ##
##                                                              ##
##                            Week 2                            ##
##                    Tuesday, Jan. 16, 2024                    ##
##                    Measuring Biodiversity                    ##
##                    Dr. Kimberly Thompson                     ##
##                                                              ##
##################################################################

##     R code adapted from:                                     
##       https://jonlefcheck.net/2012/10/23/diversity-as-effective-numbers/
##       https://peat-clark.github.io/BIO381/veganTutorial.html





########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
rm(list = ls() )
gc() #releases memory


# install packages (only needs to be done once if you don't have these loaded
# already)
install.packages("vegan")
install.packages("ggplot2")
install.packages("reshape2")

# Load the packages
library(vegan)
library(ggplot2)
library(reshape2)


#################################
##                             ##
##       Data Loading          ##
##                             ##
#################################

# The vegan package has some convenient built-in datasets that we can use to
# to illustrate different biodiversity metrics

data(BCI) # Barro Colorado Island Tree Counts
str(BCI) # a data frame of observations of 225 species at 50 sites

# Note that the sites are the numbered rows with each species in a column 
# and the abundance of each species in each cell --> site x species matrix



#################################
##      SPECIES RICHNESS       ##
#################################

# We can calculate the species richness (S) for each site with the specnumber
# function

vegan :: specnumber(BCI)

# Here we also create a new dataframe to store these values
BCI.metrics <- data.frame(Site = seq(1:50),
                           S = vegan :: specnumber(BCI))


#################################
##         ABUNDANCE           ##
#################################

# The data itself provides the abundance of each species (n), but we can use
# the rowSums function to find the total abundance for each site.

rowSums(BCI)

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics, N = rowSums(BCI))



##########################################
##   Shannon-Wiener Diversity Index     ##
##########################################

# Also called Shannon entropy

# This index can be computed with the diversity function and by specifying 
# 'shannon'

vegan :: diversity(BCI, index = "shannon")

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics,
                           H = vegan :: diversity(BCI, index = "shannon"))


##########################################
##         Shannon's Evenness           ##
##########################################

# Also called Pielou's evenness or Pielou's J

# We know that the formula for Shannon's evennes is H / Hmax, where H
# max is equal to the natural log of the species richness.

# Using this information calculate Shannon's evenness for each site.

BCI.metrics$H / log(BCI.metrics$S)

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics,
                           J = BCI.metrics$H / log(BCI.metrics$S))


##########################################
##         Gini-Simpson Index           ##
##########################################

# This index can be computed with the diversity function and by specifying 
# 'simpson'


# Remember that the Simpson index is referred to with the letter 'D", and
# the Gini-Simpson index is 1-D. Since it is not good practice to start a 
# column name with a number, we will simply call this column GS.

# Note that althogh this index is referred to as "simpson" it is actually
# calculating the Gini-Simpson index. If you are so inclined, you can confirm
# this by typing ?diversity and reading the details section that comes up
# in the help tab.

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics,
                           GS = vegan :: diversity(BCI, index = "simpson"))



##########################################
##         Rarefied Richness            ##
##########################################

# We can standardize our observed values of species richness based on the 
# number of individuals sampled at each site.

# First we find the minimum number of individuals found in each plot using
# our total abundance calculation

raremin <- min(BCI.metrics$N)

# From looking at the data we can see that site 23 only has 340 individuals.

# Now, we rarefy (i.e., standardize) the richness values for the other sites
# as if they also only had 340 individuals.

vegan :: rarefy(BCI, raremin)

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics,
                           Sn = vegan :: rarefy(BCI, raremin))

# We can also create a rarefaction curve graph where each square shows a site
# number
# Note: to 'rarefy' a larger site, follow the rarefaction curve until the curve
# corresponds with the minimum observed number of species for a given site.

rarecurve(BCI, col = "blue")



##########################################
##      ENS for the BCI diversity      ##
##########################################

# Now that we have a better understanding of ENS, let's add both the Shannon and
# the Simpson effective number of species to our BCI dataframe.

# Shannon ENS
exp(BCI.metrics$H)

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics,
                           Hens = exp(BCI.metrics$H))

# Simpson ENS
1/(1 - BCI.metrics$GS)

# Add this data to the metrics dataframe
BCI.metrics <- data.frame(BCI.metrics,
                           GSens = 1/(1 - BCI.metrics$GS))




##########################################
##             Hill Numbers             ##
##########################################

# Like the conversion of Shannon diversity, effective numbers from Simpson 
# diversity yields a linear relationship with species richness

# Both Shannon and Simpson diversity are special cases entropy = the measure of 
# disorder in a system
# (more disorder = more diversity)
# There is a general equation for entropy from which Shannon and Simpson diversity
# are derived
# This equation has a parameter `q` that defines its sensitivity to rare species
# Low values of q favor rare species, high values of q favor abundant species
# When q = 0, diversity = S (richness), because rare species are treated 
# the same as abundant ones

# This is the basis of Hill numbers!

# The following function `divprof` calculates the diversity from this equation
# along a continuum of q values, creating what is called a 'diversity profile'

# Diversity profiles allow you to gauge the relative contribution of rare species
# to diversity your system
divprof = function(community) {
  cbind(
    seq(0, 5, by=0.099),
    unlist(lapply(seq(0, 5, by=0.099),
                  function(q) sum(apply(community, 1,
                                        function(x) (x/sum(x))^q))^(1/(1-q))))) }

# Let's apply function `divprof` to one community in our BCI data

# first, filter the community so that it does not have any 0 
# observations
# Step 1: Select the first row
first_row <- BCI[1, ]

# Step 2: Identify columns with non-zero values in the first row
non_zero_columns <- which(first_row != 0)

# Step 3: Subset the dataframe to keep only selected columns
community1 <- BCI[1, non_zero_columns]

# apply the formula
community1.divprof = divprof(community1)

# Plot the results
plot(community1.divprof[,1], community1.divprof[,2],
     ylim=c(0,max(BCI.metrics$S)),
     pch=16, cex=1, xlab="q value", ylab="Diversity")


# Where q=0, diversity is species richness, which we can highlight graphically
text(1, BCI.metrics$S[1], labels=c("Richness"))
points(community1.divprof[1,1], community1.divprof[1,2],
       col="red", pch=16, cex=1)


# Where q=1, diversity is Shannon diversity, we can find the exact value of
# the point at q=1
exp(diversity(community1, index="shannon"))

# Highlight this graphically
text(2.5, exp(diversity(community1,index="shannon"))+5,
     labels=c("Shannon Diversity"))
points(community1.divprof[11,1], community1.divprof[11,2],
       col="red", pch=16, cex=1)

# Where q=2, diversity is Simpson diversity, we can find the exact value of
# the point at q=2
1/(1-diversity(community1,index="simpson"))

# Highlight this graphically
text(2.5, 1/(1-diversity(community1,index="simpson"))-15,
     labels=c("Simpson Diversity"))
points(community1.divprof[21,1], community1.divprof[21,2],
       col="red", pch=16, cex=1)

# This plot shows that richness is high, but that as we increasingly favor 
# abundant species (increase q, move to the right along the x-axis), 
# diversity drops, indicating that community 1 is composed of some abundant and
# some rare species



##########################################
##          Whittaker's Beta            ##
##########################################

# Back to the BCI data!

# Whittaker's beta is simply gamma (the regional species richness) divided
# by alpha (the local species richness)

# We can find gamma by simply looking at the number of columns in our data
# since the columns represent species

ncol(BCI)

# Calculate Whittaker's beta for each site in the BCI data
ncol(BCI) / BCI.metrics$S

# Add this info to the BCI.metrics df
BCI.metrics <- data.frame(BCI.metrics,
                           Beta = ncol(BCI) / BCI.metrics$S)



##########################################
##      Bray-Curtis Dissimilarity       ##
##########################################

# Quantifies dissimilarity between pairs of sites

# Bray Curtis is a measure of structure, it takes into account who is at 
# each site and in what abundance


# For the BCI data we have 50 sites, so our matrix will be 50x50
braycurtis = vegan :: vegdist(BCI, method = "bray")

# Note there are many more metrics available with the vegdist function in 
# vegan. You can explore what others are available, by typing ?vegdist and
# looking through the possible entries in the methods argument

braycurtis <- as.matrix(braycurtis)



# We can create a heat map to understand which communities are more or less 
# dissimilar

# Create a long form dataframe that ggplot can use
braycurtis_df <- reshape2 :: melt(braycurtis)

# Filter out the upper triangle values (they're simply a duplicate of the 
# lower triangle)
braycurtis_df <- subset(braycurtis_df, Var1 >= Var2)

# Create a heatmap using ggplot2
ggplot(braycurtis_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Bray-Curtis Dissimilarity - BCI Data") +
  theme_bw()





#########################################################
##      BONUS: Non-metric multidimensional scaling     ##
#########################################################

# One of the ways to reduce the number of dimensions through ordination
# is called non-metric multidimensional scaling.

# In a community with 100 sites that means 100 dimensions when we start
# thinking about pairwise dissimilarities, which is impossible to visualize
# all at once.

# The goal of NMDS is to collapse information from multiple dimensions
# into just a few, so that they can be visualized and interpreted.

# NMDS does not produce any statistical output (though there are ways to 
# use the results statistically, but this is not covered here).

# The goal of NMDS is to represent the position of each site/community
# in multidimensional space as accurately as possible using a reduced number
# of dimensions that can easily be visualized.

# NMDS does not use the absolute abundances of species in communities, but 
# rather their rank orders and as a result is a flexible technique that accepts
# a variety of types of data.

# We can use our site x species BCI data to demonstrate.
# The k argument specifies how many dimensions we would like to reduce to.
example_NMDS <- vegan :: metaMDS(BCI, k = 2)

# Plot the results
plot(example_NMDS)

# it doesn't look all the informative yet. But let's see what we can do to fix
# that.

# Ordination plot function especially for congested plots
ordiplot(example_NMDS,type="n") 

# add text or points to ordination plots
# orditorp(example_NMDS, display="species", col="red", air=0.01) 
orditorp(example_NMDS, display="sites", cex=1.25, air=0.01)


# Let's imagine that communities 1-25 ad some treatment applied, and communities
# 26-50 a different treatments

# Using ordihull we can draw convex hulls connecting the vertices of the points
# made by these communities on the plot.

# This is an intuitive way to understand how communities and species cluster
# based on treatments.

treat=c(rep("Treatment1", 25), rep("Treatment2", 25))

ordiplot(example_NMDS, type = "n")

ordihull(example_NMDS, groups=treat, draw="polygon", col="grey90", label=F)

# orditorp(example_NMDS, display="species", col = "red", air = 0.01)

orditorp(example_NMDS, display="sites", col = c(rep("green", 25), rep("blue", 25)),
         air = 0.01,cex = 1.25)





