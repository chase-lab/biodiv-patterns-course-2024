## Morning session

Class beginning time: 10:00

### Mobsim
Please, BEFORE THE CLASS, run the following R code to install packages in preparation of our exercise with Mobsim:
```
install.packages(c("shiny", "shinyBS", "pals", "shinyjs", "devtools", "DT", "markdown"))
devtools::install_github("mobiodiv/mobsim")
```
Please, DO NOT install `mobsim` through `install.packages("mobsim")`.

And make sure this works:
```
shiny::runGitHub("albansagouis/mobsim_app", ref = "master")
```
If the shiny app does not appear or if it crashes, please send me an email: alban.sagouis\\at/idiv.de


## Afternoon session

What can we learn from studying different biodiversity metrics? 

-	Richness (S or alpha): number of species, but will be heavily influences by rare species with an abundance of 1.  
-	Abundance (N): are there differences in number of individuals?
-	Rarefied richness: can the difference in richness simply be explained by differences in abundance? 
-	PIE: is there a difference in evenness between treatments? (note that Shannon is always somewhere in between alpha and PIE, and may therefore not be super interesting. 
-	Gamma diversity in comparison to alpha diversity: is the difference in richness scale dependent (is the interpretation for Gamma different than for Alpha?). Could this indicate homogenization of the community? 
-	Beta diversity: is treatment X more homogeneous than treatment Y? Is there a difference in species composition between treatments? 



## Group assignment 

Work in groups of 2-3. Together you will dive deeper into what you can learn by calculating different biodiversity metrics for real world data.

Presentations summarizing your work will be given via zoom on Friday afternoon to both your classmates and this week’s course instructors. Each group should plan to present their work for 15 minutes (please do not go over this time). Then there will be 5 minutes for questions. Roel, Kim, and Alban will be available to work one-on-one with groups at different times during the day on Thursday and on Friday morning. Details to be announced but probably 1.5 to 2 hours per group on Thursday and 45 minutes to 1 hour per group on Friday morning.

*Goals:*

- Calculate biodiversity metrics using real-world example data taken from the scientific literature.
- Interpret your results by clearly explaining what they tell us about ecological communities.

The papers below each provide biodiversity data (i.e., site x species matrix with abundances) necessary to calculate the metrics we have learned about this week.

### For 3-5 papers in this list:

1.  Read/scan the paper. What metrics have the authors already calculated? What were their conclusions about what these metrics indicate about the communities under investigation?
2.  Find the (raw) biodiversity data provided in or as a supplement to the paper. Note, you might need to wrangle the data in R so that it is in the format required for the different metric calculations.
3.	What metrics were not calculated by the authors? What can they tell us about the structure of the community? 
4.	For the metrics you identified in step 3, calculate them in R, using the tutorials and lab exercises from this week as a guide.
5.	Create a graph(s) of your results --- again using the tutorials and lab exercises from this week as a guide.
6.	Interpret your results. What new insights do your results provide for understanding these communities?

### Expectations for the presentation

For each paper: 

- 1 slide dedicated to what the authors already calculated and their interpretation. *Important advice:* Do not get bogged down with the details in each paper. This slide should provide a brief summary that can be explained in 2-3 sentences. For example, what species group did they study? What was the comparison that they made (some environmental gradient or different treatments?), and what was their sample size (i.e. number of sites)? What metrics (N, S, Shannon, etc) did they calculate? What did they conclude?
- 2-4 slides dedicated to explaining the additional question(s) you addressed, the methods you used (i.e., which metrics you calculated and how), your results (i.e., graphs), and your interpretations of the results. Is this different from what the authors concluded? 

### Papers

1.  Wang, N., Song, X., Wang, J., Wang, L. (2022). Impacts of different fencing periods and grazing intensities on insect diversity in the desert steppe in Inner Mongolia. Frontiers in Ecology and Evolution, 10. https://doi.org/10.3389/fevo.2022.1021677
Richness, abundance, Shannon diversity, and Simpson diversity of insects across fencing and grazing treatments. 
 
2. Lynch, K. E., Penk, M. R., Perrin, P. M., & Piggott, J. J. (2022). Cattle Grazing of a Celtic Sea Saltmarsh Affects Invertebrate Community Composition and Biomass, but not Diversity. Wetlands, 42(8), 125. https://doi.org/10.1007/s13157-022-01643-6
  Richness, total abundance, community composition, and Pielou's evenness of insects across grazing treatments. 
   
3. Assandri, G., Bogliani, G., Pedrini, P., & Brambilla, M. (2019). Toward the next Common Agricultural Policy reform: Determinants of avian communities in hay meadows reveal current policy's inadequacy for biodiversity conservation in grassland ecosystems. Journal of Applied Ecology, 56(3), 604-617. https://doi.org/10.1111/1365-2664.13332
  Richness, community composition, and number of specialist species of birds across different grassland types. 

4. Leung, T. K. C., So, K. Y. K., Shum, B. T. W., & Hau, B. C. H. (2022). Optimal Mowing Regime in Enhancing Biodiversity in Seasonal Floodplains along Engineered Channels. Sustainability, 14(7), 4002. https://doi.org/10.3390/su14074002
 Richness and abundance for birds, amphibians, reptiles, butterflies, Odonates, macroinvertebrates, and plants across mowing treatments. 

5. Senior, K. L., Giljohann, K. M., McCarthy, M. A., & Kelly, L. T. (2023). A field test of mechanisms underpinning animal diversity in recently burned landscapes. Journal of Applied Ecology, 60, 146–157. https://doi.org/10.1111/1365-2664.14315  
Abundances of reptiles and mammals (and effort) from burned sites. 

6. Leone, J. B., Pennarola, N. P., Larson, J. L., Oberhauser, K., & Larson, D. L. (2022). Divergent responses of butterflies and bees to burning and grazing management in tallgrass prairies. Ecology and Evolution, 12, e9532. https://doi.org/10.1002/ece3.9532   
Abundances of Butterflies and Bees in burned or grazed prairies. And data extraction from .pdf is _fun!_

7. Aguiar, W.M.D. and Gaglianone, M.C., 2012. Euglossine bee communities in small forest fragments of the Atlantic Forest, Rio de Janeiro state, southeastern Brazil (Hymenoptera, Apidae). Revista Brasileira de Entomologia, 56(2), pp.210-219. http://dx.doi.org/10.1590/S0085-56262012005000018. 
Abundances of Euglossina species in fragments of different sizes and elevations. And data extraction from .pdf is _fun!_

8. Almeida-Gomes, M. and Rocha, C.F.D., 2014. Diversity and distribution of lizards in fragmented Atlantic Forest landscape in Southeastern Brazil. Journal of Herpetology, 48(3), pp.423-429. https://doi.org/10.1670/12-187  
Abundances of lizards and geckos from fragments in two size categories. And data extraction from .pdf is _fun!_
  
9. Cabrera-Guzman, E. & Reynoso, V.H., 2012. Amphibian and reptile communities of rainforest fragments: Minimum patch size to support high richness and abundance. Biodiversity and Conservation, 21(12), pp.3243-3265. https://doi.org/10.1007/s10531-012-0312-4  
Abundances of Amphibian and Reptile species from fragments of different sizes, elevation, slope and temperature. And data extraction from .pdf is _fun!_

10. Filgueiras, B.K.C., Iannuzzi, L. & Leal, I.R., 2011. Habitat fragmentation alters the structure of dung beetle communities in the Atlantic Forest. Biological Conservation, 144(1), pp.362-369. https://doi.org/10.1016/j.biocon.2010.09.013  
Abundances of beetle species in fragments along an area gradient and tree density gradient. And data extraction from .doc is _fun!_

11. Lion, M.B., Garda, A.A. & Fonseca, C.R., 2014. Split distance: A key landscape metric shaping amphibian populations and communities in forest fragments. Diversity and Distributions, 20(11), pp.1245-1257. https://onlinelibrary.wiley.com/doi/10.1111/ddi.12228. see note at next paper

12. Lion, M.B., Garda, A.A., Santana, D.J. and Fonseca, C.R., 2016. The conservation value of small fragments for Atlantic forest reptiles. Biotropica, 48(2), pp.265-275. https://onlinelibrary.wiley.com/doi/full/10.1111/btp.12277 Mostly the exact same sites, but different animals than in the previous paper. These papers could be combined to perform new tests. For example, reptiles and amphibians could be combined, and the drivers tested in both papers seem to be not exacty the same. 


### Helpful Resources

Some code about data extraction in R can be found in this page:  
https://github.com/chase-lab/biodiv-patterns-course-2021/tree/main/week%202/3%20-%20Wednesday
