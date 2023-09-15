## Morning session

Class beginning time: 10:10

### MoBsim
Please, BEFORE THE CLASS, run the following R code to install packages in preparation of our exercise with MoBsim:
```
install.packages(c('shiny', 'shinyBS', 'pals', 'shinyjs', 'devtools', 'DT', 'markdown'))
devtools::install_github('albansagouis/mobsim@master')
```
Please, DO NOT install `mobsim` through `install.packages("mobsim")`.

And make sure this works:
```
shiny::runGitHub("albansagouis/mobsim_app", ref = "master")
```
If the shiny app does not appear or if it crashes, please send me an email: alban.sagouis@idiv.de


## Afternoon session

Description of the projects to prepare for Friday afternoon.

What can we learn from studying different biodiversity metrics? 
-	Richness (S or alpha): number of species, but will be heavily influences by rare species with an abundance of 1.  
-	Abundance (N): are there differences in number of individuals?
-	Rarefied richness: can the difference in richness simply be explained by differences in abundance? 
-	PIE – is there a difference in evenness between treatments? (note that Shannon is always somewhere in between alpha and PIE, and may therefore not be super interesting. 
-	Gamma diversity in comparison to alpha diversity: is the difference in richness scale dependent (is the interpretation for Gamma different than for Alpha?). Could this indicate homogenization of the community? 
-	Beta diversity: is treatment X more homogeneous than treatment Y? is there a difference in species composition between treatments? 



Group assignment 

Find 5 papers that compared different treatments of some ecological factor (land use, different time points, elevation, pollution, etc) in terms of species richness or other metrics and calculate our other favorite metrics: Rarefied richness, PIE, beta (if you really want to diver into that rabbit hole), etc. How often do you find different interpretations than the original authors?  
You will need raw data that provide for each species the abundance in each plot (not aggregated over plots or treatments). Not all papers provide this. Look for papers published in the last 3-4 years. 

1)	Find some papers that study species richness differences 
for example: 

- https://www.frontiersin.org/articles/10.3389/fevo.2022.1021677/full
- https://link.springer.com/article/10.1007/s13157-022-01643-6
- https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.13332
- https://www.mdpi.com/2071-1050/14/7/4002
- https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.14315
- https://onlinelibrary.wiley.com/doi/10.1002/ece3.9532
- Aguiar, W.M.D. and Gaglianone, M.C., 2012. Euglossine bee communities in small forest fragments of the Atlantic Forest, Rio de Janeiro state, southeastern Brazil (Hymenoptera, Apidae). Revista Brasileira de Entomologia, 56(2), pp.210-219.
- Almeida-Gomes, M. and Rocha, C.F.D., 2014. Diversity and distribution of lizards in fragmented Atlantic Forest landscape in Southeastern Brazil. Journal of Herpetology, 48(3), pp.423-429.
- Cabrera-Guzman, E. & Reynoso, V.H., 2012. Amphibian and reptile communities of rainforest fragments: Minimum patch size to support high richness and abundance. Biodiversity and Conservation, 21(12), pp.3243-3265.
- Filgueiras, B.K.C., Iannuzzi, L. & Leal, I.R., 2011. Habitat fragmentation alters the structure of dung beetle communities in the Atlantic Forest. Biological Conservation, 144(1), pp.362-369.
- Lion, M.B., Garda, A.A. & Fonseca, C.R., 2014. Split distance: A key landscape metric shaping amphibian populations and communities in forest fragments. Diversity and Distributions, 20(11), pp.1245-1257.
- Lion, M.B., Garda, A.A., Santana, D.J. and Fonseca, C.R., 2016. The conservation value of small fragments for Atlantic forest reptiles. Biotropica, 48(2), pp.265-275.
- https://doi.org/10.1590/S1676-06032006000200009
- https://doi.org/10.1007/s10531-007-9189-z
- https://www.scielo.br/j/bn/a/c4TBHHcGYzgntPPSkmQDB4M/?format=pdf


3)	Download the underlying raw data 
4)	Wrangle the data to put it in the right format 
5)	Check the result of the original papers
6)	Calculate the metrics you're interested in (alpha, beta, gamma, rarefied richness, PIE, Shannon etc)
7)	Compare to the results of the authors. How often do you find differences? Which important patterns/ differences have the original authors missed (if any)?
 

2 paths: 
Find publication (Google scholar, Web of Science (university access)) -> data repo 
Repositories (Dryad, NERC, EDI, LTER) -> find paper that belongs to it. 

Some code about data extraction in R can be found in this page:  
https://github.com/chase-lab/biodiv-patterns-course-2021/tree/main/week%202/3%20-%20Wednesday



What we expect for the presentation: 

1) question
2) Methods (explanation of datasets, how did you analyse them? which metrics did you extract? etc.) 
3) Results with graphs 
4) Conclusions.






