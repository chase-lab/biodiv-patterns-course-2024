## Morning session

Class beginning time: 10:10

### Mobsim
Please, BEFORE THE CLASS, run the following R code to install packages in preparation of our exercise with Mobsim:
```
install.packages(c('shiny', 'shinyBS', 'pals', 'shinyjs', 'devtools', 'DT', 'markdown'))
devtools::install_github('albansagouis/mobsim@dev')
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

1. Read/scan the paper. What metrics have the authors already calculated? What were their conclusions about what these metrics indicate about the communities under investigation?
2. Find the (raw) biodiversity data provided in or as a supplement to the paper. Note, you might need to wrangle the data in R so that it is in the format required for the different metric calculations.
3.	Think about what else you would be curious to know about the communities studied in the paper. What metrics could help you answer your additional question(s)?
4.	For the metrics you identified in step 3, calculate them in R, using the tutorials and lab exercises from this week as a guide.
5.	Create a graph(s) of your results --- again using the tutorials and lab exercises from this week as a guide.
6.	Interpret your results. What new insights do your results provide for understanding these communities?

### Expectations for the presentation

For each paper: 

- 1 slide dedicated to what the authors already calculated and their interpretation. *Important advice:* Do not get bogged down with the details in each paper. This slide should provide a brief summary that can be explained in 2-3 sentences. For example, what type (species, treatment, etc.) of community did they study? What metrics (N, S, Shannon, etc) did they calculate? What did they find?
- 2-4 slides dedicated to explaining the additional question(s) you addressed, the methods you used (i.e., which metrics you calculated and how), your results (i.e., graphs), and your interpretations of the results. 

### Papers

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

### Helpful Resources

Some code about data extraction in R can be found in this page:  
https://github.com/chase-lab/biodiv-patterns-course-2021/tree/main/week%202/3%20-%20Wednesday
