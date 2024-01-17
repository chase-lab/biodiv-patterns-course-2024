## Randomly creating groups
set.seed(42)
students <- c("Ann-Christin Jungnickel", "Brit Bömer",
              "Dominique Janine Penkert", "Irina Kalmanova", "Lukas Griem",
              "Madita Roth" , "Sören Knipper", "Vilte Simanskaite",
              "Lea Reinke")
stringi::stri_extract_all_regex(students, "[A-Z]", simplify = TRUE) |> 
  apply(MARGIN = 1, paste, collapse = "")

groups <- c(paste("Group", rep(1:3, each = 2)),
            rep("Group 4", 3)) |> 
  factor()
sample(students, length(students), replace = FALSE) |> 
  split(f = groups)

## Randomly affecting papers                
papers <- paste("Group", rep(1:3, each = 3)) |> 
  factor()

sample.int(9, replace = FALSE) |> 
  split(f = papers) |> 
  list(`Group 4` = 9:12)

# Group order
## Thursday morning - Alban - data extraction, R - 40 minutes
### Zoom link: https://us04web.zoom.us/j/4253956239?pwd=eFQ0NmFqbnZlNHZRVG54NjVTdWRYUT09
sample.int(4)
### Group 3: 09:00 - 09:40
### Group 1: 09:50 - 10:30
### Group 4: 10:40 - 11:20
### Group 2: 11:20 - 12:00

## Thursday afternoon
helpers <- factor(rep(c("Roel", "Kim"), each = 2))
sample.int(4) |> 
  split(f = helpers)
### Roel
### Group 4: 13:00 - 13:40
### Group 1: 13:50 - 14:30

### Kim
### Group 3: 13:00 - 13:40
### Group 2: 13:50 - 14:30

## Friday morning
helpers <- factor(rep(c("Kim", "Roel"), each = 2))
sample.int(4) |> 
  split(f = helpers)
### Kim
### Group 1: 10:30 - 11:10
### Group 2: 11:20 - 12:00

### Roel
### Group 3: 10:30 - 11:10
### Group 4: 11:20 - 12:00

## Friday afternoon
### Group order: 3, 2, 1, 4
sample.int(4)