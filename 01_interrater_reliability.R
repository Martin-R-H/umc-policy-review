
#### load packages
library(tidyverse)
library(irr)


#### set seed
set.seed(3457)


#### draw a sample of 10 UMCs for the  second rater to code
umcs <- c("Aachen", "Augsburg", "Berlin", "Bochum", "Bonn", "Dresden", "Duisburg-Essen", "Düsseldorf", "Erlangen", "Frankfurt", "Freiburg", "Gießen", "Göttingen", "Greifswald", "Halle-Wittenberg", "Hamburg", "Hannover", "Heidelberg", "Homburg", "Jena", "Kiel", "Köln", "Leipzig", "Lübeck", "Magdeburg", "Mainz", "Mannheim", "Marburg", "München LMU", "München TU", "Münster", "Oldenburg", "Regensburg", "Rostock", "Tübingen", "Ulm", "Witten/Herdecke", "Würzburg")
umc_sample <- sample(umcs, 10)
umc_sample <- replace(umc_sample, umc_sample=='Würzburg', 'Wuerzburg')
umc_sample <- replace(umc_sample, umc_sample=='Witten/Herdecke', 'Witten-Herdecke')


#### write a couple of functions for operations you will perform on both datasets
littledata <- function(x) {
  subset(x, select = c(name, document, preregistration, reporting, data_code_protocol_sharing, open_access, robustness, publication_number, grant_money, impact_factor, authorship_order))
}
renamevars <- function(x) {
  rename(x, reg = preregistration, rep = reporting, sharing = data_code_protocol_sharing , OA = open_access, robust = robustness, pub = publication_number, grants = grant_money, IF = impact_factor, author = authorship_order)
}


#### read in the tables from the original rater and the second rater - get the data from: https://osf.io/4pzjg/
#### remember to set the correct working directory!
data_1st <- read.csv("./UMC_Policy-Review_Protocol_Coding_Table.csv", sep = ";")
data_1st <- data_1st[(data_1st$name %in% umc_sample), ] # for the first rater's dataset, we just select the 10 UMCs that the second rater also coded
data_2nd <- read.csv("./UMC_Policy-Review_Protocol_Coding_Table_2nd_rater.csv", sep = ";")


#### restructure these tables
data_1st <- data_1st %>%
  littledata() %>%
  as_tibble() %>%
  renamevars()  %>%
  pivot_longer(
    cols = c(reg, rep, sharing, OA, robust, pub, grants, IF, author),
    names_to = 'type',
    values_to = 'rating1', 
    values_drop_na = TRUE
  )
data_2nd <- data_2nd %>%
  littledata() %>%
  as_tibble() %>%
  renamevars() %>%
  pivot_longer(
    cols = c(reg, rep, sharing, OA, robust, pub, grants, IF, author),
    names_to = 'type',
    values_to = 'rating2', 
    values_drop_na = TRUE
  )


#### merge the tables
data_full <- right_join(data_1st, data_2nd)


#### now, calculate interrater reliability - we decided to take Cohen's kappa, but for robustness and sensitivity, also tried some other methods that made sense
#### (the other methods all lead to better results than Cohen's kappa)
kappa2(data_full[, 4:5])
cor.test(as.vector(data_full$rating1), as.vector(data_full$rating2), method = "kendall")
meanrho(data_full[, 4:5], fisher = TRUE)
kripp.alpha(t(as.matrix(data_full[, 4:5])), 'ordinal')
