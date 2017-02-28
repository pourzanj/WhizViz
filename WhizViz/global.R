library(dplyr)

load("../data/WhizViz.Rdata")

#need all the names of factors so we can choose what to color, facet by
MasterListFactorNames <- MasterListWv %>% purrr::keep(is.factor) %>% names