library(samr)
library(tidyverse)

# assumes modelling exploration is loaded in the R session
data1_sam <- data1 %>%
    select(donor_id, outcome, Feature, value) %>%
    mutate(outcome = factor(outcome, labels = c(1, 2))) %>%
    pivot_wider(
                names_from = Feature,
                values_from = value
                ) %>%
    select(-donor_id)

data1_sam_y <- data1_sam[['outcome']]
data1_sam_x <- t(as.data.frame(data1_sam[-1]))

samobj1 <- samr::SAM(
           data1_sam_x,
           data1_sam_y,
           resp.type="Two class unpaired",
           fdr.output = 0.5,
           nperms = 1000,
           genenames = rownames(data1_sam_x)
)
samobj1


############# DATASET 2
data2_sam <- data2 %>%
    select(donor_id, outcome, Feature, value) %>%
    mutate(outcome = factor(outcome, labels = c(1, 2))) %>%
    pivot_wider(
                names_from = Feature,
                values_from = value
                ) %>%
    select(-donor_id)

data2_sam_y <- data2_sam[['outcome']]
data2_sam_x <- t(as.data.frame(data2_sam[-1]))

samobj2 <- samr::SAM(
           data2_sam_x,
           data2_sam_y,
           resp.type="Two class unpaired",
           fdr.output = 0.01,
           nperms = 1000,
           genenames = rownames(data2_sam_x)
)
samobj2
