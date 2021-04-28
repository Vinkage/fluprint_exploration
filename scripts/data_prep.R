library(tidyverse)
library(ggpubr)
library(knitr)

data_simon <- read_csv("../csv/simon_data_extra.csv", na = "NULL")
data_mike <- read_csv("../csv/mike_repeat_visit.csv", na = "NULL")

simon_wide <- data_simon %>%
    group_by(donor_id) %>%
    summarise(
              dup = duplicated(data_name)
              ) %>%
    filter(dup)
simon_wide

dview <- data_simon %>%
    group_by(donor_id) %>%
    mutate(dup = duplicated(data_name)) %>%
    filter(dup) %>%
    arrange(data_name) %>%
    filter(donor_id == 285 & data_name == "CD4_pos_T_cells") %>%
    kable(format = "latex", booktabs = TRUE)

%>%
    rowid_to_column() %>%
    pivot_wider(
                id_cols = donor_id,
                names_from = data_name,
                values_from = data,
    )
simon_wide
