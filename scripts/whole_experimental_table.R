library(tidyverse)

data <- read_csv("../csv/data_per_assay_per_year_per_outcome.csv")

tbl <- data %>%
    rowid_to_column() %>%
    pivot_wider(
                id_cols = rowid,
                names_from = name,
                values_from = data,
    )

