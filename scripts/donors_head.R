library(tidyverse)
library(knitr)
library(dlookr)

data <- read_csv("../csv/visit_multiple_21.csv")

tbl <- data %>%
    select(
        visit_id,
        year = visit_year,
        day = visit_day,
        type = visit_type_hai,
        age = age_round,
        cmv = cmv_status,
        ebv = ebv_status,
        bmi,
        vaccine,
        geo_mean,
        d_geo_mean,
        response = vaccine_resp,
        assay_data_rows = total_data
    ) %>%
kable(format = "latex", booktabs = TRUE)


clip <- pipe("xclip -selection clipboard", "w")
write(tbl, file = clip)
close(clip)

