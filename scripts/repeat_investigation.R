library(tidyverse)
library(ggpubr)

data <- read_csv("../csv/repeat_investigation.csv")

studies_data <- read_csv("../csv/donors_list.csv") %>%
    group_by(study_id) %>%
    summarise(
              count = n()
              )

tbl <- data %>%
    na_if("NULL") %>%
    select(
           donor_id,
           visit_year,
           study_id,
           visit_type_hai,
           vaccine_resp
    ) %>%
    group_by(
             donor_id, study_id, visit_year
             ) %>%
    summarise(
              type = visit_type_hai,
              response = vaccine_resp
    )
response_vec <- tbl %>%
    group_map(
              ~ {
                  if (any(!is.na(.$response))) {
                      rep(TRUE, nrow(.))
                  } else {
                      rep(FALSE, nrow(.))
                  }
              }
    )
tbl_pre <- tbl %>%
    ungroup() %>%
    mutate(response_recorded = unlist(response_vec)) %>%
    filter(type == "pre" | type == "single") %>%
    group_by(
             donor_id, study_id
             ) %>%
    summarise(
              count = n(),
              year = visit_year,
              type = type,
              response = response,
              recorded = response_recorded
    )
tbl_pre

plt <- tbl_pre %>%
    ggplot(aes(count, fill = factor(recorded, labels=c("No (for various reasons)", "Yes")))) +
    geom_bar() +
    facet_wrap(~ factor(study_id, labels = c(
        paste("SLVP015\nn=(", as.numeric(studies_data[studies_data$study_id == 15,][2]), ")"),
        paste("SLVP017\nn=(", as.numeric(studies_data[studies_data$study_id == 17,][2]), ")"),
        paste("SLVP018\nn=(", as.numeric(studies_data[studies_data$study_id == 18,][2]), ")"),
        paste("SLVP021\nn=(", as.numeric(studies_data[studies_data$study_id == 21,][2]), ")"),
        paste("SLVP024\nn=(", as.numeric(studies_data[studies_data$study_id == 24,][2]), ")"),
        paste("SLVP028\nn=(", as.numeric(studies_data[studies_data$study_id == 28,][2]), ")"),
        paste("SLVP029\nn=(", as.numeric(studies_data[studies_data$study_id == 29,][2]), ")"),
        paste("SLVP030\nn=(", as.numeric(studies_data[studies_data$study_id == 30,][2]), ")")
    ))) +
    labs(x="Seasons a donor visited", y = "Visits by donors with same amount of total seasons", fill = "Response classification available") +
    theme_pubr() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
plt

ggsave("../images/repeat_visits_per_study.png", plt, width = 2*15, height = 25, dpi=300, units = "cm")
