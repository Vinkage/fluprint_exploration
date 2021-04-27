library(tidyverse)
library(ggpubr)

data <- read_csv("../csv/data_per_assay_per_year_per_outcome.csv")

studies_data <- read_csv("../csv/donors_list.csv") %>%
    group_by(study_id) %>%
    summarise(
              count = n()
              )

tbl <- data %>%
    mutate(
        assay_type = factor(assay, labels = (
            c(
                "MSD",
                "Multiplex cytokine",
                "Cell Cytometry",
                "Multiplex cytokine",
                "Phospho cytometry",
                "Cell Cytometry",
                "Phospho cytometry",
                "Complete blood count",
                "MSD",
                "Cell Cytometry",
                "MSD",
                "Multiplex cytokine",
                "Multiplex cytokine",
                "Cell Cytometry"
            )
        ))
    )

id_plt <- tbl %>%
    group_by(year, assay, outcome) %>%
    summarise(
        measurements = n(),
        study = study
    ) %>%
    ggplot(aes(year, measurements / 1000, color = factor(assay))) +
    geom_line() +
    geom_point() +
    ylim(-1, 10) +
    facet_grid(rows = vars(factor(outcome, labels = c("low", "high")))) +
    labs(y = "number of features (thousands)") +
    scale_color_discrete(name="Assay type") +
    theme_pubclean()

type_plt <- tbl %>%
    group_by(year, assay_type, outcome) %>%
    summarise(
        measurements = n(),
        study = study
    ) %>%
    ggplot(aes(year, measurements / 1000, color = factor(assay_type))) +
    geom_line(show.legend=F) +
    geom_point(show.legend=F) +
    ylim(-1, 10) +
    facet_grid(rows = vars(factor(outcome, labels = c("low", "high")))) +
    labs(y = "number of features (thousands)") +
    scale_color_discrete(name="Assay type") +
    theme_pubclean()

study_plt <- tbl %>%
    group_by(year, assay_type) %>%
    summarise(
        measurements = n(),
        study = study
    ) %>%
    ggplot(aes(year, measurements / 1000, color = factor(assay_type))) +
    geom_line() +
    geom_point() +
    ylim(-1, 10) +
    facet_grid(rows = vars(factor(study, labels = c(
        paste("SLVP015\nn=(", as.numeric(studies_data[studies_data$study_id == 15,][2]), ")"),
        paste("SLVP017\nn=(", as.numeric(studies_data[studies_data$study_id == 17,][2]), ")"),
        paste("SLVP018\nn=(", as.numeric(studies_data[studies_data$study_id == 18,][2]), ")"),
        paste("SLVP021\nn=(", as.numeric(studies_data[studies_data$study_id == 21,][2]), ")"),
        paste("SLVP024\nn=(", as.numeric(studies_data[studies_data$study_id == 24,][2]), ")"),
        paste("SLVP028\nn=(", as.numeric(studies_data[studies_data$study_id == 28,][2]), ")"),
        paste("SLVP029\nn=(", as.numeric(studies_data[studies_data$study_id == 29,][2]), ")"),
        paste("SLVP030\nn=(", as.numeric(studies_data[studies_data$study_id == 30,][2]), ")")
    )))) +
    labs(y = "number of features (thousands)", legend="Assay type") +
    scale_color_discrete(name="Assay type") +
    theme_pubclean()


top_half <- ggarrange(
                      id_plt,
                      type_plt,
                      ncol=2,
                      labels = c("A","B")
          )

whole <- ggarrange(
                   top_half,
                   study_plt,
                   nrow=2,
                   labels=c("", "C"),
                   heights = c(1,2)
)
whole

ggsave("../images/exp_data_numbers.pdf", whole, width = 21, height = 29.7, dpi=300, units = "cm")
