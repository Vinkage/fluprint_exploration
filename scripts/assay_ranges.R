library(tidyverse)
library(ggpubr)

data <- read_csv("../csv/data_per_assay_per_year_per_outcome.csv")

tbl <- data %>%
    mutate(
        assay_type = factor(assay, labels = (
            c(
                "MSD (Arb. Intensity)",
                "Multiplex cytokine (log2 Z-score)",
                "Cell Cytometry (% parent popul.)",
                "Multiplex cytokine (log2 Z-score)",
                "Phospho cytometry (flow cytometery,\n 90 %tile fold-change)",
                "Cell Cytometry (% parent popul.)",
                "Phospho cytometry (Mass cytometry, arcsinh diff.)",
                "Complete blood count (abs. count/uL)",
                "MSD (Arb. Intensity)",
                "Cell Cytometry (% parent popul.)",
                "MSD (Arb. Intensity)",
                "Multiplex cytokine (log2 Z-score)",
                "Multiplex cytokine (log2 Z-score)",
                "Cell Cytometry (% parent popul.)"
            )
        ))
    ) %>%
    filter(
           !(assay_type=="Phospho cytometry (flow cytometery,\n 90 %tile fold-change)"
           &
            data > 10000)
    )
plt <- tbl %>%
    ggplot(aes(data, fill=assay_type)) +
    geom_histogram(show.legend=F) +
    facet_wrap(~ factor(assay_type), scales="free") +
    theme_pubclean()
plt
ggsave("../images/assay_value_distributions.png", plt, width = 2 * 15, height = 19, units = "cm")
