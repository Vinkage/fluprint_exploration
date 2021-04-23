library(tidyverse)
library(knitr)

orig <- read_csv("../csv/donor_demo.csv")

data <- orig %>%
    na_if("NULL") %>%
    replace_na(list(Ethnicity="Unknown"))

age_table <- tribble(
  ~`Age (y)`, ~Value,
  "Mean \\pm SD",   paste(round(mean(data$Age), 2), "\\pm", round(sd(data$Age), 2)),
  "Median (min. to max. range)",   paste(median(data$Age), ' (', min(data$Age), "-", max(data$Age), ')')
) %>%
kable(format = "latex", booktabs = TRUE)

gender_table <- tribble(
  ~`Gender`, ~Value,
  "Male (%)",   paste(sum(data$Gender == "Male"), "(", round(sum(data$Gender == "Male")/ nrow(data), 3) * 100, ")"),
  "Female",   paste(sum(data$Gender == "Female"), ' (', round(sum(data$Gender == "Female") / nrow(data), 3) * 100 , ")")
) %>%
kable(format = "latex", booktabs = TRUE)


race_table <- tribble(
  ~`Gender`, ~Value,
  "Caucasian (%)",   paste(sum(data$Ethnicity == "Caucasian"), "(", round(sum(data$Ethnicity == "Caucasian")/ nrow(data), 3) * 100, ")"),
  "African American (Black) (%)",   paste(sum(data$Ethnicity == "Black or African American"), ' (', round(sum(data$Ethnicity == "Black or African American") / nrow(data), 3) * 100 , ")"),
  "Asian (%)", paste(sum(data$Ethnicity == "Asian"), ' (', round(sum(data$Ethnicity == "Asian") / nrow(data), 3) * 100 , ")"),
  "Hispanic/Latino (%)", paste(sum(data$Ethnicity == "Hispanic/Latino"), ' (', round(sum(data$Ethnicity == "Hispanic/Latino") / nrow(data), 3) * 100 , ")"),
  "Other (%)", paste(sum(data$Ethnicity == "Other"), ' (', round(sum(data$Ethnicity == "Other") / nrow(data), 3) * 100 , ")"),
  "Unknown (%)", paste(sum(data$Ethnicity == "Unknown"), ' (', round(sum(data$Ethnicity == "Unknown") / nrow(data), 3) * 100 , ")"),
) %>%
kable(format = "latex", booktabs = TRUE)

