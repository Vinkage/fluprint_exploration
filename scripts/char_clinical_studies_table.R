library(tidyverse)
library(knitr)

tbl <- read_csv("../csv/char_clinical_studies.csv")
tbl <- tbl %>%
    select(-1, -3, -8, -9) %>%
    mutate(across(everything(), function(x) {
        rep <- str_replace_all(x, "â\u0080\u00934", "-")
        rep <- str_replace_all(rep, "â\u0080\u0093", "-")
        rep <- str_replace_all(rep, "â\u0080\u0089", " ")
        rep <- str_replace_all(rep, "â\u0089¥", ">=")
        return(rep)
    })) %>%
    kable(format = "latex", booktabs = TRUE)

clip <- pipe("xclip -selection clipboard", "w")
write(tbl, file=clip)
close(clip)
