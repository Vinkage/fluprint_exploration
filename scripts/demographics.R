library(tidyverse)
library(ggpubr)
orig <- read_csv("../csv/donor_demo.csv")
data <- orig
tbl <- data %>%
    mutate(`CMV status` = factor(
        `CMV status`,
        labels = c("CMV-", "CMV+", "CMV unknown")
    )) %>%
    na_if("NULL") %>%
    replace_na(list(Ethnicity="Unknown")) %>%
    pivot_longer(
        cols = c("Gender", "Ethnicity", "CMV status")
    ) %>%
    group_by(
        value, Response
    ) %>%
    summarise(
        count = n(),
    ) %>%
    mutate(
        total = if_else(Response == 1, sum(data$Response == 1), sum(data$Response == 0)),
        value = factor(value, levels = rev(c(
            "Female",
            "Male",
            "Caucasian",
            "Black or African American",
            "Asian",
            "Hispanic/Latino",
            "Other",
            "Unknown",
            "CMV+",
            "CMV-",
            "CMV unknown"
        ))),
    ) %>%
    ggplot(aes(y = value, fill = value)) +
    geom_bar(aes(x = count / total), stat = "identity", show.legend = F) +
    theme_pubr() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Distribution (%)", y="") +
    facet_wrap(~factor(Response, labels=c(paste("Low responders (n=", sum(data$Response==0),")"), paste("High responders (n=", sum(data$Response==1),")"))))
factors_demo <- tbl

data <- orig
tbl <- data %>%
    select(Age, Response) %>%
    mutate(
        Response_text = factor(
            Response,
            labels = c("Low responders", "High responders")
        )
    ) %>%
    group_by(Age, Response_text) %>%
    summarise(count = n(), Response=Response, Response_text=Response_text, Age=Age) %>%
    mutate(
        total = if_else(Response == 0, sum(data$Response == 0), sum(data$Response == 1)),
        percentage = n()/ total * 100
    ) %>%
    ggplot(aes(x = Age, y=percentage, fill=factor(Response_text))) +
    geom_polygon(show.legend=F) +
    labs(x = "Age") +
    theme_pubr() +
    facet_wrap(~Response_text)
age_responder <- tbl

figure <- ggarrange(
          factors_demo,
          age_responder,
          ncol=1,
          nrow=2,
          labels=c("A", "B")
)

ggsave('../images/demographic.png', figure, width=2*15, height=19, units="cm")

