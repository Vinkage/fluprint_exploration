library(tidyverse)
library(ggpubr)

data_simon <- read_csv("../csv/simon_data_extra.csv", na = "NULL")
data_mike <- read_csv("../csv/mike_repeat_visit.csv", na = "NULL")

f <- function(x, height = 0.05) {
    ans <- median(x)
    data.frame(ymin = ans - height / 2, ymax = ans + height / 2, y = ans)
}

simon_year_class <- data_simon %>%
    select(donor_id, year, outcome, hai_response) %>%
    mutate(outcome = factor(outcome, levels = c(1, 0), labels = c("H", "L"))) %>%
    mutate(year_flag = year) %>%
    mutate(outcome_flag = outcome) %>%
    group_by(year, outcome) %>%
    summarise(
        year_flag = year_flag,
        outcome_flag = outcome_flag,
        donor_id = donor_id,
        response = hai_response
    )
simon_year_class


xlabels <- simon_year_class %>%
    group_map(
        ~ {
            rep(paste(.$outcome_flag[1], "\nn=(", length(unique(.$donor_id)), ")"), nrow(.))
        }
    )

count_high_and_low <- data_simon %>%
    group_by(outcome) %>%
    summarise(
              count = length(unique(donor_id))
    )

simon_plt <- simon_year_class %>%
    ungroup() %>%
    mutate(label = unlist(xlabels)) %>%
    ggplot(aes(label, log2(response))) +
    geom_violin(aes(fill = outcome, color = outcome), alpha = 0.2, show.legend = F) +
    geom_boxplot(width = 0.1, show.legend = F) +
    stat_summary(
        fun.data = f, geom = "crossbar",
        colour = NA, fill = "black", width = 0.3, alpha = 1.0
    ) +
    geom_point(aes(fill = outcome), color = "black", shape = 23, show.legend = F) +
    facet_grid(cols = vars(year), scales = "free") +
    theme_pubclean() +
    labs(x = "", y = "Log2 HAI response difference", title = paste(
        "Donor first visit data used in SIMON, total=(", length(unique(simon_year_class$donor_id)), ")",
        ", high=(", count_high_and_low[2,2], ")", ", low=(", count_high_and_low[1,2],")"
    ))

mike_repeat_visits <- data_mike %>%
    mutate(year_flag = year) %>%
    group_by(donor_id, outcome) %>%
    summarise(
        count = length(unique(year_flag)),
        outcome = unique(outcome)
    ) %>%
    arrange(donor_id) %>%
    ungroup()
mike_repeat_visits

repeat_plt <- mike_repeat_visits %>%
    transmute(
              second = sum(count >= 1),
              third = sum(count >= 2),
              fourth = sum(count >= 3),
              fifth = sum(count >= 5)
    ) %>%
    pivot_longer(
                 names_to = "visit",
                 values_to = "count",
                 cols = everything()
    ) %>%
    slice(1:4, ) %>%
    ggplot(aes(factor(visit, levels=c("second", "third", "fourth", "fifth")), count, fill = visit)) +
    geom_bar(stat="identity", show.legend=F) +
    labs(x = "Repeat visit number", y = "Number of donors") +
    theme_pubclean()
repeat_plt

mike_second_visit <- data_mike %>%
    group_by(donor_id) %>%
    filter(year == min(year))
mike_second_visit

mike_second_visit_year_class <- mike_second_visit %>%
    select(donor_id, year, outcome, hai_response) %>%
    mutate(outcome = factor(outcome, levels = c(1, 0), labels = c("H", "L"))) %>%
    mutate(year_flag = year) %>%
    mutate(outcome_flag = outcome) %>%
    group_by(year, outcome) %>%
    summarise(
        year_flag = year_flag,
        outcome_flag = outcome_flag,
        donor_id = donor_id,
        response = hai_response
    )


xlabels_mike <- mike_second_visit_year_class %>%
    group_map(
        ~ {
            rep(paste(.$outcome_flag[1], "\nn=(", length(unique(.$donor_id)), ")"), nrow(.))
        }
    )

count_high_and_low_mike <- mike_second_visit %>%
    group_by(outcome) %>%
    summarise(
              count = length(unique(donor_id))
    )

f <- function(x, height = 0.02) {
    ans <- median(x)
    data.frame(ymin = ans - height / 2, ymax = ans + height / 2, y = ans)
}


mike_plt <- mike_second_visit_year_class %>%
    ungroup() %>%
    mutate(label = unlist(xlabels_mike)) %>%
    ggplot(aes(label, log2(response))) +
    geom_violin(aes(fill = outcome, color = outcome), alpha = 0.2, show.legend = F) +
    geom_boxplot(width = 0.1, show.legend = F) +
    stat_summary(
        fun.data = f, geom = "crossbar",
        colour = NA, fill = "black", width = 0.3, alpha = 1.0
    ) +
    geom_point(aes(fill = outcome), color = "black", shape = 23, show.legend = F) +
    facet_grid(cols = vars(year), scales = "free") +
    theme_pubclean() +
    labs(x = "", y = "Log2 HAI response difference", title = paste(
        "Donor second visit data used in this work, total=(", length(unique(mike_second_visit_year_class$donor_id)), ")",
        ", high=(", count_high_and_low_mike[2,2], ")", ", low=(", count_high_and_low_mike[1,2],")"
    ))
mike_plt

bottom <- ggarrange(
                    repeat_plt,
                    mike_plt,
                    ncol=2,
                    widths=c(1,2),
                    labels=c("B", "C")
)

whole <- ggarrange(
                   simon_plt,
                   bottom,
                   nrow=2,
                   heights=c(1,1),
                   labels=c("A", "")
                   )
ggsave("../images/data_selection.png", whole, width = 2 * 15, height = 19, dpi=300, units = "cm")
