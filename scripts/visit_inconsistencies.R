library(tidyverse)
library(knitr)
library(dlookr)
library(xlsx)
library(ggpubr)
library(corrplot)

data <- read_csv("../csv/visits_all.csv")

class_correct <- data %>%
    na_if("NULL") %>%
    drop_na(vaccine_resp) %>%
    group_by(donor_id, visit_year) %>%
    summarise(
              post = if_else(any(visit_type_hai == 'post'), TRUE, FALSE),
              internal = visit_internal_id,
              hai_type = visit_type_hai,
              gmt = geo_mean,
              fold_change = as.numeric(d_geo_mean),
              response = vaccine_resp,
    )

post_gmt_vec <- class_correct %>%
    group_map(
              ~ {
                  if (any(.$hai_type == 'post')) {
                      rep(.$gmt[.$hai_type == 'post'] >= 40, nrow(.))
                  } else {
                      rep(NA, nrow(.))
                  }
              }
    )

post_gmt_vec

post_gmt_vec <- unlist(post_gmt_vec)

class_correct <- class_correct %>%
    ungroup() %>%
    mutate(
           check_post = if_else(post, 'Post visit', 'No post visit'),
           post_gmt = post_gmt_vec,
           response = recode(factor(response), '0'='Low', '1'='High'),
           check_correct = if_else(
                                   (!is.na(post_gmt) & (!post_gmt | fold_change < 4) & response == 'Low') |
                                       (!is.na(post_gmt) & (post_gmt & fold_change >= 4) & response == 'High'),
                                   'Correct clasification',
                                   'False classification'
                                   ),
           )

seasonal_classification_data <- class_correct %>%
    group_by(donor_id, visit_year) %>%
    summarise(
              response=response,
              check_correct=check_correct,
              check_post
    ) %>%
    ungroup() %>%
    distinct()

seasonal_classification_plot <- seasonal_classification_data %>%
    ggplot(aes(response, fill = response)) +
    geom_bar() +
    geom_text(stat='count', aes(label=paste("n = (",stat(count),")")), vjust= +1) +
    facet_grid(factor(check_correct) ~ factor(check_post)) +
    theme_pubr() +
    labs(title="Seasonal classification of donors by correctness and post visit")
seasonal_classification_plot

ggsave("../images/season_classification.png", seasonal_classification_plot, width = 2 * 15, height = 19, units = "cm")

incorrect_cases <- class_correct %>%
    filter(check_correct == "False classification")

write.xlsx(incorrect_cases, "../incorrect_visits.xlsx", sheetName="sheet1", col.names=T, row.names=T, append=F)



desc <- data %>%
    na_if("NULL") %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(geo_mean = na_if(geo_mean, 0)) %>%
    select(age, cmv_status, ebv_status, bmi, vaccine, geo_mean, d_geo_mean, vaccine_resp, total_data) %>%
    describe() %>%
    column_to_rownames("variable") %>%
    t() %>%
    as_tibble(rownames="stat") %>%
    slice(1:8) %>%
    mutate(across(2:10, round, 1)) %>%
kable(format = "latex", booktabs = TRUE)
clip <- pipe("xclip -selection clipboard", "w")
write(desc, file = clip)
close(clip)


corr_pre <- data %>%
    select(visit_type_hai, age, vaccine, geo_mean, d_geo_mean, vaccine_resp, total_data) %>%
    na_if("NULL") %>%
    mutate(across(2:length(.), as.numeric)) %>%
    filter(!is.na(vaccine_resp) & !is.na(vaccine) & !is.na(d_geo_mean) & !is.na(vaccine_resp) & visit_type_hai == "pre") %>%
    select(-visit_type_hai)

corr_pre


library(gridGraphics)
grab_grob <- function(){
  grid.echo()
  grid.grab()
}

corr_post <- data %>%
    select(visit_type_hai, age, vaccine, geo_mean, d_geo_mean, vaccine_resp, total_data) %>%
    na_if("NULL") %>%
    mutate(across(2:length(.), as.numeric)) %>%
    filter(!is.na(vaccine_resp) & !is.na(vaccine) & !is.na(d_geo_mean) & !is.na(vaccine_resp) & visit_type_hai == "post") %>%
    select(-visit_type_hai)
corr_post


png(file = "../images/corr_plot_visits_pre_complete.png",
    height = 1.5 * 19,
    width = 2 * 15,
    res = 1200,
    units = "cm",
)
par(mfrow=c(2,1))
M <- cor(corr_pre)
corrplot.mixed(M)
M <- cor(corr_post)
corrplot.mixed(M)
dev.off()
