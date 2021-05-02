library(tidyverse)
library(caret)
library(ggpubr)
library(corrplot)
library(psych)

load("modelling_results_withrrlda.RData")
source("data_prep.R")
source("data_prep_desc.R")

results <- results
important_features <- list()
for (model in results) {
    for (dataset in c(1,2,3)) {
        model_name <- model[[dataset]]$method
        # important_features[[model_name]] <- list()
        important_features[[model_name]][[dataset]] <- varImp(model[[dataset]])
    }
}

important_features$naive_bayes[[1]]
important_features$naive_bayes[[2]]

nb_importance1 <- important_features$naive_bayes[[1]]$importance %>%
    as_tibble(rownames = 'Feature') %>%
    mutate(Score = High) %>%
    select(Feature, Score) %>%
    filter(Score >= 50) %>%
    arrange(Score) %>%
    mutate(Feature = factor(Feature, Feature))
nb_importance1

nb_importance2 <- important_features$naive_bayes[[2]]$importance %>%
    as_tibble(rownames = 'Feature') %>%
    mutate(Score = High) %>%
    select(Feature, Score) %>%
    filter(Score >= 50) %>%
    arrange(Score) %>%
    mutate(Feature = factor(Feature, Feature))
nb_importance2

importance_plt1 <- nb_importance1 %>%
    ggplot(aes(Score, Feature)) +
    geom_bar(stat="identity", fill = "green4") +
    theme_pubclean()
importance_plt1

importance_plt2 <- nb_importance2 %>%
    ggplot(aes(Score, Feature)) +
    geom_bar(stat="identity", fill = "green4") +
    theme_pubclean()
importance_plt2


top3_features1 <- nb_importance1 %>%
    arrange(desc(Score)) %>%
    slice(1:3,) %>%
    select(Feature)

top3_features1 <- as.character(top3_features1[['Feature']])

top3_features2 <- nb_importance2 %>%
    arrange(desc(Score)) %>%
    slice(1:3,) %>%
    select(Feature)

top3_features2 <- as.character(top3_features2[['Feature']])

data1 <- bind_rows(sets_partitions[[14]][['test']], sets_partitions[[14]][['train']]) %>%
    select(donor_id, outcome, top3_features1) %>%
    mutate(outcome_dummy = factor(outcome, labels = c("L", "H"))) %>%
    pivot_longer(
                 cols = top3_features1,
                 names_to = "Feature"
                 ) %>%
    group_by(outcome, Feature) %>%
    summarise(
              Feature = Feature,
              outcome_dummy = outcome_dummy,
              donor_id = donor_id,
              value = value
              )
xlabels_1 <- data1 %>%
    group_map(
        ~ {
            rep(paste(.$outcome_dummy[1], "\nn=(", length(unique(.$donor_id)), ")"), nrow(.))
        }
    )
data1 <- data1 %>%
    ungroup() %>%
    mutate(label = unlist(xlabels_1))



f <- function(x, height = 1) {
    ans <- median(x)
    data.frame(ymin = ans - height / 2, ymax = ans + height / 2, y = ans)
}

# 90th percentile values
plt1_top3 <- data1 %>%
    ggplot(aes(label, value, fill=outcome)) +
    geom_violin(aes(color = outcome), alpha = 0.4, show.legend = F) +
    facet_wrap(~ Feature, scales="free") +
    geom_boxplot(width = 0.1, show.legend = F) +
    geom_point(aes(fill = outcome), color = "black", shape = 23, show.legend = F) +
    stat_summary(
        fun.data = f, geom = "crossbar",
        colour = NA, fill = "black", width = 0.3, alpha = 1.0
    ) +
    theme_pubclean() +
    labs(x = "", y = "PBMC 90th percentile value")

data2 <- bind_rows(sets_partitions[[16]][['test']], sets_partitions[[16]][['train']]) %>%
    select(donor_id, outcome, top3_features2) %>%
    mutate(outcome_dummy = factor(outcome, labels = c("L", "H"))) %>%
    pivot_longer(
                 cols = top3_features2,
                 names_to = "Feature"
                 ) %>%
    group_by(outcome, Feature) %>%
    summarise(
              Feature = Feature,
              outcome_dummy = outcome_dummy,
              donor_id = donor_id,
              value = value
              )
xlabels_2 <- data2 %>%
    group_map(
        ~ {
            rep(paste(.$outcome_dummy[1], "\nn=(", length(unique(.$donor_id)), ")"), nrow(.))
        }
    )
data2 <- data2 %>%
    ungroup() %>%
    mutate(label = unlist(xlabels_2))



# 90th percentile values
plt2_top3 <- data2 %>%
    ggplot(aes(label, value, fill=outcome)) +
    geom_violin(aes(color = outcome), alpha = 0.4, show.legend = F) +
    facet_wrap(~ Feature, scales="free") +
    geom_boxplot(width = 0.1, show.legend = F) +
    geom_point(aes(fill = outcome), color = "black", shape = 23, show.legend = F) +
    stat_summary(
        fun.data = f, geom = "crossbar",
        colour = NA, fill = "black", width = 0.3, alpha = 1.0
    ) +
    theme_pubclean() +
    labs(x = "", y = "PBMC 90th percentile value")
plt2_top3


############# Second visit data here ##################

## DATASET 2

data2_second <- mike_second_visit %>%
    ungroup() %>%
    filter(donor_id %in% data2$donor_id) %>%
    mutate(
           Feature = name_formatted,
           value = data,
           visit = rep("Second", nrow(.)),
           outcome = factor(outcome)
           ) %>%
    select(donor_id, outcome, Feature, value, visit) %>%
    bind_rows(data2 %>%
              mutate(visit = rep("First", nrow(.))) %>%
              select(donor_id, outcome, Feature, value, visit) %>%
              filter(donor_id %in% intersection(mike_second_visit$donor_id, .$donor_id))
    )
data2_second

data2_second_top3 <- data2_second %>%
    filter(Feature %in% top3_features2) %>%
    mutate(outcome_dummy = factor(outcome, labels = c("L", "H")),
           visit_dummy = visit) %>%
    group_by(donor_id) %>%
    arrange(donor_id) %>%
    filter(1 < length(unique(visit))) %>%
    ungroup() %>%
    group_by(visit, outcome)
data2_second_top3

xlabels_2_second_top3 <- data2_second_top3 %>%
    group_map(
        ~ {
            paste(.$visit_dummy[1], .$outcome_dummy[1], "\nn=(", length(unique(.$donor_id)), ")")
        }
    )
xlabels_2_second_top3

data2_second_top3 <- data2_second_top3 %>%
    ungroup() %>%
    mutate(
           label = if_else(outcome == 1,
                           xlabels_2_second_top3[[2]],
                           if_else(visit == "First",
                                   xlabels_2_second_top3[[1]],
                                   xlabels_2_second_top3[[3]]
                           )
                           )
           ) %>%
    filter(value < 1000 & 0 < value) %>%
    group_by(donor_id) %>%
    mutate(changed = 1 < length(unique(outcome)))
data2_second_top3
# 6 outliers

# 90th percentile values
plt2_second_top3 <- data2_second_top3 %>%
    ggplot(aes(label, value)) +
    geom_violin(aes(color = outcome, fill = outcome), alpha = 0.4, show.legend = F) +
    facet_wrap(~ Feature, scales="free") +
    geom_boxplot(width = 0.1, show.legend = F) +
    geom_point(aes(fill = outcome, size = changed), color = "black", shape = 23, show.legend = F) +
    stat_summary(
        fun.data = f, geom = "crossbar",
        colour = NA, fill = "black", width = 0.3, alpha = 1.0
    ) +
    theme_pubclean() +
    labs(x = "", y = "PBMC 90th percentile value")
plt2_second_top3

######################################## dataset 1
data1_second <- mike_second_visit %>%
    ungroup() %>%
    filter(donor_id %in% data1$donor_id) %>%
    mutate(
           Feature = name_formatted,
           value = data,
           visit = rep("Second", nrow(.)),
           outcome = factor(outcome)
           ) %>%
    select(donor_id, outcome, Feature, value, visit) %>%
    bind_rows(data1 %>%
              mutate(visit = rep("First", nrow(.))) %>%
              select(donor_id, outcome, Feature, value, visit) %>%
              filter(donor_id %in% intersection(mike_second_visit$donor_id, .$donor_id))
    )
data1_second

data1_second_top3 <- data1_second %>%
    filter(Feature %in% top3_features1) %>%
    mutate(outcome_dummy = factor(outcome, labels = c("L", "H")),
           visit_dummy = visit) %>%
    group_by(donor_id) %>%
    filter(1 < length(unique(visit))) %>%
    arrange(donor_id) %>%
    ungroup() %>%
    group_by(visit, outcome)
data1_second_top3
xlabels_1_second_top3 <- data1_second_top3 %>%
    group_map(
        ~ {
            paste(.$visit_dummy[1], .$outcome_dummy[1], "\nn=(", length(unique(.$donor_id)), ")")
        }
    )
xlabels_1_second_top3
data1_second_top3 <- data1_second_top3 %>%
    ungroup() %>%
    mutate(
           label = if_else(outcome == 1,
                           xlabels_1_second_top3[[2]],
                           if_else(visit == "First",
                                   xlabels_1_second_top3[[1]],
                                   xlabels_1_second_top3[[3]]
                           )
                           )
           ) %>%
    filter(value < 1000 & 0 < value) %>%
    group_by(donor_id) %>%
    mutate(changed = 1 < length(unique(outcome)))
data1_second_top3

# 90th percentile values
plt1_second_top3 <- data1_second_top3 %>%
    ggplot(aes(label, value)) +
    geom_violin(aes(color = outcome, fill = outcome), alpha = 0.4, show.legend = F) +
    facet_wrap(~ Feature, scales="free") +
    geom_boxplot(width = 0.1, show.legend = F) +
    geom_point(aes(fill = outcome, size = changed), color = "black", shape = 23, show.legend = F) +
    stat_summary(
        fun.data = f, geom = "crossbar",
        colour = NA, fill = "black", width = 0.3, alpha = 1.0
    ) +
    theme_pubclean() +
    labs(x = "", y = "PBMC 90th percentile value")
plt1_second_top3


### Arranging the plot


data1_top <- ggarrange(
                       importance_plt1,
                       plt1_top3,
                       widths = c(1,2),
                       labels = c("A", "B")
)
data1_bot <- ggarrange(
                       plt1_second_top3,
                       labels = "C"
                       )
whole_data1 <- ggarrange(
                       data1_top,
                       data1_bot,
                       nrow = 2
                       )
whole_data1
ggsave("../images/dataset1_nb_feature_exploration.png", whole_data1, width = 2 * 15, height = 19, dpi=300, units = "cm")


data2_top <- ggarrange(
                       importance_plt2,
                       plt2_top3,
                       widths = c(1,2),
                       labels = c("A", "B")
                       )
data2_bot <- ggarrange(
                       plt2_second_top3,
                       labels = c("C")
                       )
whole_data2 <- ggarrange(
                         data2_top,
                         data2_bot,
                         nrow = 2
                         )
whole_data2
ggsave("../images/dataset2_nb_feature_exploration.png", whole_data2, width = 2 * 15, height = 19, dpi=300, units = "cm")


#################### correlations data 1

data1_wide <- bind_rows(sets_partitions[[14]][['test']], sets_partitions[[14]][['train']]) %>%
    select(-donor_id, -outcome)
data1_wide

M <- cor(data1_wide)
# pmat <- cor.mtest(data1_wide)
pmat <- psych::corr.test(M, method="pearson", adjust="BH")$p

png(file = "../images/cor_dataset1.png",
    height = 1.5 * 19,
    width = 2 * 15,
    res = 300,
    units = "cm"
    )
corrplot(M, order="hclust", p.mat=pmat, sig.level=0.0001, insig="blank")
dev.off()

#################### correlations data 2

data2_wide <- bind_rows(sets_partitions[[16]][['test']], sets_partitions[[16]][['train']]) %>%
    select(-donor_id, -outcome)
data2_wide

M <- cor(data2_wide)
# pmat <- cor.mtest(data2_wide)
pmat <- psych::corr.test(M, method="pearson", adjust="none")$p

png(file = "../images/cor_dataset2.png",
    height = 1.5 * 19,
    width = 2 * 15,
    res = 300,
    units = "cm"
    )
corrplot(M, order="hclust", p.mat=pmat, sig.level=0.05, insig="blank")
dev.off()

#################### difference heatmap data 1

after <- mike_second_visit %>%
    ungroup() %>%
    filter(donor_id %in% data1$donor_id) %>%
    mutate(
           Feature = name_formatted,
           value = data,
           visit = rep("Second", nrow(.)),
           outcome = factor(outcome)
           ) %>%
    select(donor_id, outcome, Feature, value) %>%
    filter(Feature %in% names(data1_wide)) %>%
    arrange(donor_id, Feature)
before <- bind_rows(sets_partitions[[14]][['test']], sets_partitions[[14]][['train']]) %>%
    pivot_longer(
                 cols = c(3:last_col()),
                 names_to = "Feature"
                 ) %>%
    select(donor_id, Feature, value) %>%
    filter(donor_id %in% after$donor_id) %>%
    arrange(donor_id, Feature)
diff1 <- after %>%
    mutate(before_value = before$value) %>%
    filter(0 < value) %>%
    mutate(top3 = if_else(Feature %in% top3_features1, 1, if_else(Feature %in% top3_features2, 2, 0))) %>%
    mutate(value = log2(value / before_value)) %>%
    mutate(top3 =factor(top3, labels=c("Not in top3", "Top3 dataset 14", "Top3 dataset 16"))) %>%
    ggplot(aes(reorder(factor(Feature), value, mean), value, fill=top3)) +
    geom_violin() +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "PBMC features", y = "log2 change", fill = "")
diff1
ggsave("../images/second_visit_change1.png", diff1, width = 2 * 15, height = 19, dpi=300, units = "cm")

#################### difference heatmap data 2

after <- mike_second_visit %>%
    ungroup() %>%
    filter(donor_id %in% data2$donor_id) %>%
    mutate(
           Feature = name_formatted,
           value = data,
           visit = rep("Second", nrow(.)),
           outcome = factor(outcome)
           ) %>%
    select(donor_id, outcome, Feature, value) %>%
    filter(Feature %in% names(data2_wide)) %>%
    arrange(donor_id, Feature)
before <- bind_rows(sets_partitions[[16]][['test']], sets_partitions[[16]][['train']]) %>%
    pivot_longer(
                 cols = c(3:last_col()),
                 names_to = "Feature"
                 ) %>%
    select(donor_id, Feature, value) %>%
    filter(donor_id %in% after$donor_id) %>%
    arrange(donor_id, Feature)
diff2 <- after %>%
    mutate(before_value = before$value) %>%
    filter(0 < value) %>%
    mutate(top3 = Feature %in% top3_features1) %>%
    mutate(value = log2(value / before_value)) %>%
    ggplot(aes(reorder(factor(Feature), value, mean), value, fill=top3)) +
    geom_violin(show.legend=F) +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "PBMC features", y = "Unsigned log2 change")
diff2
ggsave("../images/second_visit_change2.png", diff2, width = 2 * 15, height = 19, dpi=300, units = "cm")
