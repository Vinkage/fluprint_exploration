library(tidyverse)
library(caret)
library(MLeval)
library(pROC)


source("./data_prep.R")
# load("./modelling_results.RData")
load("modelling_results_withrrlda.RData")

second_visit_donors <- mike_wide[["donor_id"]]
second_visit_donors

second_visits <- c()
results <- results
data_list <- sets_partitions
donor_list <- list()
feature_list <- list()
count = 1
for (set in data_list) {
    donors <- c(set[["train"]][["donor_id"]], set[['test']][['donor_id']])
    features <- names(set[["train"]])
    donor_list[[count]] <- donors
    feature_list[[count]] <- features
    second_visits <- c(second_visits, length(intersect(donors, second_visit_donors)))
    count = count + 1
}
second_visits

donor_list <- donor_list[c(14, 16, 19)]
feature_list <- feature_list[c(14, 16, 19)]

donor_intersect <- intersection(donor_list[[1]], donor_list[[2]], donor_list[[3]])
n_donor_intersect <- length(donor_intersect)

feature_intersect1 <- intersection(feature_list[[1]], feature_list[[2]])
n_feature_intersect1 <- length(feature_intersect1)

# which(second_visits > 15)
# which.max(second_visits)

# dataset14_rrlda_eval <- evalm(results[["rrlda"]][[14]])
# dataset14_nb_eval <- evalm(results[["naive_bayes"]][[14]])
# dataset14_rf_eval <- evalm(results[["rf"]][[14]])
# dataset14_reglog_eval <- evalm(results[["regLogistic"]][[14]])
#
# dataset16_rrlda_eval <- evalm(results[["rrlda"]][[16]])
# dataset16_nb_eval <- evalm(results[["naive_bayes"]][[16]])
# dataset16_rf_eval <- evalm(results[["rf"]][[16]])
# dataset16_reglog_eval <- evalm(results[["regLogistic"]][[16]])
#
# dataset19_rrlda_eval <- evalm(results[["rrlda"]][[19]])
# dataset19_nb_eval <- evalm(results[["naive_bayes"]][[19]])
# dataset19_rf_eval <- evalm(results[["rf"]][[19]])
# dataset19_reglog_eval <- evalm(results[["regLogistic"]][[19]])

dataset1_rrlda_eval <- evalm(results[["rrlda"]][[1]])
dataset1_nb_eval <- evalm(results[["naive_bayes"]][[1]])
dataset1_rf_eval <- evalm(results[["rf"]][[1]])
dataset1_reglog_eval <- evalm(results[["regLogistic"]][[1]])

dataset2_rrlda_eval <- evalm(results[["rrlda"]][[2]])
dataset2_nb_eval <- evalm(results[["naive_bayes"]][[2]])
dataset2_rf_eval <- evalm(results[["rf"]][[2]])
dataset2_reglog_eval <- evalm(results[["regLogistic"]][[2]])

dataset3_rrlda_eval <- evalm(results[["rrlda"]][[3]])
dataset3_nb_eval <- evalm(results[["naive_bayes"]][[3]])
dataset3_rf_eval <- evalm(results[["rf"]][[3]])
dataset3_reglog_eval <- evalm(results[["regLogistic"]][[3]])


get_test_auc <- function(dataset, model) {
    used_datasets <- c(14,16,19)
    test_data <- data_list[[used_datasets[dataset]]][["test"]]
    print(test_data)
    model_res <- results[[model]][[dataset]]
    preds <- predict(model_res, newdata = test_data[-c(1, 2)])
    preds <- as.numeric(preds)
    auc_score <- auc(roc(
        response = test_data[["outcome"]],
        predictor = preds,
        ret = c("roc")
    ))
    round(as.numeric(auc_score), 2)
}



d1_rrlda_met <- as_tibble(dataset1_rrlda_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "rrlda")
d1_nb_met <- as_tibble(dataset1_nb_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "nb")
d1_rf_met <- as_tibble(dataset1_rf_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "rf")
d1_reglog_met <- as_tibble(dataset1_reglog_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "reglog")
d1_met <- bind_rows(list(d1_rrlda_met, d1_nb_met, d1_rf_met, d1_reglog_met)) %>%
    mutate(
        `Score (CI)` = paste(Score, "(", CI, ")")
    ) %>%
    pivot_wider(
        id_cols = model,
        values_from = Score,
        names_from = Metric
    ) %>%
    mutate(
        `test_AUC` = c(
                       get_test_auc(1, "rrlda"),
                       get_test_auc(1, "naive_bayes"),
                       get_test_auc(1, "rf"),
                       get_test_auc(1, "regLogistic")
        )
    ) %>%
    select(-`AUC-PRG`, -`AUC-PR`, -Informedness) %>%
    kable(format = "latex", booktabs = TRUE)
d1_met

d2_rrlda_met <- as_tibble(dataset2_rrlda_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "rrlda")
d2_nb_met <- as_tibble(dataset2_nb_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "nb")
d2_rf_met <- as_tibble(dataset2_rf_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "rf")
d2_reglog_met <- as_tibble(dataset2_reglog_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "reglog")
d2_met <- bind_rows(list(d2_rrlda_met, d2_nb_met, d2_rf_met, d2_reglog_met)) %>%
    mutate(
        `Score (CI)` = paste(Score, "(", CI, ")")
    ) %>%
    pivot_wider(
        id_cols = model,
        values_from = Score,
        names_from = Metric
    ) %>%
    mutate(
        `test_AUC` = c(
                       get_test_auc(2, "rrlda"),
                       get_test_auc(2, "naive_bayes"),
                       get_test_auc(2, "rf"),
                       get_test_auc(2, "regLogistic")
        )
    ) %>%
    select(-`AUC-PRG`, -`AUC-PR`, -Informedness) %>%
    kable(format = "latex", booktabs = TRUE)
d2_met

d3_rrlda_met <- as_tibble(dataset3_rrlda_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "rrlda")
d3_nb_met <- as_tibble(dataset3_nb_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "nb")
d3_rf_met <- as_tibble(dataset3_rf_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "rf")
d3_reglog_met <- as_tibble(dataset3_reglog_eval$stdres$`Group 1`, rownames = "Metric") %>%
    mutate(model = "reglog")
d3_met <- bind_rows(list(d3_rrlda_met, d3_nb_met, d3_rf_met, d3_reglog_met)) %>%
    mutate(
        `Score (CI)` = paste(Score, "(", CI, ")")
    ) %>%
    pivot_wider(
        id_cols = model,
        values_from = Score,
        names_from = Metric
    ) %>%
    mutate(
        `test_AUC` = c(
                       get_test_auc(3, "rrlda"),
                       get_test_auc(3, "naive_bayes"),
                       get_test_auc(3, "rf"),
                       get_test_auc(3, "regLogistic")
        )
    ) %>%
    select(-`AUC-PRG`, -`AUC-PR`, -Informedness) %>%
    kable(format = "latex", booktabs = TRUE)
d3_met
