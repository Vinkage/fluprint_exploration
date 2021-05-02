library(tidyverse)
library(ggpubr)
library(knitr)
library(mulset)
library(caret)

data_simon <- read_csv("../csv/simon_data_extra.csv", na = "NULL")
data_mike <- read_csv("../csv/mike_repeat_visit.csv", na = "NULL")

simon_agg <- data_simon %>%
    group_by(donor_id, data_name) %>%
    summarise(
        data = mean(data)
    )

simon_outcome <- data_simon %>%
    select(donor_id, outcome) %>%
    group_by(donor_id) %>%
    summarise(
        outcome = unique(outcome)
    ) %>%
    ungroup() %>%
    arrange(donor_id)
simon_outcome


simon_wide <- simon_agg %>%
    pivot_wider(
        id_cols = donor_id,
        names_from = data_name,
        values_from = data,
    ) %>%
    ungroup() %>%
    arrange(donor_id) %>%
    mutate(outcome = as.numeric(simon_outcome$outcome)) %>%
    select(donor_id, outcome, everything())

simon_nas <- sum(is.na(simon_wide))
simon_cells <- (dim(simon_wide)[1] * dim(simon_wide)[2])
simon_sparseness <- sum(is.na(simon_wide)) / (dim(simon_wide)[1] * dim(simon_wide)[2])

dview <- data_simon %>%
    group_by(donor_id) %>%
    mutate(dup = duplicated(data_name)) %>%
    filter(dup) %>%
    arrange(data_name) %>%
    filter(donor_id == 285) # & data_name == "CD4_pos_T_cells")

mike_agg <- data_mike %>%
    group_by(donor_id, name_formatted) %>%
    summarise(
        data = mean(data)
    )

mike_wide <- mike_agg %>%
    pivot_wider(
        id_cols = donor_id,
        names_from = name_formatted,
        values_from = data,
    )

mike_nas <- sum(is.na(mike_wide))
mike_cells <- (dim(mike_wide)[1] * dim(mike_wide)[2])
mike_sparseness <- sum(is.na(mike_wide)) / (dim(mike_wide)[1] * dim(mike_wide)[2])

simon_mulset <- mulset(simon_wide)

# set1 <- simon_mulset$`1`
# set1$features_hash
# set1$feature_count
# set1$features
# set1$samples
# set1$samples_count
# set1$datapoints

sets <- list()
count <- 1
for (set in simon_mulset) {
    if (set$feature_count >= 5 & set$samples_count >= 15) {
        sets[[count]] <- simon_wide[set$features] %>%
            drop_na() %>%
            select(donor_id, outcome, everything())
        print(paste("rows:", nrow(sets[[count]]), ", samples by mulset:", set$samples_count))
        count <- count + 1
    }
}


sets_partitions <- list()
set.seed(13121994)
count <- 1
for (set in sets) {
    set$outcome <- as.factor(set$outcome)
    partitions <- createDataPartition(set$outcome, p = 0.75)
    training_rows <- partitions$Resample1
    train <- set[training_rows, ]
    test <- set[-training_rows, ]
    if (nrow(test) >= 10) {
        sets_partitions[[count]] <- list()
        sets_partitions[[count]][["donors"]] <- set[['donor_id']]
        sets_partitions[[count]][["train"]] <- train
        sets_partitions[[count]][["test"]] <- test
        sets_partitions[[count]][["totalOutcomes"]] <- table(set$outcome)
        sets_partitions[[count]][["trainingOutcomes"]] <- table(train$outcome)
        sets_partitions[[count]][["trainingRows"]] <- nrow(train)
        sets_partitions[[count]][["testOutcomes"]] <- table(test$outcome)
        sets_partitions[[count]][["testRows"]] <- nrow(test)
        count <- count + 1
    }
}


tbl <- tibble(dataset = c(), `Rows x Cols` = c(), `total (low / high)` = c(), `train (low / high)` = c(), `test (low / high)` = c())
count = 1
for (set in sets_partitions) {
    rows <- nrow(set[["train"]]) + nrow(set[["test"]])
    cols <- ncol(set[["train"]])
    rowsxcols <- paste(rows, "x", cols)
    tot <- paste(set[["totalOutcomes"]][1], "/", set[["totalOutcomes"]][2], "(", round((set[["totalOutcomes"]][1]) / rows, 2) , ")")
    train <- paste(set[["trainingOutcomes"]][1], "/", set[["trainingOutcomes"]][2])
    test <- paste(set[["testOutcomes"]][1], "/", set[["testOutcomes"]][2])
    tbl <- tbl %>% add_row(
        dataset = count,
        `Rows x Cols` = rowsxcols,
        `total (low / high)` = tot,
        `train (low / high)` = train,
        `test (low / high)` = test
    )
    count = count + 1
}
tbl %>%
kable(format = "latex", booktabs = TRUE)

