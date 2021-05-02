library(caret)
library(tidyverse)
library(MLeval)

source("./data_prep.R")

data_list <- sets_partitions
results <- list()
models <- c("rrlda", "naive_bayes", "rf", "regLogistic")
fitControl <- trainControl( ## 10-fold CV
    method = "repeatedcv",
    number = 10,
    classProbs = TRUE,
    savePredictions = TRUE,
    repeats = 2
)
for (model in models) {
    dataset = 1
    # for (data in data_list) {
    for (data in data_list[c(14, 16, 19)]) {
        print(paste("Training", model, "on dataset", dataset))
        train <- data[["train"]]
        X_train <- as.data.frame(train[-c(1, 2)])
        Y_train <- train[c(2)][[1]]
        levels(Y_train) <- c("Low", "High")
        set.seed(13121994)
        model_trained <- train(
            X_train,
            y = Y_train,
            method = model,
            trControl = fitControl
        )
        results[[model]][[dataset]] <- model_trained
        dataset = dataset + 1
    }
}
save(results, file="./modelling_results_withrrlda.RData")
# save(results, file="./modelling_results.RData")

