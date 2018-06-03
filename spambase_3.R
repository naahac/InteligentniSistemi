library(pROC)
library(lattice)
library(boot)
library(rpart)
library(e1071)
#install.packages('neuralnet')
library(neuralnet)

data <-
  read.csv2(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",
    header = FALSE,
    sep = ","
  )

colnames(data) <-
  c(
    "word_freq_make",
    "word_freq_address",
    "word_freq_all",
    "word_freq_3d",
    "word_freq_our",
    "word_freq_over",
    "word_freq_remove",
    "word_freq_internet",
    "word_freq_order",
    "word_freq_mail",
    "word_freq_receive",
    "word_freq_will",
    "word_freq_people",
    "word_freq_report",
    "word_freq_addresses",
    "word_freq_free",
    "word_freq_business",
    "word_freq_email",
    "word_freq_you",
    "word_freq_credit",
    "word_freq_your",
    "word_freq_font",
    "word_freq_000",
    "word_freq_money",
    "word_freq_hp",
    "word_freq_hpl",
    "word_freq_george",
    "word_freq_650",
    "word_freq_lab",
    "word_freq_labs",
    "word_freq_telnet",
    "word_freq_857",
    "word_freq_data",
    "word_freq_415",
    "word_freq_85",
    "word_freq_technology",
    "word_freq_1999",
    "word_freq_parts",
    "word_freq_pm",
    "word_freq_direct",
    "word_freq_cs",
    "word_freq_meeting",
    "word_freq_original",
    "word_freq_project",
    "word_freq_re",
    "word_freq_edu",
    "word_freq_table",
    "word_freq_conference",
    "char_freq_semicolon",
    "char_freq_bracket",
    "char_freq_square_bracket",
    "char_freq_exclamation",
    "char_freq_dollar",
    "char_freq_hashtag",
    "capital_run_length_average",
    "capital_run_length_longest",
    "capital_run_length_total",
    "is_spam"
  )

for (i in 1:55) {
  data[[i]] <-  as.double(levels(data[[i]]))[data[[i]]]
}

data$word_binary_free <- ifelse(data$word_freq_free > 0, TRUE, FALSE)
data$word_binary_receive <-
  ifelse(data$word_freq_receive > 0, TRUE, FALSE)
data$word_binary_money <-
  ifelse(data$word_freq_money > 0, TRUE, FALSE)

data$is_spam <- ifelse(data$is_spam == 1, TRUE, FALSE)

#NORAMLIZATION
max = apply(data , 2 , max)
min = apply(data, 2 , min)
data = as.data.frame(scale(data, center = min, scale = max - min))

set.seed(1000) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data
for (i in 1:3) {
  sample <- sample(nrow(data), size = nrow(data), replace = TRUE)
  train <- data[sample,]
  test  <- data[-sample,]
  
  #SVM
  svm.model <- svm(is_spam ~ ., data = train, type = "C-classification")
  svm.pred <- predict(svm.model, train)
  
  #RPART
  rpart.model <- rpart(is_spam ~ ., data = train)
  rpart.pred <- predict(rpart.model, train, type = "vector")
  
  #NN
  n <- names(data)
  s <- paste(n[!n %in% "is_spam"], collapse = " + ")
  formula <- as.formula(paste("is_spam ~", s))
  neural.model <-
    neuralnet(
      formula,
      data = train,
      hidden = c(3, 2, 1) ,
      linear.output = T
    )
  # plot(neural.model)
  neural.pred = compute(neural.model, train[, c(0:60)])
  neural.res <- neural.pred[["net.result"]]
  
  
  # add columns and set thresholds for RPART NN and SVM to train
  res_roc <- roc(train$is_spam,
                 rpart.pred,
                 percent = TRUE,
                 plot = TRUE)
  rpart.threshold <- coords(res_roc, "best", ret = "threshold")
  rpart.pred[rpart.pred < rpart.threshold] <- 0
  rpart.pred[rpart.pred >= rpart.threshold] <- 1
  train["rpart"] <- rpart.pred
  
  
  res_roc <- roc(train$is_spam,
                 neural.res,
                 percent = TRUE,
                 plot = TRUE)
  neural.threshold <- coords(res_roc, "best", ret = "threshold")
  neural.res[neural.res < neural.threshold] <- 0
  neural.res[neural.res >= neural.threshold] <- 1
  train["neural"] <- neural.res
  
  train["svm"] <- svm.pred
  train$svm <- ifelse(train$svm == 1, TRUE, FALSE)
  
  #GLM TRAIN
  glm.model <- glm(is_spam ~ ., data = train, family = binomial)
  glm.predict = predict(glm.model, train, type = "response")
  res_roc <- roc(train$is_spam,
                 glm.predict,
                 percent = TRUE,
                 plot = TRUE)

  glm.threshold <- coords(res_roc, "best", ret = "threshold")
  
  ########################## TEST ############################
  #SVM TEST
  svm.pred <- predict(svm.model, test)
  
  #RPART TEST
  rpart.pred <- predict(rpart.model, test, type = "vector")
  rpart.pred[rpart.pred < rpart.threshold] <- 0
  rpart.pred[rpart.pred >= rpart.threshold] <- 1
  
  #neural TEST
  neural.test.pred = compute(neural.model, test[, c(0:60)])
  neural.test.res <- neural.test.pred[["net.result"]]
  neural.test.res[neural.test.res < neural.threshold] <- 0
  neural.test.res[neural.test.res >= neural.threshold] <- 1
  
  
  #add columns to test
  test["svm"] <- svm.pred
  test$svm <- ifelse(test$svm == 1, TRUE, FALSE)
  misClasificError <- mean(test$svm != test$is_spam)
  print(paste('Accuracy SVM test', 1 - misClasificError))
  
  test["neural"] <- neural.test.res
  misClasificError <- mean(test$neural != test$is_spam)
  print(paste('Accuracy NEURAL test', 1 - misClasificError))
  
  test["rpart"] <- rpart.pred
  misClasificError <- mean(test$rpart != test$is_spam)
  print(paste('Accuracy RPART test', 1 - misClasificError))
  
  #GLM final predict
  glm.test.predict = predict(glm.model, test, type = "response")
  glm.test.predict[glm.test.predict < glm.threshold] <- 0
  glm.test.predict[glm.test.predict >= glm.threshold] <- 1
  
  misClasificError <- mean(glm.test.predict != test$is_spam)
  print(paste('Accuracy GLM final', 1 - misClasificError))
  test_roc <- roc(test$is_spam,
                 glm.test.predict,
                 percent = TRUE,
                 plot = TRUE)
  test_auc <- round(auc(test_roc), 3)
  print(paste("GLM AUC", test_auc))
  # results.accuracy <- append(results.accuracy, misClasificError)
}
