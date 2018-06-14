library(pROC)
library(lattice)
library(boot)
library(rpart)
library(e1071)
#install.packages('neuralnet')
library(neuralnet)
require(gbm)
#install.packages("ipred")
library(ipred)

seed <- 1000

data <-
  read.csv2(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",
    header = FALSE,
    sep = ","
  )

  cols <- c(
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
  
  
  colnames(data) <- cols
  
  for(i in 1:55) {
    data[[i]] <-  as.double(levels(data[[i]]))[data[[i]]]
  }

data$word_binary_free <- ifelse(data$word_freq_free > 0, TRUE, FALSE)
data$word_binary_receive <-
  ifelse(data$word_freq_receive > 0, TRUE, FALSE)
data$word_binary_money <-
  ifelse(data$word_freq_money > 0, TRUE, FALSE)

data$is_spam <- ifelse(data$is_spam == 1, TRUE, FALSE)

max = apply(data , 2 , max)
min = apply(data, 2 , min)
data = as.data.frame(scale(data, center = min, scale = max - min))

data <- na.omit(data)#remove rows with NA

set.seed(seed)

auc_supermethod <- c()
auc_supermethod2 <- c()
auc_bagging <- c()
auc_randomforest <- c()
auc_xgboost <- c()

for (i in 1:10) {
  dir <- "D:\\"
  pdfPath <- file.path(dir, paste(paste("tretja_naloga_",i, sep = ""),".pdf", sep = ""))
  pdf(pdfPath)
  
  sample <- sample(nrow(data), size = nrow(data), replace = TRUE)
  train_orig <- data[sample,]
  test_orig <- data[-sample,]
  train <- data[sample,]
  test  <- data[-sample,]
  
  print(paste('Start predicting', i))
  
  #################### PREDICT TRAIN ###################################
  #SVM
  svm.model <- svm(is_spam ~ ., data = train, type = "C-classification")
  svm.pred <- predict(svm.model, train)
  
  #RPART
  rpart.model <- rpart(is_spam ~ ., data = train)
  rpart.pred <- predict(rpart.model, train, type = "vector")
  res_roc <- roc(train$is_spam,
                 rpart.pred,
                 percent = TRUE,
                 plot = TRUE)
  rpart.threshold <- coords(res_roc, "best", ret = "threshold")
  rpart.pred[rpart.pred < rpart.threshold] <- 0
  rpart.pred[rpart.pred >= rpart.threshold] <- 1
  
  #NN
  n <- names(data)
  s <- paste(n[!n %in% "is_spam"], collapse = " + ")
  formula <- as.formula(paste("is_spam ~", s))
  neural.model <-
    neuralnet(
      formula,
      data = train,
      hidden = c(3,2,1),
      threshold = 0.5,
      linear.output = F
    )
  # plot(neural.model)
  neural.pred = compute(neural.model, subset(train, select=-c(is_spam)))
  neural.res <- neural.pred[["net.result"]]
  res_roc <- roc(train$is_spam,
                 neural.res[,1],
                 percent = TRUE,
                 plot = TRUE)
  neural.threshold <- coords(res_roc, "best", ret = "threshold")
  #neural.res[neural.res < neural.threshold] <- 0
  #neural.res[neural.res >= neural.threshold] <- 1
  
  
  ########################## ADD COLUMNS TO TRAIN #####################

  train["rpart"] <- rpart.pred
  
  train["neural"] <- neural.res
  
  train["svm"] <- svm.pred
  train$svm <- ifelse(train$svm == 1, TRUE, FALSE)
  
  
  ########################## PREDICT TEST ############################
  #SVM TEST
  svm.pred <- predict(svm.model, test)
  
  #svm.roc <- roc(test$is_spam, svm.pred)
  # auc_svm[i] <- round(auc(svm.roc), 3)
  
  #RPART TEST
  rpart.pred <- predict(rpart.model, test, type = "vector")
  
  # rpart.roc <- roc(test$is_spam, rpart.pred)
  # auc_rpart[i] <- round(auc(rpart.roc), 3)
  
  #rpart.pred[rpart.pred < rpart.threshold] <- 0
  #rpart.pred[rpart.pred >= rpart.threshold] <- 1
  
  #neural TEST
  neural.test.pred = compute(neural.model, subset(test, select=-c(is_spam)))
  neural.test.res <- neural.test.pred[["net.result"]]
  
  #neural.roc <- roc(test$is_spam, neural.test.res)
  #neural.test.res[neural.test.res < neural.threshold] <- 0
  #neural.test.res[neural.test.res >= neural.threshold] <- 1
  
  
  ###################### ADD COLUMNS TO TEST #######################
  test["svm"] <- svm.pred
  test$svm <- ifelse(test$svm == 1, TRUE, FALSE)
  #misClasificError <- mean(test$svm != test$is_spam)
  #print(paste('Accuracy SVM test', 1 - misClasificError))
  
  test["neural"] <- neural.test.res
  #misClasificError <- mean(test$neural != test$is_spam)
  #print(paste('Accuracy NEURAL test', 1 - misClasificError))
  
  test["rpart"] <- rpart.pred
  #misClasificError <- mean(test$rpart != test$is_spam)
  #print(paste('Accuracy RPART test', 1 - misClasificError))
  
  #####################   GLM FINAL PREDICT ##################
  
  #GLM TRAIN
  glm.model <- glm(is_spam ~ ., data = train, family = binomial)
  glm.predict = predict(glm.model, train, type = "response")
  res_roc <- roc(train$is_spam,
                 glm.predict,
                 percent = TRUE,
                 plot = TRUE)
  
  glm.threshold <- coords(res_roc, "best", ret = "threshold")
  
  glm.test.predict = predict(glm.model, test, type = "response")
  glm.test.predict[glm.test.predict < glm.threshold] <- 0
  glm.test.predict[glm.test.predict >= glm.threshold] <- 1
  
  
  misClasificError <- mean(glm.test.predict != test$is_spam)
  print(paste('Accuracy GLM final', 1 - misClasificError))
  test_roc <- roc(test$is_spam, glm.test.predict)
  test_auc <- round(auc(test_roc), 3)
  print(paste("GLM AUC", test_auc))
  
  auc_supermethod[i]<-test_auc
  
  conf_matrix<-table(glm.test.predict,test$is_spam)
  #conf_matrix
  #print(specificity(conf_matrix))
  #print(sensitivity(conf_matrix))
  
  # results.accuracy <- append(results.accuracy, misClasificError)
  
  ########################### SVM FINAL PREDICT ####################

  svm.model <- svm(is_spam ~ ., data = train, type = "C-classification")
  svm.pred <- predict(svm.model, test)

  res_roc <- roc(test$is_spam, as.numeric(svm.pred))
  fit_auc <- round(auc(res_roc), 3)
  auc_supermethod2[i]<-fit_auc
  
  #######################   BOOSTING #######################

  boost=gbm(is_spam ~ . ,data = train_orig,distribution = "gaussian",n.trees = 1000,
                   shrinkage = 0.01, interaction.depth = 4)

  summary(boost)

  n.trees = seq(from=100 ,to=1000, by=100) #no of trees-a vector of 100 values

  #Generating a Prediction matrix for each Tree
  predmatrix<-predict(boost,test_orig,n.trees = n.trees)
  dim(predmatrix) #dimentions of the Prediction Matrix

  #Calculating The Mean squared Test Error
  test.error<-with(test_orig,apply( (predmatrix-is_spam)^2,2,mean))
  head(test.error) #contains the Mean squared test error for each of the 100 trees averaged


  #Plotting the test error vs number of trees

  plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")


  ########################   BAGGING ##################

  bagging <- bagging(as.factor(is_spam) ~ ., data = train_orig, mfinal=15, coob = TRUE) #control(rpart.control(maxdepth = 5, minsplit = 15))
  bagging.predict <- predict(bagging,test_orig)

  levels(bagging.predict) <- c(FALSE,TRUE)
  bagging.predict <- as.logical(bagging.predict)
  bagging.predict <- as.integer(bagging.predict)
  bg = data.frame(bagging.predict)
  
  res_roc <- roc(test_orig$is_spam, bagging.predict)
  fit_auc <- round(auc(res_roc), 3)
  auc_bagging[i]<-fit_auc

  # Load Library or packages
  library(caret)
  # Create Confusion Matrix
  confusionMatrix(data=factor(bagging.predict),
                  reference=factor(test$is_spam),
                  positive='1')

  ######################### RANDOM FOREST #####################

  #install.packages('randomForest')
  library(randomForest)

  randomForest.fit <- randomForest(as.factor(is_spam) ~ .,
                                          data=train_orig,
                                          importance=TRUE,
                                          ntree=1000)
  
  randomForest.pred <- predict(randomForest.fit, test_orig)
  randomForest.pred<-as.numeric(randomForest.pred)
  randomForest.pred <- randomForest.pred - 1
  
  res_roc <- roc(test_orig$is_spam, randomForest.pred)
  fit_auc <- round(auc(res_roc), 3)
  auc_randomforest[i]<-fit_auc

  misClasificError <- mean(randomForest.pred != as.factor(test_orig$is_spam))
  print(paste('Accuracy RANDOM FOREST test', 1 - misClasificError))

  ########################### XGBoost ###########################

  #install.packages("xgboost")
  require(xgboost)

  train_xgb <- subset(train_orig , select=-c(is_spam))
  test_xgb <- subset(test_orig , select=-c(is_spam))

  xgb.fit <- xgboost(data = as.matrix(train_xgb), label = train_orig$is_spam, max.depth = 6, eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")
  xgb.pred <- predict(xgb.fit, as.matrix(train_xgb))

  res_roc <- roc(train_orig$is_spam,
                 xgb.pred,
                 percent = TRUE,
                 plot = TRUE)

  xgb.threshold <- coords(res_roc, "best", ret = "threshold")

  xgb.pred.test <- predict(xgb.fit, as.matrix(test_xgb))
  xgb.pred.test[xgb.pred.test < xgb.threshold] <- 0
  xgb.pred.test[xgb.pred.test >= xgb.threshold] <- 1
  
  res_roc <- roc(test_orig$is_spam, xgb.pred.test)
  fit_auc <- round(auc(res_roc), 3)
  auc_xgboost[i]<-fit_auc
  misClasificError <- mean(xgb.pred.test != as.factor(test_orig$is_spam))
  print(paste('Accuracy XGBOOST test', 1 - misClasificError))
  
  dev.off()
}

dir <- "D:\\"
pdfPath <- file.path(dir, "mojpdf3.pdf")
pdf(pdfPath)

df = data.frame(auc_supermethod, auc_supermethod2, auc_bagging, auc_randomforest, auc_xgboost )
boxplot(df)

dev.off()
