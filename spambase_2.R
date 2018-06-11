library(pROC)
library(lattice)
library(boot)
library(rpart)
library(xgboost)
library(party)
library(randomForest)

data <- read.csv2("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE, sep=",")

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

for(i in 1:55) {
  data[[i]] <-  as.double(levels(data[[i]]))[data[[i]]]
}

data$word_binary_free <- ifelse(data$word_freq_free > 0, TRUE,FALSE)
data$word_binary_receive <- ifelse(data$word_freq_receive > 0, TRUE,FALSE)
data$word_binary_money <- ifelse(data$word_freq_money > 0, TRUE,FALSE)

data$is_spam<-ifelse(data$is_spam == 1, TRUE,FALSE)

set.seed(1234)

dir <- "D:\\"
pdfPath <- file.path(dir, "mojpdf2.pdf")
pdf(pdfPath)

auc_rtree <- c()
auc_ctree <- c()
auc_randomforest <- c()
auc_xgboost <- c()

###################### BOOTSTRAPING #######################
for(i in 1:10) {
  sample <- sample(nrow(data), size = nrow(data), replace = TRUE)
  sample <- unique(sample)
  
  test <- data[-sample, ]
  train <- data[sample, ]
  
  ###################### RPART Decision tree #######################
  fit <- rpart(as.factor(is_spam)~., method="class", data=train)
  
  prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  
  res <- predict(fit, train)
  res_roc <- roc(train$is_spam, res[,2])
  threshold <- coords(res_roc, "best", ret = "threshold")

  res <- predict(fit, test)
  res[res < threshold] <- 0
  res[res >= threshold] <- 1
  res[,1]<-ifelse(res[,2] < 0.5, FALSE,TRUE)
  
  fit_roc <- roc(test$is_spam, res[,1])
  fit_auc <- round(auc(fit_roc), 3)

  auc_rtree[i]<-fit_auc
  
  coords(fit_roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy"))
  
  plot(fit_roc, col = "blue")
  
  plot(fit, uniform=TRUE, main="Classification Tree for words")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  
  ###################### CTREE Decision tree #######################
  fit_ctree = ctree(is_spam~., data = train)
  plot(fit_ctree, main="Conditional Inference Tree", type="simple")

  res <- predict(fit, train)
  res_roc <- roc(train$is_spam, res[,2])
  threshold <- coords(res_roc, "best", ret = "threshold")

  res_ctree <- predict(fit_ctree, test)
  res_ctree[res_ctree < threshold] <- 0
  res_ctree[res_ctree >= threshold] <- 1

  fit_roc <- roc(test$is_spam, res_ctree[,1])
  fit_auc <- round(auc(fit_roc), 3)
  
  auc_ctree[i]<-fit_auc
  
  plot(fit_roc, col = "blue")
  
  ####################### RANDOM FOREST ##############################
  output.forest <- randomForest(as.factor(is_spam)~., data = train)
  print(output.forest)
  importance(output.forest)
  
  res_randomforest <- predict(output.forest, test)
  res_randomforest<-as.integer(as.logical(res_randomforest))

  fit_roc <- roc(test$is_spam, res_randomforest)
  fit_auc <- round(auc(fit_roc), 3)
  auc_randomforest[i] <- fit_auc
  
  ######################### XGBOOST ########################################
  train_xgb <- subset(train , select=-c(is_spam))
  test_xgb <- subset(test , select=-c(is_spam))
  
  xgb.fit <- xgboost(data = as.matrix(train_xgb), label = train$is_spam, max.depth = 6, eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")
  xgb.pred <- predict(xgb.fit, as.matrix(train_xgb))
  
  fit_roc <- roc(train$is_spam, xgb.pred)
  fit_auc <- round(auc(fit_roc), 3)
  auc_xgboost[i]<-fit_auc
  
  coords(fit_roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy"))
  }

df = data.frame(auc_rtree, auc_ctree, auc_randomforest, auc_xgboost)
boxplot(df)

dev.off()
