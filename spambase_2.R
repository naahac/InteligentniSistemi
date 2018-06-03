library(pROC)
library(lattice)
library(boot)
library(rpart)
library(xgboost)
library(party)

data <- read.csv2("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE, sep=",")

colnames(data) <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", "word_freq_our", "word_freq_over", "word_freq_remove", 
                    "word_freq_internet", "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", "word_freq_people",
                    "word_freq_report", "word_freq_addresses", "word_freq_free", "word_freq_business", "word_freq_email", "word_freq_you",
                    "word_freq_credit", "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", "word_freq_hp", "word_freq_hpl",
                    "word_freq_george", "word_freq_650", "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857", "word_freq_data",
                    "word_freq_415", "word_freq_85", "word_freq_technology", "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct",
                    "word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", "word_freq_table",
                    "word_freq_conference", "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$", "char_freq_#", "capital_run_length_average",
                    "capital_run_length_longest", "capital_run_length_total", "is_spam")

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

for(i in 1:10) {
  sample <- sample(nrow(data), size = nrow(data), replace = TRUE)
  sample <- unique(sample)
  
  test <- data[-sample, ]
  train <- data[sample, ]
  
  
  
  #odloèitveno drevo 1
  fit <- rpart(is_spam~., method="class", data=train)
  summary(fit)
  
  res <- predict(fit, test)
  res[res < 0.5] <- 0
  res[res >= 0.5] <- 1
  
  res[,1]<-ifelse(res[,2] < 0.5, FALSE,TRUE)
  
  
  fit_roc <- roc(test$is_spam, res[,1])
  fit_auc <- round(auc(fit_roc), 3)
  fit_auc
  
  plot(fit_roc, col = "blue")
  
  printcp(fit) # display the results
  plotcp(fit) # visualize cross-validation results
  summary(fit) # detailed summary of splits
  
  # plot tree
  plot(fit, uniform=TRUE, main="Classification Tree for words")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  
  #odloèitveno drevo 2
  (ct = ctree(is_spam~., data = test))
  plot(ct, main="Conditional Inference Tree", type="simple")
  
  boxplot(is_spam~.,data=test)
  
  bstSparse <- xgboost(data = train$word_freq_make, label = "test", max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
}

dev.off()
