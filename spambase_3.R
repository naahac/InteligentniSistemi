library(pROC)
library(lattice)
library(boot)
library(rpart)
library(e1071)
install.packages('neuralnet')
library(neuralnet)

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

set.seed(1000) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

#SVM
svm.model <- svm(is_spam~ ., data = train, type="C-classification")
svm.pred <- predict(svm.model, train)

train["svm"] <- svm.pred
train$svm<-ifelse(train$svm == 1, TRUE,FALSE)
misClasificError <- mean(svm.pred != train$is_spam)
print(paste('Accuracy',1-misClasificError))
table(pred = svm.pred, true = train$is_spam)

#RPART
rpart.model <- rpart(is_spam ~ ., data = train)
rpart.pred <- predict(rpart.model, train, type = "vector")
rpart.pred[rpart.pred < 0.5] <- 0
rpart.pred[rpart.pred >= 0.5] <- 1
train["rpart"] <- rpart.pred
misClasificError <- mean(rpart.pred != train$is_spam)
print(paste('Accuracy',1-misClasificError))

table(pred = rpart.pred, true = train$is_spam)

#GLM
fit <- glm(is_spam ~ ., data=train, family=binomial)
res = predict(fit, test, type="response")
res[res < 0.5] <- 0
res[res >= 0.5] <- 1

hist(res, main="Predictions")

misClasificError <- mean(res != test$is_spam)
print(paste('Accuracy',1-misClasificError))

