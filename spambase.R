library(pROC)
library(lattice)

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

dir <- "D:\\"
pdfPath <- file.path(dir, "mojpdf.pdf")
pdf(pdfPath)

for(i in 1:54) {
  hist(data[[i]], main=sprintf("Histogram of \"%s\"", colnames(data)[i]), xlab=colnames(data)[i], xlim=c(0,50), border="black", col="red", las=1)
}

roc1 <- roc(data$is_spam, data$capital_run_length_average, percent=TRUE, plot=TRUE, col='blue', auc = )
auc1 <- round(auc(roc1), 3)

word_freq_make_roc <- roc(data$is_spam, data$word_freq_make, percent=TRUE, plot=TRUE)
word_freq_make_auc <- round(auc(word_freq_make_roc), 3)

word_freq_address_roc <- roc(data$is_spam, data$word_freq_address, percent=TRUE, plot=TRUE)
word_freq_address_auc <- round(auc(word_freq_address_roc), 3)

word_freq_all_roc <- roc(data$is_spam, data$word_freq_all, percent=TRUE, plot=TRUE)
word_freq_all_auc <- round(auc(word_freq_all_roc), 3)

word_freq_3d_roc <- roc(data$is_spam, data$word_freq_3d, percent=TRUE, plot=TRUE)
word_freq_3d_auc <- round(auc(word_freq_3d_roc), 3)

word_freq_our_roc <- roc(data$is_spam, data$word_freq_our, percent=TRUE, plot=TRUE)
word_freq_our_auc <- round(auc(word_freq_our_roc), 3)

word_freq_over_roc <- roc(data$is_spam, data$word_freq_over, percent=TRUE, plot=TRUE)
word_freq_over_auc <- round(auc(word_freq_over_roc), 3)

word_freq_remove_roc <- roc(data$is_spam, data$word_freq_remove, percent=TRUE, plot=TRUE)
word_freq_remove_auc <- round(auc(word_freq_remove_roc), 3)

word_freq_internet_roc <- roc(data$is_spam, data$word_freq_internet, percent=TRUE, plot=TRUE)
word_freq_internet_auc <- round(auc(word_freq_internet_roc), 3)

word_freq_order_roc <- roc(data$is_spam, data$word_freq_order, percent=TRUE, plot=TRUE)
word_freq_order_auc <- round(auc(word_freq_order_roc), 3)

word_freq_mail_roc <- roc(data$is_spam, data$word_freq_mail, percent=TRUE, plot=TRUE)
word_freq_mail_auc <- round(auc(word_freq_mail_roc), 3)

lines(word_freq_make_roc, col="purple")
lines(word_freq_address_roc, col="blue")
lines(word_freq_all_roc, col="red")
lines(word_freq_3d_roc, col="green")
lines(word_freq_our_roc, col="black")
lines(word_freq_over_roc, col="brown")
lines(word_freq_remove_roc, col="orange")
legend("bottomright", 
       legend=c(paste("make auc", word_freq_make_auc),
                paste("address auc", word_freq_address_auc),
                paste("all auc", word_freq_all_auc),
                paste("3d auc", word_freq_3d_auc),
                paste("our auc", word_freq_our_auc),
                paste("over auc", word_freq_over_auc),
                paste("remove auc", word_freq_remove_auc),
                paste("internet auc", word_freq_internet_auc),
                paste("order auc", word_freq_order_auc),
                paste("mail auc", word_freq_mail_auc)), 
       col=c("purple", "blue", "red", "green", "black", "brown", "orange"), lwd=2);

xyplot(word_freq_free ~ word_freq_3d, data)

roc(data$is_spam, data$capital_run_length_total, percent=TRUE, plot=TRUE, col='blue')
roc(data$is_spam, data$capital_run_length_average, percent=TRUE, plot=TRUE, col='red')

set.seed(1000) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.70*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

#Prediction
fit <- glm(is_spam ~ ., data=train, family=binomial)

summary(fit)
confint(fit)# 95% CI for the coefficients
#exp(coef(fit))# exponentiated coefficients
#exp(confint(fit)) # 95% CI for exponentiated coefficients

res = predict(fit, test, type="response")
res[res < 0.5] <- 0
res[res >= 0.5] <- 1

res_roc <- roc(test$is_spam, res, percent=TRUE, plot=TRUE)
res_auc <- round(auc(res_roc), 3)
coords(res_roc, "best", ret = "threshold")

dev.off();

hist(res, main="Predictions")

misClasificError <- mean(res != test$is_spam)
print(paste('Accuracy',1-misClasificError))

warnings()



