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

data$word_freq_make <-  as.double(levels(data$word_freq_make))[data$word_freq_make]
data$word_freq_address <-  as.double(levels(data$word_freq_address))[data$word_freq_address]
data$word_freq_all <-  as.double(levels(data$word_freq_all))[data$word_freq_all]
data$word_freq_3d <-  as.double(levels(data$word_freq_3d))[data$word_freq_3d]
data$word_freq_our <-  as.double(levels(data$word_freq_our))[data$word_freq_our]
data$word_freq_over <-  as.double(levels(data$word_freq_over))[data$word_freq_over]
data$word_freq_remove <-  as.double(levels(data$word_freq_remove))[data$word_freq_remove]
data$word_freq_internet <-  as.double(levels(data$word_freq_internet))[data$word_freq_internet]
data$word_freq_order <-  as.double(levels(data$word_freq_order))[data$word_freq_order]
data$word_freq_mail <-  as.double(levels(data$word_freq_mail))[data$word_freq_mail]
data$word_freq_receive <-  as.double(levels(data$word_freq_receive))[data$word_freq_receive]
data$word_freq_will <-  as.double(levels(data$word_freq_will))[data$word_freq_will]
data$word_freq_people <-  as.double(levels(data$word_freq_people))[data$word_freq_people]
data$word_freq_report <-  as.double(levels(data$word_freq_report))[data$word_freq_report]
data$word_freq_addresses <-  as.double(levels(data$word_freq_addresses))[data$word_freq_addresses]
data$word_freq_free <-  as.double(levels(data$word_freq_free))[data$word_freq_free]
data$word_freq_business <-  as.double(levels(data$word_freq_business))[data$word_freq_business]
data$word_freq_email <-  as.double(levels(data$word_freq_email))[data$word_freq_email]
data$word_freq_you <-  as.double(levels(data$word_freq_you))[data$word_freq_you]
data$word_freq_credit <-  as.double(levels(data$word_freq_credit))[data$word_freq_credit]
data$word_freq_your <-  as.double(levels(data$word_freq_your))[data$word_freq_your]
data$word_freq_font <-  as.double(levels(data$word_freq_font))[data$word_freq_font]
data$word_freq_000 <-  as.double(levels(data$word_freq_000))[data$word_freq_000]
data$word_freq_money <-  as.double(levels(data$word_freq_money))[data$word_freq_money]
data$word_freq_hp <-  as.double(levels(data$word_freq_hp))[data$word_freq_hp]
data$word_freq_hpl <-  as.double(levels(data$word_freq_hpl))[data$word_freq_hpl]
data$word_freq_george <-  as.double(levels(data$word_freq_george))[data$word_freq_george]
data$word_freq_650 <-  as.double(levels(data$word_freq_650))[data$word_freq_650]
data$word_freq_lab <-  as.double(levels(data$word_freq_lab))[data$word_freq_lab]
data$word_freq_labs <-  as.double(levels(data$word_freq_labs))[data$word_freq_labs]
data$word_freq_telnet <-  as.double(levels(data$word_freq_telnet))[data$word_freq_telnet]
data$word_freq_857 <-  as.double(levels(data$word_freq_857))[data$word_freq_857]
data$word_freq_data <-  as.double(levels(data$word_freq_data))[data$word_freq_data]
data$word_freq_415 <-  as.double(levels(data$word_freq_415))[data$word_freq_415]
data$word_freq_85 <-  as.double(levels(data$word_freq_85))[data$word_freq_85]
data$word_freq_technology <-  as.double(levels(data$word_freq_technology))[data$word_freq_technology]
data$word_freq_1999 <-  as.double(levels(data$word_freq_1999))[data$word_freq_1999]
data$word_freq_parts <-  as.double(levels(data$word_freq_parts))[data$word_freq_parts]
data$word_freq_pm <-  as.double(levels(data$word_freq_pm))[data$word_freq_pm]
data$word_freq_direct <-  as.double(levels(data$word_freq_direct))[data$word_freq_direct]
data$word_freq_cs <-  as.double(levels(data$word_freq_cs))[data$word_freq_cs]
data$word_freq_meeting <-  as.double(levels(data$word_freq_meeting))[data$word_freq_meeting]
data$word_freq_original <-  as.double(levels(data$word_freq_original))[data$word_freq_original]
data$word_freq_project <-  as.double(levels(data$word_freq_project))[data$word_freq_project]
data$word_freq_re <-  as.double(levels(data$word_freq_re))[data$word_freq_re]
data$word_freq_edu <-  as.double(levels(data$word_freq_edu))[data$word_freq_edu]
data$word_freq_table <-  as.double(levels(data$word_freq_table))[data$word_freq_table]
data$word_freq_conference <-  as.double(levels(data$word_freq_conference))[data$word_freq_conference]
data$"char_freq_;" <-  as.double(levels(data$"char_freq_;"))[data$"char_freq_;"]
data$"char_freq_(" <-  as.double(levels(data$"char_freq_("))[data$"char_freq_("]
data$"char_freq_[" <-  as.double(levels(data$"char_freq_["))[data$"char_freq_["]
data$"char_freq_!" <-  as.double(levels(data$"char_freq_!"))[data$"char_freq_!"]
data$"char_freq_$" <-  as.double(levels(data$"char_freq_$"))[data$"char_freq_$"]
data$"char_freq_#" <-  as.double(levels(data$"char_freq_#"))[data$"char_freq_#"]
data$capital_run_length_average <-  as.double(levels(data$capital_run_length_average))[data$capital_run_length_average]
#data$capital_run_length_longest <-  as.double(levels(data$capital_run_length_longest))[data$capital_run_length_longest]

data$is_spam<-ifelse(data$is_spam == 1, TRUE,FALSE)

dir <- "D:\\"
pdfPath <- file.path(dir, "mojpdf.pdf")
pdf(pdfPath)

hist(data$word_freq_make, main="Frequency of word make", xlab="word make", border="black", col="red", las=1)
hist(data$word_freq_address, main="Frequency of word address", xlab="word address", border="black", col="red", las=1)
hist(data$word_freq_all, main="Frequency of word all", xlab="word all", border="black", col="red", las=1)
hist(data$word_freq_3d, main="Frequency of word 3d", xlab="word 3d", border="black", col="red", las=1)
hist(data$word_freq_our, main="Frequency of word our", xlab="word our", border="black", col="red", las=1)
hist(data$word_freq_over, main="Frequency of word over", xlab="word over", border="black", col="red", las=1)
hist(data$word_freq_remove, main="Frequency of word remove", xlab="word remove", border="black", col="red", las=1)
hist(data$word_freq_internet, main="Frequency of word internet", xlab="word internet", border="black", col="red", las=1)
hist(data$word_freq_order, main="Frequency of word order", xlab="word order", border="black", col="red", las=1)
hist(data$word_freq_mail, main="Frequency of word mail", xlab="word mail", border="black", col="red", las=1)
hist(data$word_freq_receive, main="Frequency of word receive", xlab="word receive", border="black", col="red", las=1)
hist(data$word_freq_will, main="Frequency of word will", xlab="word will", border="black", col="red", las=1)
hist(data$word_freq_people, main="Frequency of word people", xlab="word people", border="black", col="red", las=1)
hist(data$word_freq_report, main="Frequency of word report", xlab="word report", border="black", col="red", las=1)
hist(data$word_freq_addresses, main="Frequency of word addresses", xlab="word addresses", border="black", col="red", las=1)
hist(data$word_freq_free, main="Frequency of word free", xlab="word free", border="black", col="red", las=1)

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


dev.off();

roc(data$is_spam, data$capital_run_length_total, percent=TRUE, plot=TRUE, col='blue')
roc(data$is_spam, data$capital_run_length_average, percent=TRUE, plot=TRUE, col='red')

# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

#Prediction
fit <- glm(is_spam~., data=train, family=binomial)
summary(fit)
confint(fit)# 95% CI for the coefficients
#exp(coef(fit))# exponentiated coefficients
#exp(confint(fit)) # 95% CI for exponentiated coefficients

res = predict(fit, test, type="response")
res[res < 0.5] <- 0
res[res >= 0.5] <- 1

hist(res, main="Predictions")

misClasificError <- mean(res != test$is_spam)
print(paste('Accuracy',1-misClasificError))



