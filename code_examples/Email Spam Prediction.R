uci_spam<-readRDS("~/Documents/Predict 454/Assignment_04/uci_spambase.RData")
View(uci_spam)

str(uci_spam)

# install packages and load libraries

library(tidyverse)
library(dplyr)
install.packages("DataExplorer")
library(DataExplorer)
install.packages("stargazer")
library(stargazer)
library(lattice)
library(ggplot2)
library(gridExtra)
library(frequency)




###########################################################################
# Notes:
###########################################################################
# (1) u, train, and test are not modeling variables
# (2) u is a uniform random number
# (3) train is the training sample flag (75%)
# (4) test is the testing sample flag (25%)


# Check flag assignments;
table(uci_spam$train,uci_spam$test)

# Perform quality check on whole data set
attach(uci_spam)
summary_table<-introduce(uci_spam)
View(summary_table)
# 4601 rows
# 0 missing values
# 61 variables

summary(uci_spam)
stargazer(uci_spam, digits=2,type = "html", out = "spam_descriptive.htm")

# subset data for different data checks
subset<-uci_spam[c(1:54)]
subset2<-uci_spam[c(55:57)]
summary(subset2)
stargazer(subset2, type = "html",out="spam_descriptive.doc")

# convert spam to binary factor
uci_spam$spam<-as.factor(uci_spam$spam)
uci_spam$spam_label <- factor(uci_spam$spam, levels=c(0, 1),
                              labels=c("Not Spam", "Spam"))
summary(uci_spam$spam)

# bar plot of spam
ggplot(uci_spam, aes(x=as.factor(spam) )) + geom_bar()

p2<-ggplot(uci_spam, aes(x=capital_run_length_longest, color=spam_label)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
p3<-ggplot(uci_spam, aes(x=capital_run_length_average, color=spam_label)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
p4<-ggplot(uci_spam, aes(x=capital_run_length_total, color=spam_label)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
p2
p3
p4
grid.arrange(p2,p3,p4, ncol=3)

# all capital run length variables could be transformed but don't need to be
# the models created are flexible and do not rely on normality assumptions
p2<-ggplot(uci_spam, aes(x=log(capital_run_length_longest), color=spam_label)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
p3<-ggplot(uci_spam, aes(x=log(capital_run_length_average), color=spam_label)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
p4<-ggplot(uci_spam, aes(x=log(capital_run_length_total), color=spam_label)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
grid.arrange(p2,p3,p4, ncol=3)



###########################################################################
# split data into training and test set for EDA
spam_train <- uci_spam[uci_spam$train==1,]
spam_test <- uci_spam[uci_spam$test==1,]

# subset data to remove unnecessary columns
sub_train <- spam_train[c( -59, -60, -61, -62)]
sub_test<-spam_test[c(-59, -60, -61, -62)]
View(sub_train)
str(sub_train)

colMax <- function(data) sapply(data, max, na.rm = TRUE)
max_values<-colMax(sub_train[,1:54])

#par(mar=c(10,4,4,4))
#barplot(max_values, main="Maximum Values for Frequency Attributes", las=2, ylim = c(0,50),col="deepskyblue4", cex.names = 0.85, cex.axis = 0.8)

library(plyr)
zero_freq <- ldply(subset2, function(c) (sum(c==0))/length(c))
zero_freq

library(scales)
p <- ggplot(zero_freq, aes(x=.id, y=V1)) + 
  geom_bar(stat="identity", fill="deepskyblue4") +
  xlab("Frequency Attributes") + ylab("Percent of values = 0") +
  scale_y_continuous(labels=percent)
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# filter data for only spam and re-examine max. values
spam_subset<-sub_train %>% filter(spam == 1)
spam_max_values<-ldply(spam_subset[,1:54], function(c) max(c))
spam_max_values

p <- ggplot(spam_max_values, aes(x=.id, y=V1)) + 
  geom_bar(stat="identity", fill="deepskyblue4") +
  xlab("Frequency Attributes") + ylab("Max. Number of Occurrences")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Word and Character Frequency for Spam e-mails")


# compare to non-spam occurrences
spam_subset2<-sub_train %>% filter(spam == 0)
spam_max_values2<-ldply(spam_subset2[,1:54], function(c) max(c))
spam_max_values2

p1 <- ggplot(spam_max_values2, aes(x=.id, y=V1)) + 
  geom_bar(stat="identity", fill="deepskyblue4") +
  xlab("Frequency Attributes") + ylab("Max. Number of Occurrences")
p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Word and Character Frequency for Non-Spam e-mails")


# examine distributions after "removing" zeros
smaller <- sub_train %>% 
  filter(`char_freq_!` > 0)
ggplot(data = smaller, mapping = aes(x = `char_freq_!`)) +
  geom_histogram(binwidth = 1)


# barplots of char/word frequencies by spam vs non-spam
p1<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_george, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'George' \n in spam vs. non-spam emails")

p2<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_85, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum")+
  ggtitle("Sum of the frequency of '85' \n in spam vs. non-spam emails")

p3<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_edu, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'edu' \n in spam vs. non-spam emails")

p4<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_re, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 're' \n in spam vs. non-spam emails")


p6<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_hp, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'hp' \n in spam vs. non-spam emails")

p7<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_project, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'project' \n in spam vs. non-spam emails")

#p8<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_you, fill=spam_label)) +
#  geom_bar(stat="identity") +
#  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
#  ggtitle("Sum of the frequency of 'you' \n in spam vs. non-spam emails")

grid.arrange(p1,p2,p3,p4,p6,p7, ncol=3)

p1<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_3d, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum")+
  ggtitle("Sum of the frequency of the word '3d' \n in spam vs. non-spam emails")

p2<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_free, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'free' \n in spam vs. non-spam emails")

p3<-ggplot(data=spam_train, aes(x=spam_label, y=capital_run_length_average, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("capital_run_length_average \n in spam vs. non-spam emails")

p4<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_font, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'font' \n in spam vs. non-spam emails")

p5<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_credit, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'credit' \n in spam vs. non-spam emails")

p6<-ggplot(data=spam_train, aes(x=spam_label, y=word_freq_money, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("Frequency Sum") +
  ggtitle("Sum of the frequency of 'money' \n in spam vs. non-spam emails")

grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)

# character frequencies
p1<-ggplot(data=spam_train, aes(x=spam_label, y=`char_freq_#`, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("'#' frequency in spam vs. \n non-spam emails")
p1

p2<-ggplot(data=spam_train, aes(x=spam_label, y=`char_freq_[`, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("'[' frequency in spam vs. \n non-spam emails")
p2

p3<-ggplot(data=spam_train, aes(x=spam_label, y=`char_freq_!`, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("'!' frequency in spam vs. \n non-spam emails")
p3

p4<-ggplot(data=spam_train, aes(x=spam_label, y=`char_freq_$`, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("'$' frequency in spam vs. \n non-spam emails")
p4

p5<-ggplot(data=spam_train, aes(x=spam_label, y=`char_freq_;`, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("';' frequency in spam vs. \n non-spam emails")
p5

p6<-ggplot(data=spam_train, aes(x=spam_label, y=`char_freq_(`, fill=spam_label)) +
  geom_bar(stat="identity") +
  xlab("Spam vs. Non-Spam") + ylab("") +
  ggtitle("'(' frequency in spam vs. \n non-spam emails")
p6

grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3)


# correlations

library(corrplot)

pdf(file = "spam_correlations.pdf", width = 8.5, height = 11)
oldPar = par(mar = c(3.1, 3, 1, 1))

#corrplot.mixed(cor(sub_train), lower.col = "black", number.cex = .7,tl.cex = 0.5, tl.col="black")
#plot_correlation(sub_train, type = 'continuous')
corrplot(cor(sub_train), method = "color", type = "upper", 
         sig.level = 0.3, insig = "blank", tl.col = "darkblue",tl.cex=0.7)

par(oldPar)
dev.off()

cor(sub_train)

xyplot(jitter(word_freq_hp) ~ jitter(word_freq_hpl) | spam, 
       data = sub_train,
       strip=function(...) strip.default(..., style=1),
       main="Word Frequencies by Spam Type",
       group = spam)

xyplot(jitter(word_freq_hp) ~ jitter(word_freq_650) | spam, 
       data = sub_train,
       strip=function(...) strip.default(..., style=1),
       main="Word Frequencies by Spam Type",
       group = spam)

xyplot(jitter(word_freq_hp) ~ jitter(word_freq_lab) | spam, 
       data = sub_train,
       strip=function(...) strip.default(..., style=1),
       main="Word Frequencies by Spam Type",
       group = spam)

xyplot(jitter(word_freq_hp) ~ jitter(word_freq_labs) | spam, 
       data = sub_train,
       strip=function(...) strip.default(..., style=1),
       main="Word Frequencies by Spam Type",
       group = spam)

ggplot(sub_train, aes(word_freq_hp, word_freq_labs, color = spam)) + 
  geom_jitter() +
  ggtitle("Frequency of 'HP' vs. 'labs' by spam") +
  theme_light()



# exploratory classification tree
exp_tree<-rpart(spam~.,data = sub_train, method = "class")
exp_tree
rpart.plot(exp_tree, extra = 103)



###########################################################################
##### MODEL CREATION #####
attach(sub_test)
attach(sub_train)
options(scipen = 999)
# Model 1: Logistic Regression using backward selection
log_1b<-glm(spam~., data = sub_train, family = "binomial")
summary(log_1b)
backwards = step(log_1b)
formula(backwards)
summary(backwards)
summary(backwards)$coef

stargazer(backwards,type = "text")

glm.probs<-predict(log_1b, type = "response")
glm.test.probs<-predict(log_1b,sub_test,type = "response")

glm.train.probs<-predict(log_1b,sub_train,type="response")
t1<-table(sub_train$spam, glm.train.probs > 0.5)


t1<-table(sub_test$spam, glm.test.probs > 0.5)
accuracy_Test <- sum(diag(t1)) / sum(t1)
accuracy_Test
# 93.115%

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

prec <- precision(t1)
prec # = 91.63%
rec <- recall(t1)
rec # = 90.4%


#interpreting coefficients
invlogit<-function(x){
  1/(1+exp(-x))
}
round(invlogit(log_1b$coefficients), digits = 5)



# simple logistic regression model
glm1<-glm(spam~word_freq_our+word_freq_over+word_freq_remove+
            word_freq_internet+word_freq_order+word_freq_free+
            word_freq_business+word_freq_your+word_freq_000+
            word_freq_money+word_freq_hp+word_freq_hpl+
            word_freq_george+word_freq_415+word_freq_85+
            word_freq_technology+word_freq_meeting+
            word_freq_project+word_freq_re+word_freq_edu+
            capital_run_length_longest+capital_run_length_total,
          data = sub_train, family = "binomial")

summary(glm1)
backwards = step(glm1)
formula(backwards)
summary(backwards)

glm.probs<-predict(glm1, type = "response")
glm.test.probs<-predict(glm1,sub_test,type = "response")
t1<-table(sub_test$spam, glm.test.probs > 0.5)
t1
accuracy_Test <- sum(diag(t1)) / sum(t1)
accuracy_Test

prec <- precision(t1)
prec # = 0.9054374
rec <- recall(t1)
rec # = 0.8549107


# Model 2: Classification Trees
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

tree1<-rpart(spam~.,data = sub_train, method = "class")
rpart.plot(tree1, extra = 106)

tree.probs<-predict(tree1, sub_test, type = 'class')
table_mat <- table(sub_test$spam, tree.probs)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
# 0.904475



# Model 3: Support Vector Machine
tr_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# repeated cross validation with 10 re-sampling iterations, repeated 3 times
set.seed(3233)
# to repeat model creation

svm_Linear <- train(spam ~., data = sub_train, method = "svmLinear",
                    trControl=tr_ctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear

test_pred <- predict(svm_Linear, newdata = sub_test)
test_pred
confusionMatrix(test_pred, sub_test$spam)
# Accuracy = 93.03%

# tune the model
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)
svm_Linear_Grid <- train(spam ~., data = sub_train, method = "svmLinear",
                           trControl=tr_ctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = sub_test)
test_pred_grid
confusionMatrix(test_pred_grid,sub_test$spam)
# Accuracy = 93.03%


# SVM with Non-linear Kernel
set.seed(3233)
svm_Radial <- train(spam ~., data = sub_train, method = "svmRadial",
                    trControl=tr_ctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial
plot(svm_Radial)

test_pred_Radial <- predict(svm_Radial, newdata = sub_test)
confusionMatrix(test_pred_Radial, sub_test$spam)
# Accuracy = 93.55%

# tune the radial model
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                   1, 1.5, 2,5))
set.seed(3233)
svm_Radial_Grid <- train(spam ~., data = sub_train, method = "svmRadial",
                           trControl=tr_ctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_radial,
                           tuneLength = 10)
svm_Radial_Grid
plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = sub_test)
confusionMatrix(test_pred_Radial_Grid, sub_test$spam)


# simple svm model
library(class)
library(kernlab)
svm_model <- svm(spam~ ., data=sub_train, method="C-classification", kernel="linear")
svm_model
plot(svm_model, sub_train)

pred_train <-predict(svm_model,sub_train)
mean(pred_train==sub_train$spam)
# 0.9366095
#test set predictions
pred_test <-predict(svm_model,sub_test)
mean(pred_test==sub_test$spam)
# 0.9302926

# tune simple model
tune.out <- tune(svm, spam~., data = sub_train, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)




# Model 4: Random Forest
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)

set.seed(123)

# default RF model
rf1 <- randomForest(formula = as.factor(spam) ~ ., data= sub_train, importance=TRUE, ntree=100)
rf1
plot(rf1)

importance(rf1)
varImpPlot(rf1)
old.par <- par(mar = c(1, 1, 1, 1))
par(old.par)
varImpPlot(rf1,  
           sort = T,
           n.var=20,
           main="Top 20 - Variable Importance")

varImpPlot(rf1, useModel=TRUE, sort=TRUE)
plot(varImpPlot(rf1), main = "Random Forest")


rf1.pred = predict(rf1, sub_test)
plot(margin(rf1, sub_test$spam))

# Create Confusion Matrix
confusionMatrix(rf1.pred,reference = sub_test$spam)
# 0.9552

rf_tab<-table(sub_test$spam, rf1.pred)
precision(rf_tab)





