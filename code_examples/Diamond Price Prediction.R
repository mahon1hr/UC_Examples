print(str(two_months_salary))

##install packages needed for analysis
install.packages("tidyverse")
install.packages("padr")
install.packages("DataExplorer")
install.packages("stargazer",dependencies=TRUE)
install.packages("qwraps2")
install.packages("magrittr")
install.packages("ggplot")
install.packages("wesanderson")
install.packages("RColorBrewer")
install.packages("rpart.utils")
install.packages("mlbench")
install.packages("randomForest")

library(wesanderson)
library(MASS)
library(tidyverse)
library(padr)
library(DataExplorer)
library(stargazer)
library(lattice)
library(qwraps2)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(mlbench) #For feature engineering
library(randomForest)
library(tree)
library(gbm)
library(nnet)
library(pls)
library(ISLR)
library(caret)




##-----------------------------------------------------------------------##
## Data Quality Check
options(qwraps2_markup = "markdown")

summary(two_months_salary)
attach(two_months_salary)

summary_table<-introduce(two_months_salary)
stargazer(summary_table,title = "Summary Table",summary=FALSE,type="html",flip = TRUE,out = "summary.doc")

two_months_salary %>% head
plot_str(two_months_salary)
plot_missing(two_months_salary)
p1<-plot_histogram(two_months_salary)
p2<-plot_boxplot(two_months_salary, by="cut", geom_boxplot_args = list("outlier.color" = "red"))
plot_density(two_months_salary)
plot_correlation(two_months_salary, type = 'continuous')
plot_bar(two_months_salary)
create_report(two_months_salary)


##check for outliers
par(mfrow=c(1,1))
outlier_values <- boxplot.stats(clarity)$out  # outlier values.
boxplot.stats(two_months_salary$carat, do.conf = TRUE, do.out = TRUE)
carat_outliers<-two_months_salary[which(two_months_salary$carat %in% outlier_values),]
carat_outliers$outlier_column<-("carat_outliers")
price_outliers<-two_months_salary[which(two_months_salary$price %in% outlier_values),]
price_outliers$outlier_column<-("price_outliers")
color_outliers<-two_months_salary[which(two_months_salary$color %in% outlier_values),]
color_outliers$outlier_column<-("color_outliers")
clarity_outliers<-two_months_salary[which(two_months_salary$clarity %in% outlier_values),]
clarity_outliers$outlier_column<-("clarity_outliers")
outlier_table<-rbind(carat_outliers,price_outliers,color_outliers,clarity_outliers)
#print table of outliers
stargazer(outlier_table,title = "Identified Outliers",summary=FALSE,type="html",out = "outlier.doc")


##check data integrity - does the row of values make sense together
##check integrity using scatterplots of price and carat across cut, color, clarity, channel, store
##e.g. if price is high but carat, color, clarity, and cut are all poor or some variation of
#these factors are less desirable

##scatter plots
p1<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = carat, y = price, color = cut)) + 
  theme(legend.position="bottom") +
  scale_color_brewer(palette="Paired")

p2<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = carat, y = price, color = color_factor)) + 
  theme(legend.position="bottom") +
  scale_color_brewer(palette="Paired")

p3<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = carat, y = price, color = clarity_factor)) + 
  theme(legend.position="bottom") +
  scale_color_brewer(palette="Paired")

p4<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = carat, y = price, color = channel)) + 
  theme(legend.position="bottom") +
  scale_color_brewer(palette="Paired")

p5<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = carat, y = price, color = store)) + 
  theme(legend.position="bottom") +
  scale_color_brewer(palette="Paired")

grid.arrange(arrangeGrob(p1,p2,p3,p4, p5, ncol=3))


p1<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = clarity, y = price, color = cut)) +
  theme(legend.position = "bottom")

p2<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = clarity, y = price, color = color)) +
  theme(legend.position = "bottom")

p3<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = clarity, y = price, color = channel)) +
  theme(legend.position = "bottom")

p4<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = clarity, y = price, color = store)) +
  theme(legend.position = "bottom")

grid.arrange(arrangeGrob(p1,p2,p3,p4, ncol=2))


p1<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = color, y = price, color = cut)) +
  theme(legend.position = "bottom")

p3<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = color, y = price, color = clarity)) +
  theme(legend.position = "bottom")

p2<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = color, y = price, color = channel)) +
  theme(legend.position = "bottom")

p4<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = color, y = price, color = store)) +
  theme(legend.position = "bottom")

grid.arrange(arrangeGrob(p1,p2,p3,p4, ncol=2))



##-----------------------------------------------------------------------##
## EDA
#create color and clarity factors for plots
two_months_salary$color_factor<-as.factor(two_months_salary$color)
two_months_salary$clarity_factor<-as.factor(two_months_salary$clarity)


chalmers <- two_months_salary[two_months_salary$store == "Chalmers",]
ggplot(chalmers, aes(x=price, fill=cut)) + 
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(title="Diamond Price by Clarity", x="Price", y="Frequency")

p1<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = color, y = price)) +
  labs(title="Diamond Price by Color", x="Color", y="Price")


p2<-ggplot(data = two_months_salary) +
  geom_point(mapping = aes(x = clarity, y = price)) +
  labs(title="Diamond Price by Clarity", x="Clarity", y="Price")

grid.arrange(arrangeGrob(p1,p2, ncol=2))


#histograms
p1<-ggplot(two_months_salary, aes(x=price, fill=clarity_factor)) + 
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(title="Diamond Price by Clarity", x="Price", y="Frequency")
p1
p2<-ggplot(two_months_salary, aes(x=price, fill=color_factor)) + 
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(title="Diamond Price by Color", x="Price", y="Frequency")
p2
p3<-ggplot(two_months_salary, aes(x=price, fill=cut)) + 
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(title="Diamond Price by Cut", x="Price", y="Frequency")
p3
p4<-ggplot(two_months_salary, aes(x=price, fill=channel)) + 
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(title="Diamond Price by Channel", x="Price", y="Frequency")
p4

grid.arrange(arrangeGrob(p1,p2,p3,p4, ncol=4))


##bar plots for store type
p1<-ggplot(two_months_salary, aes(fill=cut, x=store)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Diamond cut type frequency per store", x="Store", y="Number of Diamonds")
p1
p2<-ggplot(two_months_salary, aes(fill=color_factor, x=store)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Frequency of diamond color per store", x="Store", y="Number of Diamonds")
p2
p3<-ggplot(two_months_salary, aes(fill=clarity_factor, x=store)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Frequency of diamond clarity per store", x="Store", y="Number of Diamonds")
p3
p4<-ggplot(two_months_salary, aes(fill=channel, x=store)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Diamond channel per store", x="Store", y="Number of Diamonds")
p4

##bar plots for cut type
p1<-ggplot(data = two_months_salary) +
  geom_bar(mapping = aes(x = cut, fill = clarity_factor),position = "dodge") +
  labs(title="Diamond Clarity by Cut Type", x="Cut Type", y="Number of Diamonds")
p1
p2<-ggplot(data = two_months_salary) +
  geom_bar(mapping = aes(x = cut, fill = color_factor),position = "dodge") +
  labs(title="Diamond Color by Cut Type", x="Cut Type", y="Number of Diamonds")
p2

p3<-ggplot(data = two_months_salary) +
  geom_bar(mapping = aes(x = cut, fill = channel),position = "dodge") +
  labs(title="Diamond Channel by Cut Type", x="Cut Type", y="Number of Diamonds")
p3

grid.arrange(arrangeGrob(p1,p2,p3, ncol=3))

##lattice plots
xyplot(jitter(clarity) ~ jitter(carat) | channel, 
       data = two_months_salary,
       strip=function(...) strip.default(..., style=1),
       xlab = "Carat", 
       ylab = "Clarity")

xyplot(jitter(price) ~ jitter(carat) | color_factor + cut, 
       data = two_months_salary,
       layout = c(3, 3),
       strip=function(...) strip.default(..., style=1),
       xlab = "Size or Weight of Diamond (carats)", 
       ylab = "Price")

xyplot(jitter(store) ~ jitter(price) | channel, 
       data = two_months_salary,
       group = cut,
       strip=function(...) strip.default(..., style=1),
       xlab = "Size or Weight of Diamond (carats)", 
       ylab = "Price")


##
two_months_salary$internet <- ifelse((diamonds$channel == "Internet"),2,1)
two_months_salary$internet <- factor(diamonds$internet,levels=c(1,2),labels=c("NO","YES"))


##exploratory tree
reg_tree <- rpart(price ~ cut+channel+store+carat+color_factor+clarity_factor, method="anova", data=two_months_salary)
printcp(reg_tree) # display the results 
plotcp(reg_tree) # visualize cross-validation results 
summary(reg_tree) # detailed summary of splits
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(reg_tree) # visualize cross-validation results   
bestcp <- reg_tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"]

# plot tree
par(mfrow=c(1,1))
prp(reg_tree, faclen = 0, cex = 0.8, extra = 1)
rpart.rules(reg_tree)

#prune tree
pfit<- prune(reg_tree, cp=bestcp) # from cptable   
prp(pfit, faclen = 0, cex = 0.8, extra = 1)
rpart.rules(pfit)

tree_stats <- two_months_salary[two_months_salary$carat > 1.41,]
summary(tree_stats$price)

##-----------------------------------------------------------------------##
##split data into 70/30 training/test
smp_size <- floor(0.70 * nrow(two_months_salary))
# set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(two_months_salary)), size = smp_size)

train <- two_months_salary[train_ind, ]
test <- two_months_salary[-train_ind, ]
View(train)

#get rid of derived factor variables
drops <- c("color_factor","clarity_factor")
train<-train[ , !(names(train) %in% drops)]
test<-test[ , !(names(test) %in% drops)]

#naïve regression model using backwards variable selection and 
#evaluate its goodness-of-fit
lm1 <- lm(price~carat+color+clarity+cut+channel+store, data=train)
summary(lm1)
confint(lm1, level=0.95) # CIs for model parameters 
influence(lm1) # regression diagnostics
plot(lm1)
residuals1 <- resid(lm1) #Extracting residuals from model1
plot(residuals1) #Plotting the residuals

res_lm1 <- residuals(lm1)
rmse_train <- (mean((res_lm1)^2))^(1/2)
rmse_train #1412.778

lm1_test <- predict(lm1, newdata = test)
test_error_lm1 <- lm1_test-test$price
rmse_test_lm <- sqrt(mean(test_error_lm1^2))
rmse_test_lm #1255.982


#backward stepwise regression
step <-step(lm1, direction="backward")
summary(step)
par(mfrow=c(2,2))
plot(step)

#identified observations 93, 100, and 36 as influential via Cook's D
step_resids<-resid(step)
plot(step_resids)
step$anova

res_step_lm1 <- residuals(step)
rmse_train <- (mean((res_step_lm1)^2))^(1/2)
rmse_train #1412.778

step_lm1_test <- predict(step, newdata = test)
test_error_step_lm1 <- step_lm1_test-test$price
rmse_test_lm <- sqrt(mean(test_error_step_lm1^2))
rmse_test_lm #1255.982


#AIC stepwise regression in both directions
step_AIC <- stepAIC(lm1, direction="both")
summary(step_AIC)
plot(step_AIC)
stepAIC_resids<-resid(step_AIC)
plot(stepAIC_resids)
step_AIC$anova # display results

all_vifs <- car::vif(step_AIC)
print(all_vifs)

res_stepAIC_lm1 <- residuals(step_AIC)
rmse_train <- (mean((res_stepAIC_lm1)^2))^(1/2)
rmse_train #1412.778


##Model EDA
#use color and clarity factors
lm2 <- lm(price~carat+color_factor+clarity_factor+cut+channel+store,data=train)
summary(lm2)
confint(lm2, level=0.95) # CIs for model parameters 
influence(lm2) # regression diagnostics
plot(lm2) #identified observations 93, 381, and 8 as influential
residuals2 <- resid(lm2) #Extracting residuals from model1
plot(residuals2) #Plotting the residuals

res_lm2<-residuals(lm2)
rmse_train<-(mean((res_lm2)^2))^(1/2)
rmse_train #1357.787

lm2_test <- predict(lm2, newdata = test)
test_error_lm2 <- lm2_test-test$price
rmse_test_lm <- sqrt(mean(test_error_lm2^2))
rmse_test_lm #1281.954


#backward stepwise regression
step2 <-step(lm2, direction="backward")
summary(step2)
par(mfrow=c(2,2))
plot(step2)
step2$anova

res_step_lm2<-residuals(step2)
rmse_train<-(mean((res_step_lm2)^2))^(1/2)
rmse_train #1357.787

#stepwise regression
step_AIC2 <- stepAIC(lm2, direction="both")
summary(step_AIC2)
step_AIC2$anova # display results

res_stepAIC_lm2 <- residuals(step_AIC2)
rmse_train <- (mean((res_stepAIC_lm2)^2))^(1/2)
rmse_train #1357.787

all_vifs2 <- car::vif(step_AIC2)
print(all_vifs2)


#compare models
anova(lm1,lm2)
anova(step_AIC, step_AIC2)
anova(step2,step)



#log = natural log
mod <- lm(log(price) ~ log(carat), data = train)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
mod_resids<-resid(mod)
plot(mod_resids)
#calculate MSE
test_log_price<-log(test$price)
pred_mod <- predict(mod, newdata = test, type="response") # validation predictions
mean((test_log_price - pred_mod)^2) # mean prediction error
# 0.059277
sd((test_log_price - pred_mod)^2)/sqrt(length(test_log_price)) # std error
#0.008908

#library(forecast)
#accuracy(pred_mod, test_log_price)

#plot outliers by carat
ggplot(data = train) + 
  geom_point(mapping = aes(x = carat, y = mod_resids))

mod2<-update(mod,~ . +color_factor)
summary(mod2)
plot(mod2)
pred_mod2 <- predict(mod2, newdata = test, type="response") # validation predictions
mean((test_log_price - pred_mod2)^2) # mean prediction error
# 0.04344
sd((test_log_price - pred_mod2)^2)/sqrt(length(test_log_price)) # std error
#0.007692048
mape(test_log_price, pred_mod2)
# 0.0179932

mod3<-update(mod,~ . +clarity_factor)
summary(mod3)
plot(mod3)
pred_mod3 <- predict(mod3, newdata = test, type="response") # validation predictions
mean((test_log_price - pred_mod3)^2) # mean prediction error
# 0.0538773
sd((test_log_price - pred_mod3)^2)/sqrt(length(test_log_price)) # std error
#0.0078852
mape(test_log_price, pred_mod3)
# 0.02104799

mod4<-update(mod,~.+cut)
summary(mod4)
plot(mod4)
pred_mod4 <- predict(mod4, newdata = test, type="response") # validation predictions
mean((test_log_price - pred_mod4)^2) # mean prediction error
# 0.05785
sd((test_log_price - pred_mod4)^2)/sqrt(length(test_log_price)) # std error
# 0.0083
mape(test_log_price, pred_mod4)
# 0.02209966

mod5<-update(mod,~.+color)
summary(mod5)
plot(mod5)
pred_mod5 <- predict(mod5, newdata = test, type="response") # validation predictions
mean((test_log_price - pred_mod5)^2) # mean prediction error
# 0.04306
sd((test_log_price - pred_mod5)^2)/sqrt(length(test_log_price)) # std error
# 0.0073999
mape(test_log_price, pred_mod5)
# 0.01808191

mod6<-lm(log(price)~log(carat) + clarity_factor + store, data=train)
summary(mod6)
par(mfrow=c(2,2))
plot(mod6)
pred_mod6 <- predict(mod6, newdata = test, type="response") # validation predictions
mean((test_log_price - pred_mod6)^2) # mean prediction error
# 0.0380892
sd((test_log_price - pred_mod6)^2)/sqrt(length(test_log_price)) # std error
# 0.00398236
mape(test_log_price, pred_mod6)
# 0.01916894
stargazer(mod6, type = "html",out = "reg_summary.doc")

mod7<-update(mod,~.+clarity)
summary(mod7)
plot(mod7)
pred_mod7<-predict(mod7, newdata = test, type="response")
mean((test_log_price - pred_mod7)^2)
# 0.05736
sd((test_log_price - pred_mod7)^2)/sqrt(length(test_log_price))
# 0.0083859
mape(test_log_price, pred_mod7)
# 0.02182252


mod8<-lm(log(price)~I(carat^(1/3))+clarity+store, data=train)
summary(mod8)         
plot(mod8)
pred_mod8<-predict(mod8, newdata = test, type="response")
mean((test_log_price - pred_mod8)^2)
# 0.0524
sd((test_log_price - pred_mod8)^2)/sqrt(length(test_log_price))
# 0.0075269
library(Metrics)
mse(test_log_price,pred_mod8)
# 0.05241275
mape(test_log_price, pred_mod8)
# 0.02206556



#play around with exponents
ggplot(data = train) + 
  geom_point(mapping = aes(x = I(carat^(1/2)), y = price))

ggplot(data = train) + 
  geom_point(mapping = aes(x = I(carat^(1/2)), y = log10(price)))

ggplot(data = train) + 
  geom_point(mapping = aes(x = I(carat^(1/2)), y = log(price)))

ggplot(data = train) + 
  geom_point(mapping = aes(x = I(carat^(1/3)), y = price))

ggplot(data = train) + 
  geom_point(mapping = aes(x = I(carat^(1/3)), y = log10(price)))

ggplot(data = train) + 
  geom_point(mapping = aes(x = I(carat^(1/3)), y = log(price)))



##LM w/o interaction terms
lm1_1 <- lm(I(log(price)) ~ I(carat^(1/2)), data = train)
lm1_2 <- update(lm1_1,~ . +carat)
lm1_3 <- update(lm1_2,~ . +cut)
lm1_4 <- update(lm1_3,~ . +color)
lm1_5 <- update(lm1_4,~ . +clarity)
lm1_6 <- update(lm1_5,~ . +color_factor)
lm1_7 <- update(lm1_6,~ . +clarity_factor)
anova(lm1_1,lm1_2,lm1_3,lm1_4,lm1_5,lm1_6,lm1_7)
pred_lm1_2<-predict(lm1_2, newdata = test, type="response")
mse(test_log_price,pred_lm1_2)
# 0.06088513
mape(test_log_price,pred_lm1_2)
# 0.02235175
pred_lm1_3<-predict(lm1_3, newdata = test, type="response")
mse(test_log_price,pred_lm1_3)
# 0.05990271
mape(test_log_price,pred_lm1_3)
# 0.02247582

lm2_1<-update(mod1,~ . +cut)
lm2_2<-update(mod1,~ . +color_factor)
lm2_3<-update(mod1,~ . +clarity_factor)
anova(lm1_1,lm2_1,lm2_2,lm2_3)

lm3_1 <- lm(I(log10(price)) ~ I(carat^(1/3)), data = train)
lm3_2 <- update(mod1,~ . +carat)
lm3_3 <- update(mod2,~ . +cut)
lm3_4 <- update(mod3,~ . +color)
lm3_5 <- update(mod4,~ . +clarity)
lm3_6 <- update(mod3,~ . +color_factor)
lm3_7 <- update(mod6,~ . +clarity_factor)
anova(lm3_1,lm3_2,lm3_3,lm3_4,lm3_5,lm3_6,lm3_7)

lm4_1<-update(mod1,~ . +cut)
lm4_2<-update(mod1,~ . +color_factor)
lm4_3<-update(mod1,~ . +clarity_factor)
anova(lm3_1,lm4_1,lm4_2,lm4_3)


##LM with interaction terms
lm_int1 <- lm(log(price) ~ log(carat)*cut, data=train)
summary(lm_int1)
pred_lm_int1<-predict(lm_int1, newdata = test, type="response")
mse(test_log_price,pred_lm_int1)
# 0.05740184
mape(test_log_price,pred_lm_int1)
# 0.02191598

lm_int2 <- lm(log(price) ~ log(carat)*cut+store, data=train)
summary(lm_int2)
pred_lm_int2<-predict(lm_int2, newdata = test, type="response")
mse(test_log_price,pred_lm_int2)
# 0.04557454
mape(test_log_price,pred_lm_int2)
# 0.01978666


lm_int3 <- lm(log(price) ~ log(carat)*clarity_factor+store, data=train)
summary(lm_int3)
pred_lm_int3<-predict(lm_int3, newdata = test, type="response")
mse(test_log_price,pred_lm_int3)
# 0.03787778
mape(test_log_price,pred_lm_int3)
# 0.01922718

lm_int4 <- lm(log(price) ~ log(carat)*clarity_factor+store+cut, data=train)
summary(lm_int4)
plot(lm_int4)
pred_lm_int4<-predict(lm_int4, newdata = test, type="response")
mse(test_log_price,pred_lm_int4)
# 0.03543461
mape(test_log_price,pred_lm_int4)
# 0.01874404

lm_int5 <- lm(log(price) ~ log(carat)*clarity_factor+color_factor, data=train)
summary(lm_int5)
plot(lm_int5)
pred_lm_int5<-predict(lm_int5, newdata = test, type="response")
mse(test_log_price,pred_lm_int5)
# 0.03384953
mape(test_log_price,pred_lm_int5)
# 0.01549507




##regression trees, again
library(partykit)

rpart_tree2<-rpart(log(price)~I(carat^(1/2)) + color_factor + clarity_factor + cut + channel + store, method = "anova",data=train)
plotcp(rpart_tree2, minline = TRUE) #fine
prp(rpart_tree2, faclen = 0, cex = 0.8, extra = 1)
pred_rpart_tree2 <- predict(rpart_tree2, newdata = test)
mse(test_log_price,pred_rpart_tree2)
# 0.08595131
mape(test_log_price,pred_rpart_tree2)
# 0.02703043
rparty.tree2 <- as.party(rpart_tree2)
rparty.tree2
plot(rparty.tree2)
rpart_tree2.cv<-cv.tree(rpart_tree2)
install.packages("rattle")
library(rattle)
ptree<- prune(rpart_tree2,cp=rpart_tree2$cptable[which.min(rpart_tree2$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree)

rpart_tree1<-rpart(log(price)~log(carat) + clarity_factor + color_factor + cut + channel + store, method = "anova",data=train)
plotcp(rpart_tree1, minline = TRUE) #fine
prp(rpart_tree1, faclen = 0, cex = 0.8, extra = 1)
pred_rpart_tree1 <- predict(rpart_tree1, newdata = test)
mse(test_log_price,pred_rpart_tree1)
# 0.08595131
mape(test_log_price,pred_rpart_tree1)
# 0.02703043
summary(rpart_tree1)
rparty.tree1 <- as.party(rpart_tree1)
rparty.tree1
plot(rparty.tree1)
rsq.rpart(rpart_tree1)

tree_mod1<-rpart(log(price)~log(carat)+color+clarity+cut+channel+store,data=train,method="anova")
plotcp(tree_mod1, minline = TRUE) #fine
prp(tree_mod1, faclen = 0, cex = 0.8, extra = 1)
pred_rpart_tree2 <- predict(tree_mod1, newdata = test)
mse(test_log_price,pred_rpart_tree2)
# 0.08692634
mape(test_log_price,pred_rpart_tree2)
# 0.02774124
rparty.tree2 <- as.party(tree_mod1)
rparty.tree2
plot(rparty.tree2)



View(train)
attach(train)
##random forest
set.seed(1)
rm_forest1=randomForest(log(price) ~ log(carat)+color_factor+clarity_factor+cut+channel+store, data = train, importance=TRUE)
varImpPlot(rm_forest1, main="Variable Importance for Random Forest Model")

oob.err=double(6)
test.err=double(6)
mape.err=double(6)
for(mtry in 1:6) 
{
  rf=randomForest(log(price) ~ log(carat)+color_factor+clarity_factor+cut+channel+store, data = train, mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test, mse(test_log_price,pred)) #Mean Squared Test Error
  mape.err[mtry]=with(test, mape(test_log_price,pred))
  cat(mtry," ") #printing the output to the console
}
oob.err
test.err
mape.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))





##### RESULTS
#model comparison






