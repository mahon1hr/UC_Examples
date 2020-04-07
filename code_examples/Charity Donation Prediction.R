#422 Charity Data Final Project

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.
# want to predict donr

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.
# want to predict damt


#libraries used for analysis and display presentation
library(dplyr)
library(class)
library(ISLR)
library(randomForest)
library(caret)
library(MASS)
library(ggplot2)
library(data.table)
library(DataExplorer)
library(PerformanceAnalytics)
library(usdm)
library(e1071)
library(glmnet)
library(pls)
library(tree)
library(gbm)
library(nnet)
library(pls)
library(ROCR)


#quick look at data
str(charity)
summary(charity)
#donr and damt have 2007 NAs which will be used for the test set


#Sample Code Transformations of Predictor
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)


# set up data for analysis
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
length(c.train)
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)






#------------------------------------------------------------------------------------------------
######EDA#######
#use original data (unstandardized)
x.train=data.frame(data.train[,2:23])  #First without standardization
y.train=data.frame(data.train[c.train==1,2:23])  #Remove non-donors


summary(x.train)
1989+1995

?DataExplorer
plot_str(x.train)
#clean up visuals later for paper
plot_histogram(x.train)
plot_histogram(y.train)

#plot notes:
## remove donr, reg1, reg2, reg3, reg4, home, genf, wrat <- discrete or qualitative
## rgif, agif, tlag, incm,inca, plow,tgif, lgif are all right skewed
## potentially transform them, or cap for outliers
## look at outliers
## damt: large spike at 0 => people who don't donate

plot_density(x.train)
plot_density(y.train)


x.train$region=5
x.train$region=ifelse(x.train$reg1==1,1, ifelse(x.train$reg2==1,2, ifelse(x.train$reg3==1,3, ifelse(x.train$reg4==1,4,5))))
region=as.factor(x.train$region)
y.train$region=5
y.train$region=ifelse(y.train$reg1==1,1, ifelse(y.train$reg2==1,2, ifelse(y.train$reg3==1,3, ifelse(y.train$reg4==1,4,5))))
region=as.factor(y.train$region)


#change qualitative variables to factors
x.train$reg1=as.factor(x.train$reg1)
x.train$reg2=as.factor(x.train$reg2)
x.train$reg3=as.factor(x.train$reg3)
x.train$reg4=as.factor(x.train$reg4)
x.train$home=as.factor(x.train$home)
x.train$hinc=as.factor(x.train$hinc)
x.train$genf=as.factor(x.train$genf)
x.train$donr=as.factor(x.train$donr)

y.train$reg1=as.factor(y.train$reg1)
y.train$reg2=as.factor(y.train$reg2)
y.train$reg3=as.factor(y.train$reg3)
y.train$reg4=as.factor(y.train$reg4)
y.train$home=as.factor(y.train$home)
y.train$hinc=as.factor(y.train$hinc)
y.train$genf=as.factor(y.train$genf)
y.train$donr=as.factor(y.train$donr)



?plot_correlation
plot_correlation(x.train,type = "continuous",title = "Correlations for Donors and Non-donors")
plot_correlation(y.train,type = "continuous",title="Correlations for Donors")

interaction.vars <- data.frame(cbind(x.train$damt,
                                     x.train$npro, x.train$tgif,
                                     x.train$lgif, x.train$rgif, 
                                     x.train$tdon, x.train$tlag,
                                     x.train$agif, x.train$avhv,
                                     x.train$plow, x.train$inca,
                                     x.train$incm, x.train$wrat))
colnames(interaction.vars) <- c("damt","npro", "tgif", "lgif", "rgif", 
                                "tdon", "tlag", "agif", "avhv",
                                "plow","inca", "incm","wrat")
cor(interaction.vars)
IntCors <- chart.Correlation(interaction.vars)

#repeat for donors only
interaction.vars <- data.frame(cbind(y.train$damt,
                                     y.train$npro, y.train$tgif,
                                     y.train$lgif, y.train$rgif, 
                                     y.train$tdon, y.train$tlag,
                                     y.train$agif, y.train$avhv,
                                     y.train$plow, y.train$inca,
                                     y.train$incm, y.train$wrat))
colnames(interaction.vars) <- c("damt","npro", "tgif", "lgif", "rgif", 
                                "tdon", "tlag", "agif", "avhv",
                                "plow","inca", "incm","wrat")
cor(interaction.vars)
IntCors <- chart.Correlation(interaction.vars, main="Correlation plot for Donors")
vif(x.train)
vif(y.train)


counts=table(x.train$donr, x.train$donr)
par(mfrow=c(1,3))
barplot(counts, main="Number of Donors vs. Non-donors",ylab="Number of Observations", ylim=c(0,2000), 
        col=c("gray","darkblue"), legend=rownames(counts))
boxplot(damt~donr, data=y.train, col="darkblue",main="Donation Amount", ylab="Amount Donated")
hist(y.train$damt, main="Distribution of Donation Amount",col="darkblue", xlab="Amount Donated")
summary(x.train$donr)
mean(y.train$damt) #$14.50
median(y.train$damt) #$14


#look at region
counts1=table(x.train$donr, x.train$region)
par(mfrow = c(1,3))
barplot(counts1, main="Number of Donors by Region", xlab="Region", col=c("gray","darkblue"),
        legend = rownames(counts1))

boxplot(damt~region ,data=x.train, col=c("lightsalmon","lightblue","khaki","darkseagreen1","plum1"),
        main="Amount Donated by Region", xlab="Region", ylab="Amount Donated")

boxplot(damt~region,data = y.train,col=c("lightsalmon","lightblue","khaki","darkseagreen1","plum1"),
        main="Amount Donated by Region (donors only)",xlab="Region",ylab="Amount Donated")

sum(y.train$reg2=='1') # 903
903/1995 #~45%
sum(y.train$reg1=='1') #454
454/1995 #~23%

#breakdown donation amount by region
par(mfrow=c(2,3))
hist(x.train$damt[x.train$region=='1'], main="Region 1",xlim=c(0,25),ylim=c(0,400),xlab="Donation Amount", col="lightsalmon")
boxplot(x.train$damt[x.train$region=="1"], main="Region 1",ylim=c(0,25),ylab="Donation Amount",col="lightsalmon")
qqnorm(x.train$damt[x.train$region=='1'],main = "Region 1",col="lightsalmon")
hist(y.train$damt[y.train$region=='1'], main="Region 1 (donr=1)",xlim=c(0,25),ylim=c(0,125),xlab="Donation Amount", col="lightsalmon")
boxplot(y.train$damt[y.train$region=="1"], main="Region 1 (donr=1)",ylab="Donation Amount",col="lightsalmon")
qqnorm(y.train$damt[y.train$region=='1'],main = "Region 1 (donr=1)",col="lightsalmon")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$region=='2'], main="Region 2",xlim=c(0,25),ylim=c(0,450),xlab="Donation Amount", col="lightblue")
boxplot(x.train$damt[x.train$region=='2'], main="Region 2",ylim=c(0,25),ylab="Donation Amount", col="lightblue")
qqnorm(x.train$damt[x.train$region=='2'],main = "Region 2",col="lightblue")
hist(y.train$damt[y.train$region=='2'], main="Region 2 (donr=1)",xlim=c(0,25),ylim=c(0,400),xlab="Donation Amount", col="lightblue")
boxplot(y.train$damt[y.train$region=='2'], main="Region 2 (donr=1)",ylab="Donation Amount", col="lightblue")
qqnorm(y.train$damt[y.train$region=='2'],main = "Region 2 (donr=1)",col="lightblue")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$region=='3'], main="Region 3",xlim=c(0,25),ylim=c(0,350),xlab="Donation Amount", col="khaki")
boxplot(x.train$damt[x.train$region=='3'], main="Region 3",ylim=c(0,25),ylab="Donation Amount", col="khaki")
qqnorm(x.train$damt[x.train$region=='3'],main = "Region 3",col="khaki")
hist(y.train$damt[y.train$region=='3'], main="Region 3 (donr=1)",xlim=c(0,25),ylim=c(0,50),xlab="Donation Amount", col="khaki")
boxplot(y.train$damt[y.train$region=='3'], main="Region 3 (donr=1)",ylab="Donation Amount", col="khaki")
qqnorm(y.train$damt[y.train$region=='3'],main = "Region 3 (donr=1)",col="khaki")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$region=='4'], main="Region 4",xlim=c(0,25),ylim=c(0,400),xlab="Donation Amount", col="darkseagreen1")
boxplot(x.train$damt[x.train$region=='4'], main="Region 4",ylim=c(0,25),ylab="Donation Amount", col="darkseagreen1")
qqnorm(x.train$damt[x.train$region=='4'],main = "Region 4",col="darkseagreen1")
hist(y.train$damt[y.train$region=='4'], main="Region 4 (donr=1)",xlim=c(0,25),ylim=c(0,50),xlab="Donation Amount", col="darkseagreen1")
boxplot(y.train$damt[y.train$region=='4'], main="Region 4 (donr=1)",ylab="Donation Amount", col="darkseagreen1")
qqnorm(y.train$damt[y.train$region=='4'],main = "Region 4 (donr=1)",col="darkseagreen1")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$region=='5'], main="Region 5",xlim=c(0,25),ylim=c(0,550),xlab="Donation Amount", col="plum1")
boxplot(x.train$damt[x.train$region=='5'], main="Region 5",ylim=c(0,25),ylab="Donation Amount", col="plum1")
qqnorm(x.train$damt[x.train$region=='5'],main = "Region 5",col="plum1")
hist(y.train$damt[y.train$region=='5'], main="Region 5 (donr=1)",xlim=c(0,25),ylim=c(0,80),xlab="Donation Amount", col="plum1")
boxplot(y.train$damt[y.train$region=='5'], main="Region 5 (donr=1)",ylab="Donation Amount", col="plum1")
qqnorm(y.train$damt[y.train$region=='5'],main = "Region 5 (donr=1)",col="plum1")

mean(y.train$damt[y.train$reg4=='1'])
median(y.train$damt[y.train$reg4=='1'])
mean(y.train$damt[y.train$reg2=='1'])
median(y.train$damt[y.train$reg2=='1'])
mean(y.train$damt[y.train$reg1=='1'])
median(y.train$damt[y.train$reg1=='1'])



#look at homeowner type
par(mfrow=c(1,3),lty=1)
counts2=table(x.train$donr, x.train$home)
barplot(counts2, main="Number of Donors by Home Ownership Type",
        xlab="Home Ownership", col=c("gray","darkblue"), legend = rownames(counts2))

boxplot(damt~home ,data=x.train, col=(c("skyblue","red")), main="Amount Donated by Home Ownership Type", 
        xlab="Home Ownership", ylab="Amount Donated")

boxplot(damt~home, data = y.train, col=c("skyblue","red"),main="Amount Donated by Home Ownership Type (donors only)",
        xlab="Home Ownership",ylab="Amount Donated")

sum(x.train$home=='1')

#breakdown donation amount by homeowner type
par(mfrow=c(2,3))
hist(x.train$damt[x.train$home=="1"],main="Homeowner",xlab="Donation Amount",col="red")
boxplot(x.train$damt[x.train$home=="1"],main="Homeowner",ylab="Donation Amount",col="red")
qqnorm(x.train$damt[x.train$home=="1"],main = "Homeowner",col="red")
hist(y.train$damt[y.train$home=="1"],main="Homeowner (donr=1)",xlab="Donation Amount",ylim=c(0,450),col="red")
boxplot(y.train$damt[y.train$home=="1"],main="Homeowner (donr=1)",ylab="Donation Amount",col="red")
qqnorm(y.train$damt[y.train$home=="1"],main = "Homeowner (donr=1)",col="red")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$home=="0"],main="Non-Homeowner",xlab="Donation Amount",col="skyblue")
boxplot(x.train$damt[x.train$home=="0"],main="Non-Homeowner",ylab="Donation Amount",col="skyblue")
qqnorm(x.train$damt[x.train$home=="0"],main = "Non-Homeowner",col="skyblue")
hist(y.train$damt[y.train$home=="0"],main="Non-Homeowner (donr=1)",xlab="Donation Amount",ylim=c(0,20),col="skyblue")
boxplot(y.train$damt[y.train$home=="0"],main="Non-Homeowner (donr=1)",ylab="Donation Amount",col="skyblue")
qqnorm(y.train$damt[y.train$home=="0"],main = "Non-Homeowner (donr=1)",col="skyblue")


mean(x.train$damt[x.train$home=='1'])
median(x.train$damt[x.train$home=='1'])
mean(y.train$damt[y.train$home=='1'])
median(y.train$damt[y.train$home=='1'])



#look at number of children
counts3=table(x.train$donr, x.train$chld)
par(mfrow=c(1,3))
barplot(counts3, main="Number of Donors by Number of Children", xlab="Number of Children", 
        col=c("gray","darkblue"),legend = rownames(counts3))

boxplot(damt~chld ,data=x.train, col=(c("lightsalmon","lightblue","khaki","darkseagreen1","plum1","snow1")),
        main="Amount Donated by Number of Children", 
        xlab="Number of Children", ylab="Donation Amount")

boxplot(damt~chld ,data=y.train, col=(c("lightsalmon","lightblue","khaki","darkseagreen1","plum1","snow1")),
        main="Amount Donated by Number of Children (donors only)", xlab="Number of Children", ylab="Donation Amount")

sum(x.train$chld=='0') #1395
1395/3984 #~35%
sum(x.train$chld=='1') #404
404/3984 #~10%
sum(x.train$chld=='2') #1151
1151/3984 #~29%
sum(x.train$chld=='3') #656
sum(x.train$chld=='4') #281
sum(x.train$chld=='5') #97
(656+281+97)/3984 #~26%


#breakdown donation amount by number of children
par(mfrow=c(2,3))
hist(x.train$damt[x.train$chld=='0'], main="Children = 0",xlim=c(0,25),ylim=c(0,500),xlab="Donation Amount", col="lightsalmon")
boxplot(x.train$damt[x.train$chld=="0"], main="Children = 0",ylim=c(0,25),ylab="Donation Amount",col="lightsalmon")
qqnorm(x.train$damt[x.train$chld=='0'],main = "Children = 0",col="lightsalmon")
hist(y.train$damt[y.train$chld=='0'], main="Children = 0 (donr=1)",xlim=c(0,25),ylim=c(0,300),xlab="Donation Amount", col="lightsalmon")
boxplot(y.train$damt[y.train$chld=="0"], main="Children = 0 (donr=1)",ylab="Donation Amount",col="lightsalmon")
qqnorm(y.train$damt[y.train$chld=='0'],main = "Children = 0 (donr=1)",col="lightsalmon")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$chld=='1'], main="Children = 1",xlim=c(0,25),ylim=c(0,200),xlab="Donation Amount", col="lightblue")
boxplot(x.train$damt[x.train$chld=='1'], main="Children = 1",ylim=c(0,25),ylab="Donation Amount", col="lightblue")
qqnorm(x.train$damt[x.train$chld=='1'],main = "Children = 1",col="lightblue")
hist(y.train$damt[y.train$chld=='1'], main="Children = 1 (donr=1)",xlim=c(0,25),ylim=c(0,60),xlab="Donation Amount", col="lightblue")
boxplot(y.train$damt[y.train$chld=='1'], main="Children = 1 (donr=1)",ylab="Donation Amount", col="lightblue")
qqnorm(y.train$damt[y.train$chld=='1'],main = "Children = 1 (donr=1)",col="lightblue")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$chld=='2'], main="Children = 2",xlim=c(0,25),ylim=c(0,800),xlab="Donation Amount", col="khaki")
boxplot(x.train$damt[x.train$chld=='2'], main="Children = 2",ylim=c(0,25),ylab="Donation Amount", col="khaki")
qqnorm(x.train$damt[x.train$chld=='2'],main = "Children = 2",col="khaki")
hist(y.train$damt[y.train$chld=='2'], main="Children = 2 (donr=1)",xlim=c(0,25),ylim=c(0,125),xlab="Donation Amount", col="khaki")
boxplot(y.train$damt[y.train$chld=='2'], main="Children = 2 (donr=1)",ylab="Donation Amount", col="khaki")
qqnorm(y.train$damt[y.train$chld=='2'],main = "Children = 2 (donr=1)",col="khaki")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$chld=='3'], main="Children = 3",xlim=c(0,25),ylim=c(0,600),xlab="Donation Amount", col="darkseagreen1")
boxplot(x.train$damt[x.train$chld=='3'], main="Children = 3",ylim=c(0,25),ylab="Donation Amount", col="darkseagreen1")
qqnorm(x.train$damt[x.train$chld=='3'],main = "Children = 3",col="darkseagreen1")
hist(y.train$damt[y.train$chld=='3'], main="Children = 3 (donr=1)",xlim=c(0,25),ylim=c(0,80),xlab="Donation Amount", col="darkseagreen1")
boxplot(y.train$damt[y.train$chld=='3'], main="Children = 3 (donr=1)",ylab="Donation Amount", col="darkseagreen1")
qqnorm(y.train$damt[y.train$chld=='3'],main = "Children = 3 (donr=1)",col="darkseagreen1")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$chld=='4'], main="Children = 4",xlim=c(0,25),ylim=c(0,250),xlab="Donation Amount", col="plum1")
boxplot(x.train$damt[x.train$chld=='4'], main="Children = 4",ylim=c(0,25),ylab="Donation Amount", col="plum1")
qqnorm(x.train$damt[x.train$chld=='4'],main = "Children = 4",col="plum1")
hist(y.train$damt[y.train$chld=='4'], main="Children = 4 (donr=1)",xlim=c(0,25),ylim=c(0,25),xlab="Donation Amount", col="plum1")
boxplot(y.train$damt[y.train$chld=='4'], main="Children = 4 (donr=1)",ylab="Donation Amount", col="plum1")
qqnorm(y.train$damt[y.train$chld=='4'],main = "Children = 4 (donr=1)",col="plum1")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$chld=='5'], main="Children = 5",xlim=c(0,25),ylim=c(0,100),xlab="Donation Amount", col="snow1")
boxplot(x.train$damt[x.train$chld=='5'], main="Children = 5",ylim=c(0,25),ylab="Donation Amount", col="snow1")
qqnorm(x.train$damt[x.train$chld=='5'],main = "Children = 5",col="snow4")
hist(y.train$damt[y.train$chld=='5'], main="Children = 5 (donr=1)",xlim=c(0,25),ylim=c(0,5),xlab="Donation Amount", col="snow1")
boxplot(y.train$damt[y.train$chld=='5'], main="Children = 5 (donr=1)",ylab="Donation Amount", col="snow1")
qqnorm(y.train$damt[y.train$chld=='5'],main = "Children = 5 (donr=1)",col="snow4")

median(y.train$damt[y.train$chld=='3']) #13
median(y.train$damt[y.train$chld=='4']) #13
median(y.train$damt[y.train$chld=='5']) #12.5




#look at gender
counts4=table(x.train$donr, x.train$genf)
par(mfrow = c(1,3))
barplot(counts4, main="Number of Donors by Gender",
        xlab="Gender", col=c("gray","darkblue"),legend = rownames(counts4))

boxplot(damt~genf ,data=x.train, col=c("skyblue","red"),
        main="Amount Donated by Gender", xlab="Gender", ylab="Donation Amount")

boxplot(damt~genf ,data=y.train, col=c("skyblue","red"),
        main="Amount Donated by Gender (donors only)", xlab="Gender", ylab="Donation Amount")

#breakdown donation amount by gender
par(mfrow=c(2,3))
hist(x.train$damt[x.train$genf=="1"],main="Female",xlab="Donation Amount",col="red")
boxplot(x.train$damt[x.train$genf=="1"],main="Female",ylab="Donation Amount",col="red")
qqnorm(x.train$damt[x.train$genf=="1"],main = "Female",col="red")
hist(y.train$damt[y.train$genf=="1"],main="Female (donr=1)",xlab="Donation Amount",ylim=c(0,300),col="red")
boxplot(y.train$damt[y.train$genf=="1"],main="Female (donr=1)",ylab="Donation Amount",col="red")
qqnorm(y.train$damt[y.train$genf=="1"],main = "Female (donr=1)",col="red")

par(mfrow=c(2,3))
hist(x.train$damt[x.train$genf=="0"],main="Male",xlab="Donation Amount",ylim=c(0,800),col="skyblue")
boxplot(x.train$damt[x.train$genf=="0"],main="Male",ylab="Donation Amount",col="skyblue")
qqnorm(x.train$damt[x.train$genf=="0"],main = "Male",col="skyblue")
hist(y.train$damt[y.train$genf=="0"],main="Male (donr=1)",xlab="Donation Amount",ylim=c(0,200),col="skyblue")
boxplot(y.train$damt[y.train$genf=="0"],main="Male (donr=1)",ylab="Donation Amount",col="skyblue")
qqnorm(y.train$damt[y.train$genf=="0"],main = "Male (donr=1)",col="skyblue")

sum(x.train$genf=='1') #2410
2410/3984 #60.5%
sum(y.train$genf=='1') #1190
1190/2410 #49.4%
sum(x.train$genf=='0') #1574
1574/3984 #39.5%
sum(y.train$genf=='0') #805
805/1574 #51%
mean(y.train$damt[y.train$genf=='1']) #14.47
median(y.train$damt[y.train$genf=='1']) #14
mean(y.train$damt[y.train$genf=='0']) #14.53
median(y.train$damt[y.train$genf=='0']) #14



#look at hinc
counts5=table(x.train$donr, x.train$hinc)
par(mfrow=c(1,3),lty=1)
barplot(counts5, main="Number of Donors by Household Income", xlab="Household Income",
        col=c("gray","darkblue"),legend=rownames(counts5))
boxplot(damt~hinc, data = x.train, col=c("lightsalmon","lightblue","khaki","darkseagreen1","plum1","snow1","antiquewhite"),
        main="Amount Donated by Household Income",xlab="Household Income",ylab="Donation Amount")
boxplot(damt~hinc, data = y.train, col=c("lightsalmon","lightblue","khaki","darkseagreen1","plum1","snow1","antiquewhite"),
        main="Amount Donated by Household Income (donors only)",xlab="Household Income",ylab="Donation Amount")


sum(x.train$hinc=='4') #1835
1835/3984 #46%
sum(y.train$hinc=='4') #1226
1226/1995
sum(y.train$hinc=='5') #274
sum(y.train$hinc=='6') #70
70/274


mean(y.train$incm[y.train$hinc=='4']) #$47.46K
median(y.train$incm[y.train$hinc=='4']) #$42K
mean(y.train$inca[y.train$hinc=='4']) #$60.10K
median(y.train$inca[y.train$hinc=='4']) #54K

mean(y.train$incm[y.train$hinc=='1']) 
median(y.train$incm[y.train$hinc=='1']) 
mean(y.train$inca[y.train$hinc=='1']) 
median(y.train$inca[y.train$hinc=='1'])

mean(y.train$incm[y.train$hinc=='2']) 
median(y.train$incm[y.train$hinc=='2']) 
mean(y.train$inca[y.train$hinc=='2']) 
median(y.train$inca[y.train$hinc=='2'])

mean(y.train$incm[y.train$hinc=='7']) 
median(y.train$incm[y.train$hinc=='7']) 
mean(y.train$inca[y.train$hinc=='7']) 
median(y.train$inca[y.train$hinc=='7'])

mean(y.train$damt[y.train$hinc=='1']) #13.07
mean(y.train$damt[y.train$hinc=='2']) #13.90
mean(y.train$damt[y.train$hinc=='3']) #14.06
mean(y.train$damt[y.train$hinc=='4']) #14.49
mean(y.train$damt[y.train$hinc=='5']) #15.01
mean(y.train$damt[y.train$hinc=='6']) #15.01
mean(y.train$damt[y.train$hinc=='7']) #15.50

median(y.train$damt[y.train$hinc=='1']) #13.00
median(y.train$damt[y.train$hinc=='2']) #14
median(y.train$damt[y.train$hinc=='3']) #14
median(y.train$damt[y.train$hinc=='4']) #14
median(y.train$damt[y.train$hinc=='5']) #15
median(y.train$damt[y.train$hinc=='6']) #15
median(y.train$damt[y.train$hinc=='7']) #15

max(y.train$damt)




#look at wrat
counts6=table(x.train$donr,x.train$wrat)
par(mfrow=c(1,3))
barplot(counts6,main="Number of Donors by Wealth Rating",xlab="Wealth Rating",
        col=c("gray","darkblue"),legend=rownames(counts6))
boxplot(damt~wrat, data = x.train, col=c("lightsalmon","lightblue","khaki","darkseagreen1","plum1","snow1","antiquewhite","paleturquoise","palegoldenrod","mediumpurple1"),
        main="Amount Donated by Wealth Rating",xlab="Wealth Rating",ylab="Donation Amount")
boxplot(damt~wrat, data = y.train, col=c("lightsalmon","lightblue","khaki","darkseagreen1","plum1","snow1","antiquewhite","paleturquoise","palegoldenrod","mediumpurple1"),
        main="Amount Donated by Wealth Rating (donors only)",xlab="Wealth Rating",ylab="Donation Amount")

sum(y.train$wrat=='1')/sum(x.train$wrat=='1') #5.7%
sum(y.train$wrat=='4')/sum(x.train$wrat=='4') #33.7%
sum(y.train$wrat=='5')/sum(x.train$wrat=='5') #41.1%
sum(y.train$wrat=='6')/sum(x.train$wrat=='6') #55.4%
sum(y.train$wrat=='7')/sum(x.train$wrat=='7') #59.2%
sum(y.train$wrat=='8')/sum(x.train$wrat=='8') #56.6%
sum(y.train$wrat=='9')/sum(x.train$wrat=='9') #55.4%



#look at avhv
par(mfrow=c(1,3))
boxplot(avhv~donr, data = x.train, col=c("gray","darkblue"), 
        main="Avg. Home Value by Donor Type", xlab="Donor", ylab="Avg. Home Value")
hist(x.train$avhv[x.train$donr=='0'],main="Non-Donors Avg. Home Value", col="gray", ylim=c(0,700), xlab="Avg. Home Value")
hist(y.train$avhv, main="Donors Avg. Home Value", col="darkblue", xlab="Avg. Home Value")
summary(x.train$avhv[x.train$donr=='0'])
209+(209-129)*1.5 #$329K
summary(x.train$avhv[x.train$donr=='1'])
230.5+(230.5-139)*1.5 #$367.75K



#look at incm
par(mfrow=c(1,3))
boxplot(incm~donr, data = x.train, col=c("gray","darkblue"), 
        main="Median Income by Donor Type", xlab="Donor", ylab="Median Income")
hist(x.train$incm[x.train$donr=='0'],main="Non-Donors Median Income", col="gray",ylim=c(0,1000), xlab="Median Income")
hist(y.train$incm, main="Donors Median Income", col="darkblue",ylim=c(0,800), xlab="Median Income")

summary(x.train$incm[x.train$donr=='0'])
50+1.5*25
summary(x.train$incm[x.train$donr=='1'])
60+30*1.5
summary(x.train$inca[x.train$donr=='1'])
73+29*1.5 #116.5
summary(x.train$inca[x.train$donr=='0'])
64+25*1.5 #101.5





#look at inca
par(mfrow=c(1,3))
boxplot(inca~donr, data = x.train, col=c("gray","darkblue"), 
        main="Avg. Income by Donor Type", xlab="Donor", ylab="Avg. Income")
hist(x.train$inca[x.train$donr=='0'],main="Non-Donors Avg. Income", col="gray", xlab="Avg. Income")
hist(y.train$inca, main="Donors Avg. Income", col="darkblue",ylim=c(0,800), xlab="Avg. Income")



#look at plow
par(mfrow=c(1,3))
boxplot(plow~donr, data = x.train, col=c("gray","darkblue"), 
        main="% Low Income by Donor Type", xlab="Donor", ylab="% Low Income")
hist(x.train$plow[x.train$donr=='0'],main="Non-Donors % Low Income", col="gray",ylim=c(0,1000),xlim=c(0,100), xlab="% Low Income")
hist(y.train$plow, main="Donors % Low Income", col="darkblue",ylim=c(0,800),xlim=c(0,100), xlab="% Low Income")

summary(x.train$plow[x.train$donr=='0'])
22+17*1.5 #47.5%
summary(x.train$plow[x.train$donr=='1'])
18+15*1.5 #40.5%



#look at npro
par(mfrow=c(1,3))
boxplot(npro~donr, data = x.train, col=c("gray","darkblue"), 
        main="Number of Promotions by Donor Type", xlab="Donor", ylab="Number of Promotions")
hist(x.train$npro[x.train$donr=='0'],main="Non-Donors Number of Promotions", col="gray",ylim=c(0,250),xlab="Number of Promotions")
hist(y.train$npro, main="Donors Number of Promotions", col="darkblue",ylim=c(0,250), xlab="Number of Promotions")

mean(x.train$npro[x.train$donr=='0']) #57.5
median(x.train$npro[x.train$donr=='0']) #54
mean(x.train$npro[x.train$donr=='1']) #65.7
median(x.train$npro[x.train$donr=='1']) #64



#look at tgif,agif,rgif,lgif
par(mfrow=c(2,3))
boxplot(tgif~donr, data = x.train, col=c("gray","darkblue"), 
        main="Lifetime Gifts Amt. by Donor Type", xlab="Donor", ylab="Gift Amount ($)")
hist(x.train$tgif[x.train$donr=='0'],main="Non-Donors Lifetime Gift Amt.", col="gray", ylim=c(0,2000), xlab="Gift Amount ($)")
hist(y.train$tgif, main="Donors Lifetime Gift Amt.", col="darkblue", xlab="Gift Amount ($)")

boxplot(rgif~donr, data = x.train, col=c("gray","darkblue"), 
        main="Recent Gift Amt. by Donor Type", xlab="Donor", ylab="Gift Amount ($)")
hist(x.train$rgif[x.train$donr=='0'],main="Non-Donors Recent Gift Amt.", col="gray", ylim=c(0,1000), xlab="Gift Amount ($)")
hist(y.train$rgif, main="Donors Recent Gift Amt.", col="darkblue", xlab="Gift Amount ($)")

par(mfrow=c(2,3))
boxplot(lgif~donr, data = x.train, col=c("gray","darkblue"), 
        main="Largest Gift Amt. by Donor Type", xlab="Donor", ylab="Gift Amount ($)")
hist(x.train$lgif[x.train$donr=='0'],main="Non-Donors Largest Gift Amt.", col="gray", ylim=c(0,2000), xlab="Gift Amount ($)")
hist(y.train$lgif, main="Donors Largest Gift Amt.", col="darkblue",ylim=c(0,2000), xlab="Gift Amount ($)")

boxplot(agif~donr, data = x.train, col=c("gray","darkblue"), 
        main="Avg. Gift Amt. by Donor Type", xlab="Donor", ylab="Gift Amount ($)")
hist(x.train$agif[x.train$donr=='0'],main="Non-Donors Avg. Gift Amt.", col="gray", ylim=c(0,800), xlab="Gift Amount ($)")
hist(y.train$agif, main="Donors Avg. Gift Amt.", col="darkblue", xlab="Gift Amount ($)")


sum(x.train$agif<15)
sum(x.train$agif<5)
(3021-364)/3984


#look at tdon and tlag
par(mfrow=c(2,3))
counts8=table(x.train$donr,x.train$tlag)
counts9=table(x.train$donr,x.train$tdon)
barplot(counts8, main="Number of Months Between 1st & 2nd Gift", ylim=c(0,800),xlab="Number of Months",
        col=c("gray","darkblue"), legend=rownames(counts8))
boxplot(tlag~donr, data=x.train, main="Number of Months Between 1st & 2nd Gift",col=c("gray","darkblue"), ylab="Number of Months")
boxplot(tlag~donr, data=x.train, col=c("gray","darkblue"),main="Number of Months Between 1st & 2nd Gift (outliers removed)",outline=FALSE, ylab="Number of Months")
barplot(counts9, main="Number of Months Since Last Donation", ylim=c(0,400),xlab="Number of Months",
        col=c("gray","darkblue"),legend=rownames(counts9))
boxplot(tdon~donr, data=x.train, col=c("gray","darkblue"),main="Number of Months Since Last Donation", ylab="Number of Months")
boxplot(tdon~donr, data=x.train, col=c("gray","darkblue"),main="Number of Months Since Last Donation (outliers removed)", ylab="Number of Months")


sum(x.train$tlag=='3') #647
sum(x.train$tlag=='4') #653
sum(x.train$tlag=='5') #674
sum(x.train$tlag=='6') #645
sum(x.train$tlag=='7') #516
(647+653+674+645+516)/3984
sum(x.train$tdon<24)
sum(x.train$tdon<12)
3503-173 #3330
3330/3984
summary(x.train$tdon[x.train$donr=='0'])
summary(x.train$tdon[x.train$donr=='1'])



?plot_scatterplot
plot_scatterplot(x.train,"damt", size = 0.5)
plot_scatterplot(x.train,"incm", size = 0.5)
#incm & plow and inca & plow both show a curvilinear relationship














#------------------------------------------------------------------------------------------------
##### CLASSIFICATION MODELS ######

# linear discriminant analysis
model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()
# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model


model.lda2=lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif +rgif +I(rgif^2) +tdon+ I(tdon^2) + tlag + agif, 
                  data.train.std.c) 

model.lda3=lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                 avhv + incm + inca + plow + npro + tgif + lgif +rgif +I(rgif^3) +tdon+ I(tdon^2) + tlag + agif, 
               data.train.std.c) 


post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs
post.valid.lda2 <- predict(model.lda2, data.valid.std.c)$posterior[,2] # n.valid.c post probs
post.valid.lda3 <- predict(model.lda3, data.valid.std.c)$posterior[,2] # n.valid.c post probs


# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
par(mfrow=c(1,1))
plot(profit.lda1, ylab="Profit",xlab="Mailings") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
cutoff.lda1
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
# check n.mail.valid 
344+985 #1329
#check profit
14.5*985-2*1329   #11624.5



###repeat for lda2
profit.lda2=cumsum(14.5*c.valid[order(post.valid.lda2, decreasing=T)]-2)
plot(profit.lda2, ylab="Profit",xlab="Mailings") # see how profits change as more mailings are made
n.mail.valid2=which.max(profit.lda2) # number of mailings that maximizes profits
c(n.mail.valid2, max(profit.lda2)) # report number of mailings and maximum profit
# 1322 11667.5

cutoff.lda2=sort(post.valid.lda2, decreasing=T)[n.mail.valid2+1] # set cutoff based on n.mail.valid2
cutoff.lda2
chat.valid.lda2=ifelse(post.valid.lda2>cutoff.lda2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda2, c.valid) # classification table
# check n.mail.valid 
335+987 #1322
#check profit
14.5*987-2*1322   #11667.5



###repeat for lda3
profit.lda3=cumsum(14.5*c.valid[order(post.valid.lda3, decreasing=T)]-2)
plot(profit.lda3, ylab="Profit",xlab="Mailings", main="Expected Profit versus Number of Mailings") # see how profits change as more mailings are made
n.mail.valid3=which.max(profit.lda3) # number of mailings that maximizes profits
c(n.mail.valid3, max(profit.lda3)) # report number of mailings and maximum profit
# 1369 11675

cutoff.lda3=sort(post.valid.lda3, decreasing=T)[n.mail.valid3+1] # set cutoff based on n.mail.valid2
cutoff.lda3
chat.valid.lda3=ifelse(post.valid.lda3>cutoff.lda3, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda3, c.valid) # classification table
# check n.mail.valid 
375+994 #1369
#check profit
14.5*994-2*1369   #11675
994/1369
375/1369



model.lda3
plot(model.lda3, main="Distributions of Donors and Non-Donors")
prediction(post.valid.lda3, data.valid.std.c$donr) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(main="Linear Discriminant Analysis ROC Curve")


####################
# logistic regression
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))


model.log2=glm(donr ~ reg1 + reg2 + home + chld + hinc + I(hinc^2) + wrat + incm + plow + npro + tlag,
                  data.train.std.c, family=binomial("logit"))

model.log3=glm(donr ~ reg1 + reg2 + home + chld + hinc + I(hinc^2) + wrat + incm + plow + npro + tlag +tdon +I(tdon^2),
               data.train.std.c, family=binomial("logit"))

summary(model.log1)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs
profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.validlg1 <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.validlg1, max(profit.log1)) # report number of mailings and maximum profit
# 1291 11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.validlg1+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
310+981 #1291
14.5*981-2*1291 #11642.5



# repeat for log2
post.valid.log2=predict(model.log2, data.valid.std.c, type="response") # n.valid post probs
profit.log2=cumsum(14.5*c.valid[order(post.valid.log2, decreasing=T)]-2)
plot(profit.log2) # see how profits change as more mailings are made
n.mail.validlg2=which.max(profit.log2) # number of mailings that maximizes profits
c(n.mail.validlg2, max(profit.log2)) # report number of mailings and maximum profit
# 1320 11628

cutoff.log2=sort(post.valid.log2, decreasing=T)[n.mail.validlg2+1] # set cutoff based on n.mail.valid
chat.valid.log2=ifelse(post.valid.log2>cutoff.log2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log2, c.valid) # classification table
336+984
14.5*984-2*1320 #11628



#repeat for log3
post.valid.log3=predict(model.log3, data.valid.std.c, type="response") # n.valid post probs
profit.log3=cumsum(14.5*c.valid[order(post.valid.log3, decreasing=T)]-2)
plot(profit.log3, xlab="Mailings",ylab="Profit",main="Expected Profit versus Number of Mailings") # see how profits change as more mailings are made
n.mail.validlg3=which.max(profit.log3) # number of mailings that maximizes profits
c(n.mail.validlg3, max(profit.log3)) # report number of mailings and maximum profit
# 1362 11689

cutoff.log3=sort(post.valid.log3, decreasing=T)[n.mail.validlg3+1] # set cutoff based on n.mail.valid
chat.valid.log3 <- ifelse(post.valid.log3>cutoff.log3, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log3, c.valid) # classification table
368+994 #1362
14.5*994-2*1362 #11689
994/1362
368/1362

summary(model.log3)
exp(coef(model.log3))



###################
##quadratic discriminant analysis
model.qda1=qda(donr ~ reg1 + reg2 + reg3 + reg4 + home +
                    chld + hinc  + genf + wrat + avhv + 
                    incm + inca + plow + npro + tgif + 
                    lgif + rgif + tdon + tlag + agif,
                  data.train.std.c)

model.qda2=qda(donr ~ reg1 + reg2 + reg3 + reg4 + home +
                 chld + hinc +I(hinc^2) + genf + wrat + avhv + 
                 incm + inca + plow + npro + tgif + 
                 lgif + rgif +I(rgif^2)+ tdon + tlag + agif,
               data.train.std.c)

model.qda3=qda(donr ~ reg1 + reg2 + reg3 + reg4 + home +
                 chld + hinc +I(hinc^2) + genf + wrat + avhv + 
                 incm + inca + plow + npro + tgif + 
                 lgif + rgif +I(rgif^3)+ tdon+I(tdon^2) + tlag + agif,
               data.train.std.c)

post.valid.qda1=predict(model.qda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs


# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.qda1=cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
plot(profit.qda1, ylab="Profit",xlab="Mailings", main="Expected Profit versus Number of Mailings")
n.mail.validqda1=which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.validqda1, max(profit.qda1)) # report number of mailings and maximum profit
#1378 11251
cutoff.qda1=sort(post.valid.qda1, decreasing=T)[n.mail.validqda1+1] # set cutoff based on # n.mail.valid
chat.valid.qda1=ifelse(post.valid.qda1>cutoff.qda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda1, c.valid) # classification table

412+966 #1378
14.50*966-2*1378 #11251
966/1378
412/1378

plot(model.qda1, main="Distributions of Donors and Non-Donors")
prediction(post.valid.qda1, data.valid.std.c$donr) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(main="Quadratic Discriminant Analysis ROC Curve")




#repeat for qda2
post.valid.qda2=predict(model.qda2, data.valid.std.c)$posterior[,2] # n.valid.c post probs
profit.qda2=cumsum(14.5*c.valid[order(post.valid.qda2, decreasing=T)]-2)
plot(profit.qda2, ylab="Profit",xlab="Mailings")
n.mail.validqda2=which.max(profit.qda2) # number of mailings that maximizes profits
c(n.mail.validqda2, max(profit.qda2)) # report number of mailings and maximum profit
#1380 11218
cutoff.qda2=sort(post.valid.qda2, decreasing=T)[n.mail.validqda2+1] # set cutoff based on # n.mail.valid
chat.valid.qda2=ifelse(post.valid.qda2>cutoff.qda2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda2, c.valid) # classification table

416+964 #1380
14.50*964-2*1380 #11218


#repeat for qda3
post.valid.qda3=predict(model.qda3, data.valid.std.c)$posterior[,2] # n.valid.c post probs
profit.qda3=cumsum(14.5*c.valid[order(post.valid.qda3, decreasing=T)]-2)
plot(profit.qda3, ylab="Profit",xlab="Mailings")
n.mail.validqda3=which.max(profit.qda3) # number of mailings that maximizes profits
c(n.mail.validqda3, max(profit.qda3)) # report number of mailings and maximum profit
#1296 11125
cutoff.qda3=sort(post.valid.qda3, decreasing=T)[n.mail.validqda3+1] # set cutoff based on # n.mail.valid
chat.valid.qda3=ifelse(post.valid.qda3>cutoff.qda3, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda3, c.valid) # classification table

350+946 #1296
14.50*946-2*1296 #11125




###################
##random forest

y=ifelse(c.train==0,"n_donor","donor")
y=factor(y)

y.val=ifelse(c.valid==0, "n_donor","donor")
y.val=factor(y.val)


grid.search=expand.grid(mtry=3) 

control=trainControl(method = "cv",number = 4,verboseIter = FALSE,
                        returnData = FALSE,returnResamp = "all",classProbs = TRUE, 
                        summaryFunction = twoClassSummary,allowParallel = TRUE)

set.seed(1)
rf.mod1=train(x=x.train.std, y=y, trControl = control,
                tuneGrid = grid.search, metric = "ROC", method = "rf")                 

rf.validate1=predict(rf.mod1, x.valid.std)
table(true=y.val, predicted=rf.validate1)
(912+873)/2018
#0.885

# Estimate Profit on Validation
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
rf.probs1=predict(rf.mod1, x.valid.std, type = "prob")[,1]
rf.profit1=cumsum(14.5*c.valid[order(rf.probs1, decreasing=T)]-2)
n.mail.validrf1=which.max(rf.profit1)
c(n.mail.validrf1,max(rf.profit1))
# 1245  11749

rf.mod1
summary(rf.mod1)
?varImp
varImp(rf.mod1, useModel=TRUE, sort=TRUE)
plot(varImp(rf.mod1), main = "Random Forest")




########################
##Boosting
set.seed(1)
boost.mod1=gbm(donr~., data=data.train.std.c, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
set.seed(1)
boost.probs1=predict.gbm(boost.mod1, newdata = data.valid.std.c,
                          n.trees = 5000, type = "response")
boost.pred = rep("0", 2018)
boost.pred[boost.probs1 > .5] = "1"
table(boost.pred, c.valid)
profit.boost1=cumsum(14.5 * c.valid[order(boost.probs1, decreasing = T)] - 2)
n.mail.valid.boost1=which.max(profit.boost1)
c(n.mail.valid.boost1,max(profit.boost1))
# 1274 11836



###try using lda3 equation
set.seed(1)
boost.mod2=gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc 
               + I(hinc^2) + genf + wrat + avhv + incm + inca + plow 
               + npro + tgif + lgif +rgif +I(rgif^3) +tdon+ I(tdon^2) 
               + tlag + agif, data = data.train.std.c, distribution = "bernoulli", 
               n.trees = 5000, interaction.depth = 4)

set.seed(1)
boost.probs2=predict.gbm(boost.mod2, newdata = data.valid.std.c,
                           n.trees = 5000, type = "response")
boost.pred2=rep("0", 2018)
boost.pred2[boost.probs2 > .5] = "1"
table(boost.pred2 , c.valid)
profit.boost2=cumsum(14.5 * c.valid[order(boost.probs2, decreasing = T)] - 2)
n.mail.valid.boost2=which.max(profit.boost2)
c(n.mail.valid.boost2,max(profit.boost2))
# 1233 11874.5

summary(boost.mod2)
(923)



########################
##SVM Model
svm.dat=data.frame(y, x.train.std)

svm.mod1=svm(y~., data=svm.dat, kernel = "radial", gamma = 0.02,
               cost = 7,scale = FALSE,fitted = TRUE,probability = TRUE,decision.values = TRUE)

svm.mod2=svm(y~., data=svm.dat, kernel = "linear", cost = 7,
             scale = FALSE,fitted = TRUE,probability = TRUE,decision.values = TRUE)


par(mfrow = c(1,2))
plot(svm.mod1, svm.dat, hinc~chld)
plot(svm.mod1, svm.dat)

svm.pred1=predict(svm.mod1, x.valid.std)
table(y.val, svm.pred1)
(911 + 867)/2018
(911+88+152+867)
# 0.88107


# Estimate Profit on Validation
#calculate ordered profit function using average donation = $14.50 and mailing cost = $2
svm.probs1=predict(svm.mod1, x.valid.std, probability = TRUE)
svm.probs1=attr(svm.probs1, "probabilities")[,2]
svm.profit1=cumsum(14.5*c.valid[order(svm.probs1, decreasing=T)]-2)
n.mail.valid.svm1=which.max(svm.profit1)
c(n.mail.valid.svm1, max(svm.profit1))
# 1336  11712

summary(svm.mod1)




#repeat with SVM2
svm.pred2=predict(svm.mod2, x.valid.std)
table(y.val, svm.pred2)
(866+826)/2018
# Estimate Profit on Validation
#calculate ordered profit function using average donation = $14.50 and mailing cost = $2
svm.probs2=predict(svm.mod2, x.valid.std, probability = TRUE)
svm.probs2=attr(svm.probs2, "probabilities")[,2]
svm.profit2=cumsum(14.5*c.valid[order(svm.probs2, decreasing=T)]-2)
n.mail.valid.svm2=which.max(svm.profit2)
c(n.mail.valid.svm2, max(svm.profit2))



set.seed(1)
tune.out1=tune(svm,y~.,data=svm.dat,kernel="radial",
               ranges=list(cost=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2.5, 5, 7.5,10,55,100)))
summary(tune.out1)
bestmod1=tune.out1$best.model
summary(bestmod1)

svm.mod3=svm(y~., data=svm.dat, kernel = "radial", gamma = 0.05,
             cost = 2.5,scale = FALSE,fitted = TRUE,probability = TRUE,decision.values = TRUE)


svm.pred3=predict(svm.mod3, x.valid.std)
table(y.val, svm.pred3)
(906+865)/2018  #87.76%
# Estimate Profit on Validation
#calculate ordered profit function using average donation = $14.50 and mailing cost = $2
svm.probs3=predict(svm.mod3, x.valid.std, probability = TRUE)
svm.probs3=attr(svm.probs3, "probabilities")[,2]
svm.profit3=cumsum(14.5*c.valid[order(svm.probs3, decreasing=T)]-2)
n.mail.valid.svm3=which.max(svm.profit3)
c(n.mail.valid.svm3, max(svm.profit3))





########################
##Neural Net
model.nnet1=nnet(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
                      I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + 
                      tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c, 
                    size = 2)

model.nnet2=nnet(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
                      I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + 
                      tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c, 
                    size = 3)

model.nnet3=nnet(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
                   I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + 
                   tgif + lgif + rgif +I(rgif^3) + tdon +I(tdon^2) + tlag + agif, data.train.std.c, 
                 size = 3)


post.valid.nnet1=predict(model.nnet1, data.valid.std.c)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.nnet1=cumsum(14.5*c.valid[order(post.valid.nnet1, decreasing = T)] - 2)
plot(profit.nnet1) # see how profits change as more mailings are made
n.mail.valid.nn1=which.max(profit.nnet1) # number of mailings that maximizes profits
c(n.mail.valid.nn1, max(profit.nnet1)) # report number of mailings and maximum profit
# 1131 10773.5

cutoff.nn1=sort(post.valid.nnet1, decreasing=T)[n.mail.valid.nn1+1] # set cutoff based on n.mail.valid
chat.valid.nn1=ifelse(post.valid.nnet1 > cutoff.nn1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.nn1, c.valid) # classification table



##repeat with NN2
post.valid.nnet2=predict(model.nnet2, data.valid.std.c)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.nnet2=cumsum(14.5*c.valid[order(post.valid.nnet2, decreasing = T)] - 2)
plot(profit.nnet2) # see how profits change as more mailings are made
n.mail.valid.nn2=which.max(profit.nnet2) # number of mailings that maximizes profits
c(n.mail.valid.nn2, max(profit.nnet2)) # report number of mailings and maximum profit
# 1125 11017.5

cutoff.nn2=sort(post.valid.nnet2, decreasing=T)[n.mail.valid.nn2+1] # set cutoff based on n.mail.valid
chat.valid.nn2=ifelse(post.valid.nnet2 > cutoff.nn2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.nn2, c.valid) # classification table



##repeat with NN3
post.valid.nnet3=predict(model.nnet3, data.valid.std.c)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.nnet3=cumsum(14.5*c.valid[order(post.valid.nnet3, decreasing = T)] - 2)
plot(profit.nnet3) # see how profits change as more mailings are made
n.mail.valid.nn3=which.max(profit.nnet3) # number of mailings that maximizes profits
c(n.mail.valid.nn3, max(profit.nnet3)) # report number of mailings and maximum profit
#1244 11388.5

cutoff.nn3=sort(post.valid.nnet3, decreasing=T)[n.mail.valid.nn3+1] # set cutoff based on n.mail.valid
chat.valid.nn3=ifelse(post.valid.nnet3 > cutoff.nn3, 1, 0) # mail to everyone above the cutoff
table(chat.valid.nn3, c.valid) # classification table

summary(model.nnet3)
model.nnet3
library(devtools)
library(NeuralNetTools)
plotnet(model.nnet3)





####################
##KNN
set.seed(1)
knn.mod1=knn(data.train.std.c, data.valid.std.c, c.train,k=10)
set.seed(1)
knn.mod2=knn(data.train.std.c, data.valid.std.c, c.train,k=12)


mean(c.valid!=knn.mod1)
mean(c.valid!="No")

table(knn.mod1,c.valid)
table(knn.mod2,c.valid)

knn.profit1=cumsum(14.5*c.valid[order(knn.mod1, decreasing=T)]-2)
n.mail.valid.knn1=which.max(knn.profit1) # number of mailings that maximizes profits
c(n.mail.valid.knn1, max(knn.profit1))
# 1150  11750.5

knn.profit2=cumsum(14.5*c.valid[order(knn.mod2, decreasing=T)]-2)
n.mail.valid.knn2=which.max(knn.profit2) # number of mailings that maximizes profits
c(n.mail.valid.knn2, max(knn.profit2))
# 1168  11758
# 1171  11781
(815+972)/2018




###---------------------------- Results------------------------------------------###
# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1322   11667.5 LDA2
# 1369   11675   LDA3
# 1321   11640.5 Log1
# 1320   11628   Log2
# 1362   11689   Log3
# 1399   11238   QDA1
# 1380   11218   QDA2
# 1296   11125   QDA3
# 1214   11724   RF1  
# 1274   11836   Boost1
# 1233   11874.5 Boost2 ***winner w/highest profit
# 1336   11712   SVM1
# 1420   11384.5 SVM2
# 1323   11665.5 SVM3
# 1311   10773.5 NN1
# 1125   11017.5 NN2
# 1244   11388.5 NN3
# 1150   11750.5 KNN1  
# 1168   11758   KNN2




###---------------------model selection and applying to test set-----------------------###
# select Boost2 since it has maximum profit in the validation sample
set.seed(1)
post.test=predict.gbm(boost.mod2, newdata = data.test.std,
                        n.trees = 5000, type = "response")


# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid=which.max(profit.boost2)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1709  298
# based on this model we'll mail to the 298 highest posterior probabilities

# See below for saving chat.test into a file for submission


281*14.50
4074.50-(2*281)
(298*14.50)-(2*298)


#-------------------------------------------------------------------------------------------------------
##### PREDICTION MODELING ######

# Least squares regression
model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

summary(model.ls1)
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615

# drop non-significant variables
model.ls2 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + genf +
                  incm + plow + npro + rgif + tdon + agif, 
                data.train.std.y)
summary(model.ls2)
pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.870988
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.17019

par(mfrow=c(2,2))
plot(model.ls2)


########################
## Partial Least Squares
set.seed(1)
pls.mod1=plsr(damt~reg1 + reg2 + reg3 + reg4 + home + chld + hinc +
                 wrat + genf + avhv + incm + inca + plow + npro + tgif +
                 lgif + rgif + tdon + tlag + agif, data = data.train.std.y,
               scale = TRUE, validation = "CV") 
summary(pls.mod1)
validationplot(pls.mod1, val.type="MSEP", type = "b")
# start with 8 components
set.seed(1)
pls.pred1=predict(pls.mod1, data.valid.std.y, ncomp=8)
mean((y.valid - pls.pred1)^2)
sd((y.valid - pls.pred1)^2)/sqrt(n.valid.y)


#try 6 components
set.seed(1)
pls.pred2=predict(pls.mod1, data.valid.std.y, ncomp=6)
mean((y.valid - pls.pred2)^2)
# 1.868145
sd((y.valid - pls.pred2)^2)/sqrt(n.valid.y)



#########################
##Neural Nets
nnet1=nnet(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
                I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + 
                tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.y, 
              size = 2, linout = TRUE)
pred.nnet1=predict(nnet1, newdata = data.valid.std.y)
mean((y.valid - pred.nnet1)^2)
#3.3869
sd((y.valid - pred.nnet1)^2)/sqrt(n.valid.y)
# 0.64618

summary(nnet1)
nnet1


nnet2=nnet(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
                I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + 
                tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.y, 
              size = 3, linout = TRUE)
pred.nnet2=predict(nnet2, newdata = data.valid.std.y)
mean((y.valid - pred.nnet2)^2)
# 1.604106
sd((y.valid - pred.nnet2)^2)/sqrt(n.valid.y)
#0.1699067




######################
##Random Forest
set.seed(1)
rf.reg1=randomForest(damt~., data=data.train.std.y, importance=TRUE)
rf.reg1
set.seed(1)
pred.reg1=predict(rf.reg1, newdata = data.valid.std.y)
mean((y.valid - pred.reg1)^2)
#1.672363
sd((y.valid - pred.reg1)^2/sqrt(n.valid.y))
#0.172274



####RF2
grid.search=expand.grid(mtry = 5) 
control=trainControl(method = "cv",
                        number = 4,
                        verboseIter = FALSE,
                        returnData = FALSE,
                        returnResamp = "all",
                        allowParallel = TRUE)
set.seed(1)
rf.reg2=train(damt ~ .,
                data = data.train.std.y,
                trControl = control,
                tuneGrid = grid.search,
                importance = TRUE,
                metric = "RMSE",
                method = "rf")                 

rf.reg2
summary(rf.reg2)
pred.reg2=predict(rf.reg2,data.valid.std.y)
mean((y.valid - pred.reg2)^2)
#1.656952
sd((y.valid-pred.reg2)^2/sqrt(n.valid.y))
#0.1732884





###################
##Bagging
set.seed(1)
bag.mod1=randomForest(damt~., data = data.train.std.y, mtry=20,
                        importance=TRUE)
bag.mod1
set.seed(1)
pred.bag1=predict(bag.mod1, newdata=data.valid.std.y)
mean((y.valid - pred.bag1)^2)
# 1.70452
sd((y.valid - pred.bag1)^2/sqrt(n.valid.y))
#0.1746157


##Bag2
set.seed(1)
bag.mod2=randomForest(damt~., data=data.train.std.y, mtry=20, ntree=50)
bag.mod2
set.seed(1)
pred.bag2=predict(bag.mod2,newdata=data.valid.std.y)
mean((y.valid-pred.bag2)^2)




###################
##Boosting
set.seed(1)
boost.mod1=gbm(damt~.,data=data.train.std.y,distribution="gaussian",
               n.trees=5000, interaction.depth=4)
summary(boost.mod1)
set.seed(1)
boost.pred1=predict.gbm(boost.mod1, newdata = data.valid.std.y,
                         n.trees = 5000, type = "response")
mean((y.valid-boost.pred1)^2)
#1.539818



##Boost2
set.seed(1)
boost.mod2=gbm(damt~reg1 + reg2 + reg3 + reg4 + home + chld + 
                   hinc + I(hinc^2) + genf + wrat + avhv + incm + 
                   inca + plow + npro + tgif + lgif + rgif + tdon + 
                   tlag + agif, data = data.train.std.y, distribution = "gaussian", 
                 shrinkage = 0.1,n.trees = 1000, interaction.depth = 4)
summary(boost.mod2) #rgif, lgif, agif and reg4 are important variables
set.seed(1)
boost.pred2=predict.gbm(boost.mod2, newdata = data.valid.std.y, type = "response", n.trees = 1000)
mean((y.valid-boost.pred2)^2)
#1.539335



##Boost3
set.seed(1)
boost3=gbm(damt~reg1 + reg2 + reg3 + reg4 + home + chld + 
                 hinc + I(hinc^2) + genf + wrat + avhv + incm + 
                 inca + plow + npro + tgif + lgif + rgif + tdon + 
                 tlag + agif, data = data.train.std.y, distribution = "gaussian", 
               shrinkage = 0.1,n.trees = 500, interaction.depth = 4)
summary(boost3) #rgif, lgif, agif and reg4 are important variables
boost3
set.seed(1)
boost.pred3=predict.gbm(boost3, newdata = data.valid.std.y, type = "response", n.trees = 500)
mean((y.valid-boost.pred3)^2)
#1.464606

###---------------------------------------Results--------------------------------------------###

# MPE  Model
# 1.867523 LS1
# 1.867433 LS2
# 1.868145 PLS1
# 1.672363 RF1
# 1.656952 RF2
# 1.604106 NN2
# 1.70452  BAG2
# 1.539818 Boost1
# 1.539335 Boost2
# 1.464606 Boost3  ***lowest MPE


# select Boost model 3 since it has minimum mean prediction error in the validation sample
set.seed(1)
yhat.test=predict.gbm(boost3, newdata = data.test.std,
                         n.trees=500, type="response") # test predictions

?predict.gbm

###---------------------------------FINAL RESULTS-------------------------------------------###

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="HM.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file for evaluation based on actual test donr and damt values

summary(ip)
sum(ip$chat=='1')
sum(ip$yhat[ip$chat=='1'])
View(ip)