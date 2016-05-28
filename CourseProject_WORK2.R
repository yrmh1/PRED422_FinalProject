## PRED 422 - Course Final Project WORK FIle ##
library(corrplot)
library(MASS)
library(ISLR)
library(bestglm)
library(rmarkdown)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(ada)
##########
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

############ Load "charity.csv" data file ############
charity <- read.csv(file.choose())

######## Create categorical variables for wrat:########
charity$wrat1<-ifelse(charity$wrat>0 & charity$wrat<2, 1, 0)
charity$wrat2<-ifelse(charity$wrat>1 & charity$wrat<3, 1, 0)
charity$wrat3<-ifelse(charity$wrat>2 & charity$wrat<4, 1, 0)
charity$wrat4<-ifelse(charity$wrat>3 & charity$wrat<5, 1, 0)
charity$wrat5<-ifelse(charity$wrat>4 & charity$wrat<6, 1, 0)
charity$wrat6<-ifelse(charity$wrat>5 & charity$wrat<7, 1, 0)
charity$wrat7<-ifelse(charity$wrat>6 & charity$wrat<8, 1, 0)
charity$wrat8<-ifelse(charity$wrat>7 & charity$wrat<9, 1, 0)
charity$wrat9<-ifelse(charity$wrat>8 & charity$wrat<10, 1, 0)

######## Create categorical variables for chld:########
charity$chld1<-ifelse(charity$chld>0 & charity$chld<2, 1, 0)
charity$chld2<-ifelse(charity$chld>1 & charity$chld<3, 1, 0)
charity$chld3<-ifelse(charity$chld>2 & charity$chld<4, 1, 0)
charity$chld4<-ifelse(charity$chld>3 & charity$chld<5, 1, 0)
charity$chld5<-ifelse(charity$chld>4 & charity$chld<6, 1, 0)

######## Create categorical variables for hinc:########
charity$hinc2<-ifelse(charity$hinc>1 & charity$hinc<3, 1, 0)
charity$hinc3<-ifelse(charity$hinc>2 & charity$hinc<4, 1, 0)
charity$hinc4<-ifelse(charity$hinc>3 & charity$hinc<5, 1, 0)
charity$hinc5<-ifelse(charity$hinc>4 & charity$hinc<6, 1, 0)
charity$hinc6<-ifelse(charity$hinc>5 & charity$hinc<7, 1, 0)
charity$hinc7<-ifelse(charity$hinc>6 & charity$hinc<8, 1, 0)

######## Remove old variable4s: ########
deletevars <- names(charity) %in% c("chld","hinc","wrat")
charity <- charity[!deletevars]

######### Create density plots of variables w/skewed distributions #############
# 3 rows, 3 columns
par(mfrow=c(3,3))
######################
# incm - 12 Higly right-skewed towards less wealthy...
x=charity$incm
x.nm=names(charity[12])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# inca -  13 same as above...
x=charity$inca
x.nm=names(charity[13])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# plow -  14 same as above...
x=charity$plow
x.nm=names(charity[14])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# tgif -  16 Higly right-skewed...
x=charity$tgif
x.nm=names(charity[16])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# lgif -  17 same as above...
x=charity$lgif
x.nm=names(charity[17])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# rgif -  18 same as above...
x=charity$rgif
x.nm=names(charity[18])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# tlag -  20 Higly right-skewed...
x=charity$tlag
x.nm=names(charity[20])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# agif - 21 Higly right-skewed...
x=charity$agif
x.nm=names(charity[21])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")


######## Log x-forms of predictors w/heavily skewed distributions ################
charity.t <- charity
# Add 1 to eliminate negative #s tranformed to Inf values...
charity.t$avhv <- log(charity.t$avhv+1)
charity.t$incm <- log(charity.t$incm+1)
charity.t$inca <- log(charity.t$inca+1)
charity.t$plow <- log(charity.t$plow+1)
charity.t$tgif <- log(charity.t$tgif+1)
charity.t$lgif <- log(charity.t$lgif+1)
charity.t$rgif <- log(charity.t$rgif+1)
charity.t$tlag <- log(charity.t$tlag+1)
charity.t$agif <- log(charity.t$agif+1)

# Add further transformations if desired...
# Use poly(x,2) for polynomial xform or log(x) for log xform...
# What about interaction terms?  lm(y~.+ x1:x2 + x3:x4,data=dataframe )


#################################################################################
############ Density plots of log x-formed variables ################
######################
# incm - 12 Higly right-skewed towards less wealthy...
x=charity.t$incm
x.nm=names(charity.t[12])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# inca -  13 same as above...
x=charity.t$inca
x.nm=names(charity.t[13])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# plow -  14 same as above...
x=charity.t$plow
x.nm=names(charity.t[14])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# tgif -  16 Higly right-skewed...
x=charity.t$tgif
x.nm=names(charity.t[16])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# lgif -  17 same as above...
x=charity.t$lgif
x.nm=names(charity.t[17])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# rgif -  18 same as above...
x=charity.t$rgif
x.nm=names(charity.t[18])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# tlag -  20 Higly right-skewed...
x=charity.t$tlag
x.nm=names(charity.t[20])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")
######################
# agif - 21 Higly right-skewed...
x=charity.t$agif
x.nm=names(charity.t[21])
# Density Plot
d <- density(x)
plot(d, main=paste("Kernel Density: ",x.nm))
polygon(d, col="red", border="blue")


# 1 row, 1 column
par(mfrow=c(1,1))

########################################################################################
# set up training data for analysis
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,c(2:18,22:41)]
c.train <- data.train[,19] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,20] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

x.train.mean <- apply(x.train, 2, mean) # Get mean warnings()
x.train.sd <- apply(x.train, 2, sd) # Get sd
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd

apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd

data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

#############################################################
# set up validation data
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,c(2:18,22:41)]
c.valid <- data.valid[,19] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,20] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd

data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

#############################################################
# set up test data
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,c(2:18,22:41)]

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)


########################################################################################
# Take a look...
#dim(data.train)
#summary(data.train)
#str(data.train)

# Keep only numeric predictor variables for correlation plot:
#data.train.corr <- data.train[c(-1,-24)]
# A cool correlation plot:
#corrplot(cor(data.train.corr))
corrplot(cor(x.train))
# Eliminate incm,inca,plow - keep avhv
# Eliminate npro - keep tgif
# Eliminate lgif,agif - keep rgif

#x.train.cor<-subset(x.train,select=c(reg1,reg2,reg3,reg4,home,chld,hinc,genf,wrat,avhv,tgif,rgif,tdon,tlag))
deletevars <- names(x.train) %in% c("incm","inca","plow","npro","lgif","agif")
x.train.cor <- x.train[!deletevars]

corrplot(cor(x.train.cor))
# plow (low income) highly correlated w/incm, inca & avhv - remove some variables?
# inca highly correlated w/incm & avhv
# ---------------------------------------
# npro highly correlated w/tgif --> OK
# lgif highly correlated w/rgif --> OK?
# agif highly correlated w/lgif & rgif --> select one?

# chld (# of children) highly (negatively) correlated w/response variables donr & damt - interesting!
# Let's look at that...
# fit1=lm(damt~chld,data=data.train.corr)
# summary(fit1)
# plot(data.train.corr$chld,data.train.corr$damt)
# abline(fit1)
# Ordinal variable chld not appropriate for a lm, maybe use dummy vars instead?

########################################################################################
################ Decision Tree ############
########################################################################################
model.tree1<-rpart(donr~.,data=data.train.std.c,xval=10,method="class")
fancyRpartPlot(model.tree1)
plot(model.tree1)
text(model.tree1)


########################################################################################
################ Logistic Regression Best subsets selection ############
########################################################################################
# Just try this:
#library(bestglm)
#xc.train<-cbind(x.train,c.train)
xc.train<-cbind(x.train.cor,c.train)
#xc.train<-subset(xc.train, select = c(reg1,reg2,reg3,reg4,home,chld,hinc,genf,wrat,avhv,tgif,rgif,tdon,tlag,c.train))
xc.train<-subset(data.train.std.c, select = c(reg1,reg2,reg3,reg4,home,chld,hinc,genf,wrat,avhv,tgif,rgif,tdon,tlag,donr))
best.logistic<-bestglm(Xy = xc.train,IC="BIC",family=binomial)

# Output:  Best model:  # reg1,reg2,home,chld,wrat,avhv,tgif,tdon,tlag
best.logistic # Best model coeffs, SE, z & P values - USE THIS ONE...
best.logistic$Subsets # All subsets w/loglokelihood & BIC...
summary(best.logistic) # Likelihood-ratio hypothesis test, etc.
best.logistic$BestModel # Just best coeffs...
best.logistic$BestModels # 5 best models...
best.logistic$Bestq # ?
best.logistic$qTable # ?
best.logistic$Title # ?
best.logistic$ModelReport # ?


########################################################################################
############################# CLASSIFICATION MODELING ##################################
########################################################################################
# See Mod 5 Lab.r for polynomial classification model example...
# See Mod 6 Lab.r for QDA & KNN classification models...

########################################################################################
############ linear discriminant analysis ###########

#library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c)

# include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

#######################################################################################
############ linear discriminant analysis 2 ###########
# Use: reg1,reg2,reg3,reg4,home,chld,hinc,I(hinc^2),wrat,I(wrat^2),avhv,tgif,tdon,tlag
model.lda2 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + wrat + I(wrat^2) + 
                    avhv + tgif + tdon + tlag,data.train.std.c)

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda2 <- predict(model.lda2, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda2 <- cumsum(14.5*c.valid[order(post.valid.lda2, decreasing=T)]-2)
plot(profit.lda2) # see how profits change as more mailings are made
n.mail.valid2 <- which.max(profit.lda2) # number of mailings that maximizes profits
c(n.mail.valid2, max(profit.lda2)) # report number of mailings and maximum profit
# 1308.0 11666.5

cutoff.lda2 <- sort(post.valid.lda2, decreasing=T)[n.mail.valid2+1] # set cutoff based on n.mail.valid
chat.valid.lda2 <- ifelse(post.valid.lda2>cutoff.lda2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda2, c.valid) # classification table

#               c.valid
#chat.valid.lda2   0   1
#              0 696  14
#              1 323 985
# check n.mail.valid = 323+985 = 1308
# check profit = 14.5*985-2*1308 = 11666.5



#######################################################################################
############ linear discriminant analysis 3 ###########
# Use: reg1,reg2,reg3,reg4,home,chld,hinc,I(hinc^2),wrat,I(wrat^2),avhv,tgif,tdon,tlag
model.lda3 <- lda(donr ~.,data.train.std.c)

post.valid.lda3 <- predict(model.lda3, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda3 <- cumsum(14.5*c.valid[order(post.valid.lda3, decreasing=T)]-2)
plot(profit.lda3) # see how profits change as more mailings are made
n.mail.valid3 <- which.max(profit.lda3) # number of mailings that maximizes profits
c(n.mail.valid3, max(profit.lda3)) # report number of mailings and maximum profit
# 1255.0 11830.5

cutoff.lda3 <- sort(post.valid.lda3, decreasing=T)[n.mail.valid3+1] # set cutoff based on n.mail.valid
chat.valid.lda3 <- ifelse(post.valid.lda3>cutoff.lda3, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda3, c.valid) # classification table

#               c.valid
#chat.valid.lda3   0   1
#              0 753  10
#              1 266 989
# check n.mail.valid3 = 266+989 = 1255
# check profit = 14.5*989-2*1255 = 11830.5



########################################################################################
########### logistic regression ###########
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5 - NO!
# 1367.0 11635.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 643   8
#              1 376 991
# check n.mail.valid = 376+991 = 1367
# check profit = 14.5*981-2*1367 = 11635.5

#######################################################################################
########### logistic regression 2 ###########
# Use: reg1,reg2,reg3,reg4,home,chld,hinc,I(hinc^2),wrat,I(wrat^2),avhv,tgif,tdon,tlag
model.log2 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + wrat + I(wrat^2) + 
                    avhv + tgif + tdon + tlag,data.train.std.c, family=binomial("logit"))

post.valid.log2 <- predict(model.log2, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log2 <- cumsum(14.5*c.valid[order(post.valid.log2, decreasing=T)]-2)
plot(profit.log2) # see how profits change as more mailings are made
n.mail.valid2 <- which.max(profit.log2) # number of mailings that maximizes profits
c(n.mail.valid2, max(profit.log2)) # report number of mailings and maximum profit
# 1367.0 11635.5

cutoff.log2 <- sort(post.valid.log2, decreasing=T)[n.mail.valid2+1] # set cutoff based on n.mail.valid
chat.valid.log2 <- ifelse(post.valid.log2>cutoff.log2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log2, c.valid) # classification table - NO!
#               c.valid
#chat.valid.log2   0   1
#              0 642   9
#              1 377 990
# check n.mail.valid2 = 377+990 = 1367
# check profit = 14.5*990-2*1367 = 11635.5


#######################################################################################
########### logistic regression 3 ###########
model.log3 <- glm(donr ~.,data.train.std.c, family=binomial("logit"))

post.valid.log3 <- predict(model.log3, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log3 <- cumsum(14.5*c.valid[order(post.valid.log3, decreasing=T)]-2)
plot(profit.log3) # see how profits change as more mailings are made
n.mail.valid3 <- which.max(profit.log3) # number of mailings that maximizes profits
c(n.mail.valid3, max(profit.log3)) # report number of mailings and maximum profit
# 1262.0 11874.5

cutoff.log3 <- sort(post.valid.log3, decreasing=T)[n.mail.valid3+1] # set cutoff based on n.mail.valid
chat.valid.log3 <- ifelse(post.valid.log3>cutoff.log3, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log3, c.valid) # classification table
#               c.valid
#chat.valid.log3   0   1
#              0 750   6
#              1 269 993
# check n.mail.valid3 = 269+993 = 1262
# check profit = 14.5*993-2*1262 = 11874.5


#######################################################################################
########### Random Forest 1 ###########
# Make donr as.factor:
data.train.std.c$donr<-as.factor(data.train.std.c$donr)
set.seed(1)
model.rf1<-train(donr~.,data.train.std.c,
                 method="rf",
                 trControl=trainControl(method="cv",number=5),
                 prox=TRUE,
                 metric="Accuracy", # or "Kappa"
                 allowParallel=TRUE)
print(model.rf1)
print(model.rf1$finalModel)
plot(model.rf1)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

post.valid.rf1 <- predict(model.rf1, data.valid.std.c, type="prob") # n.valid post probs
# Do some stuff to get the prediction to work...
post.valid.rf1$RFprob<-post.valid.rf1[,"1"]
post.valid.rf1$class<-predict(model.rf1,data.valid.std.c)
post.valid.rf1<-post.valid.rf1$RFprob

profit.rf1 <- cumsum(14.5*c.valid[order(post.valid.rf1, decreasing=T)]-2)
plot(profit.rf1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf1)) # report number of mailings and maximum profit
# 1447 11388.5

cutoff.rf1 <- sort(post.valid.rf1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.rf1 <- ifelse(post.valid.rf1>cutoff.rf1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.rf1, c.valid) # classification table
#               c.valid
#chat.valid.log3   0   1
#              0 557  14
#              1 462 985
# check n.mail.valid3 = 462+985 = 1447
# check profit = 14.5*985-2*1447 = 11388.5


#######################################################################################
########### KNN Model 1 ###########
model.knn1 <- train(donr~.,data.train.std.c,
                    method = "knn",
                    #preProcess = c("center", "scale"),
                    tuneLength = 10,
                    trControl=trainControl(method="cv",number=5))

print(model.knn1)
plot(model.knn1)
print(model.knn1$finalModel)


#######################################
## Boosted tree
#library(ada)
model.ada1 <- train(donr~.,
                    data.train.std.c,
                    method="ada",
                    tuneGrid = data.frame(iter=100,maxdepth=2,nu=.01)
)



#######################################################################################
# Results

# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1308   11666.5 LDA2
# 1255   11830.5 LDA3
# 1367   11635.5 Log1
# 1367   11635.5 Log2
# 1262   11874.5 Log3 ***
# 1447   11388.5 RF1
# 0000   00000.0 KNN1

########################################################################################
# select model.log1 since it has maximum profit in the validation sample - NO!
# Select Log3...

post.test <- predict(model.log1, data.test.std, type="response") # post probs for test data


########################################################################################
# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.log1)
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
# 1676  331
# based on this model we'll mail to the 331 highest posterior probabilities

# Here's log3 model results:
# chat.test
# 0    1 
# 1693  314 


# See below for saving chat.test into a file for submission


########################################################################################
################################ PREDICTION MODELING ###################################
########################################################################################
# See Mod 4 Lab.r for Subset Selection methods...
# See Mod 5 Lab.r for Non-linear Models...

# Least squares regression

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error: 1.867523

sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error: 0.1696615


# drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error: 1.867433

sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error: 0.1696498


# Results
# MPE  Model
# 1.867523 LS1
# 1.867433 LS2

# select model.ls2 since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions



########################################################################################
#################################### FINAL RESULTS #####################################
########################################################################################
# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="RM.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Canvas for evaluation based on actual test donr and damt values
