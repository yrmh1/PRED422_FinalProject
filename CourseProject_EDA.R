## PRED 422 - Course Final Project EDA FIle ##

############ Load "charity.csv" data file ############
charity <- read.csv(file.choose())
#attach(data.train)
# names(data.train)
# str(data.train)

ztrain<-subset(charity,part=="train")
ztest<-subset(charity,part=="test")
zvalid<-subset(charity,part=="valid")
names(ztrain.reg)

deletevars <- names(ztrain) %in% c("ID", "damt", "part") 
ztrain.class <- ztrain[!deletevars]
#table(ztrain$donr)
ztrain.reg <- subset(ztrain, donr==1)
deletevars2 <- names(ztrain) %in% c("ID", "donr", "part") 
ztrain.reg <- ztrain[!deletevars2]

############ Outlier detection ############
#Figure 2 - Outlier Detection
par(mfrow=c(2,1),mar=c(5,4,2,1))
boxplot(z$avhv,horizontal=T,axes=F)
dotchart(z$avhv)
#boxplot(z$avhv,horizontal=T,axes=F)
#ggplot(charity,aes(y=avhv,x=1,size=.002))+geom_boxplot(color="black", size=0.2)+theme_bw()+coord_flip()

?boxplot

#Figure 3 - Cleveland Dot Plots to detect outliers:
library(lattice)
#library(ggplot2)
# Prepare dataframe w/numeric vars sorted by damt:
z <- charity[order(damt),] # Order by damt...
z<-na.omit(z) # omit NAs...
keepvars<-c("avhv","incm","inca","plow","npro","tgif","lgif","rgif","tdon","tlag","agif","damt")
z<-z[keepvars] # Keep only continuous vars...
#fix(z) # Check it out...

#colnames(Z) <- c("wing length","tarsus length","head length","culmen length","nalospi to bill tip","weight")
# Create dot plots:
dotplot(as.matrix(z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Data ordered by damt")


############ X & Y var relationships? ############
# Matrix of scatter plots:
# Arrange the variables so that those w/higher correlations are closer to the principal diagonal.
# Also color code the cells to reflect the size of the correlations...
library(gclus)
dta <- ztrain[c("avhv","incm","inca","plow","npro","tgif","lgif","rgif","tdon","tlag","agif","damt")] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


###################################################################
####### Var Interactions?
#Figure 11
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
#Take the data from species 1, Sex = 0 and Wing length >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
Wing1<- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])

#Define Month and Sex as categorical variables
fMonth1 <- factor(Mon1,levels=c(5,6,7,8,9),
                  labels=c("May","Jun","Jul","Aug","Sep"))
fSex1   <- factor(Sex1, levels=c(4,5),labels=c("Male","Female"))

M1 <- lm(Wei1 ~ Wing1*fMonth1*fSex1)
summary(M1)
anova(M1)

#Make the coplot
coplot(Wei1 ~ Wing1 | fMonth1 * fSex1, ylab = "Weight (g)",
       xlab = "Wing length (mm)",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


############ X Var Correlations? ############
######## rgif & tdon ##########
#par(mfrow=c(1,3),mar=c(5,4,2,1))
# plot(ztrain$tdon,ztrain$rgif)
# plot(ztest$tdon,ztest$rgif)
# plot(zvalid$tdon,zvalid$rgif)

## Form a scatterplot of rgif vs tdon with part as facet to color scatterplot. Repeat with CLASS as facet.
## Calc a Pearson Corr. Coef. between rgif & tdon. Use cor.test() to test if coeff. differs from 0...
g2<-ggplot(ztrain, aes(x=tdon,y=rgif))
g2+geom_point(aes(color=home))+geom_smooth(method="loess",colour="red")

g2<-ggplot(ztrain, aes(x=tdon,y=rgif))
g2+geom_point(aes(color=genf))+geom_smooth(method="loess",colour="red")
# Correlation is NOT significant:
cor(ztrain$tdon, ztrain$rgif)
cor.test(ztrain$tdon, ztrain$rgif)

######## npro & tgif ##########
## As more lifetime number of promotions received to date (npro) increases, 
## dollar amount of lifetime gifts to date (tgif) increases:
g2<-ggplot(ztrain, aes(x=npro,y=tgif))
g2+geom_point(aes(color=home))+geom_smooth(method="loess",colour="red")

g2<-ggplot(ztrain, aes(x=npro,y=tgif))
g2+geom_point(aes(color=genf))+geom_smooth(method="loess",colour="red")
# Correlation is significant:
cor(ztrain$npro, ztrain$tgif)
cor.test(ztrain$npro, ztrain$tgif)

######## npro & agif ##########
## No correlation between lifetime number of promotions received to date (npro) and
## average dollar amount of gifts to date (agif):
g2<-ggplot(ztrain, aes(x=npro,y=agif))
g2+geom_point(aes(color=home))+geom_smooth(method="loess",colour="red")

g2<-ggplot(ztrain, aes(x=npro,y=agif))
g2+geom_point(aes(color=genf))+geom_smooth(method="loess",colour="red")
# Correlation is NOT significant:
cor(ztrain$npro, ztrain$agif)
cor.test(ztrain$npro, ztrain$agif)

########################################################################################
################ Decision Tree ############
########################################################################################
# Vars of importance: child,reg2,home,hinc,wrat,tdon
model.tree1<-rpart(donr~.,data=ztrain.class,xval=10,method="class")
fancyRpartPlot(model.tree1)
# plot(model.tree1)
# text(model.tree1)
# Vars of importance: child,rgif,home,lgif
model.tree1<-rpart(damt~.,data=ztrain.reg,xval=10,method="class")
fancyRpartPlot(model.tree1)

