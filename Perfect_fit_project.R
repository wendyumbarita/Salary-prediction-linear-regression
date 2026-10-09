library(GGally)
library(ggplot2)
library(freqparcoord)
library(Hmisc)
library(Rmisc)
library(car)
library(formattable)


data(prgeng)
data <- prgeng
data$index <- seq_len(nrow(data))
data$cit <- factor(data$cit, 
                   levels=c(1, 2, 3, 4,5), 
                   labels=c("US", "Territory", "Aboard", "Naturalized", "Non.cit"))
data$occ <- factor(data$occ, levels=c(100, 101, 102, 106,140, 141), 
                   labels=c("Sys.analyst", "Comp.programmer", "Soft.engineer", "Data.admin", "Hard.engineer", "Elect.engineer"))
data$sex <- factor(data$sex, levels=c(1, 2), 
                   labels=c("male", "female"))
data$engl <- factor(data$engl, levels=c(0, 10, 11, 20, 21, 30, 31, 40, 41), 
                    labels=c("Native", "Very-well", "Very-well", "Well", "Well", "Not-well", "Not-well", "Not-all", "Not-all"))
data$birth <- factor(data$birth)
data$powspuma <- factor(data$powspuma)

#--------------------------Look at the wage income----------------------------
hist(data$wageinc)
hist(data$wageinc, 
     col = ("chartreuse4"),         
     border = "white",        
     main = "Histogram of Wage Income",
     xlab = "Wage Income",
     ylab = "Frequency") #nicer histogram <3

#---------------------Look at each predictor closely----------------------------
#--------Histogram for numerical data
hist(data$age, 
     col = c("#4DB6AC"),         
     border = "white",        
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Frequency")
hist(data$educ, 
     col = c("#4DB6AC"),         
     border = "white",        
     main = "Histogram of Education",
     xlab = "Education",
     ylab = "Frequency")
hist(data$wkswrkd, 
     col = c("#4DB6AC"),         
     border = "white",        
     main = "Histogram of Weeks Worked",
     xlab = "Weeks Worked",
     ylab = "Frequency")
hist(data$yrentry, 
     col = c("#4DB6AC"),         
     border = "white",        
     main = "Histogram of Year of Entry",
     xlab = "Year of Entry",
     ylab = "Frequency")

#-------frequency tables for categorical data

engl.table <- as.data.frame(table(data$engl))
colnames(engl.table) <- c("English", "Frequency")
formattable(engl.table, list(
  Frequency = color_tile("white", "#4DB6AC")))

cit.table <- as.data.frame(table(data$cit))
colnames(cit.table) <- c("Citizenship", "Frequency")
formattable(cit.table, list(
  Frequency = color_tile("white", "#4DB6AC")))

occ.table <- as.data.frame(table(data$occ))
colnames(occ.table) <- c("Occupation", "Frequency")
formattable(occ.table, list(
  Frequency = color_tile("white", "#4DB6AC")))

pow.table <- as.data.frame(table(data$powspuma))
colnames(pow.table) <- c("Location", "Frequency")
formattable(pow.table, list(
  Frequency = color_tile("white", "#4DB6AC")))

sex.table <- as.data.frame(table(data$sex))
colnames(sex.table) <- c("Sex", "Frequency")
formattable(sex.table, list(
  Frequency = color_tile("white", "#4DB6AC")))

birth.table <- as.data.frame(table(data$birth))
colnames(birth.table) <- c("Birth", "Frequency")
formattable(birth.table, list(
  Frequency = color_tile("white", "#4DB6AC")))

#Observations on the data (individually):

#For wageinc we see there are some outliers with $325000. Let's remove this 
#from the data. Also, let's remove wageinc = 0 considering some people may have
#done voluntary jobs.

#powspuma and birth have a significant amount of categories that may lead to a 
#difficult interpretation of the model. Do not use them to fit a mdoel

#--------------Remove wageinc = 0 and wageinc = 325000
data <- subset(data, wageinc != 325000) #remove those with income 325000
data <- subset(data, wageinc != 0) #remove those with income 0


#----------------------Fit a full model first------------
m <- lm(wageinc ~ age + sex + educ + wkswrkd 
          + engl + cit + occ + yrentry, data = data )
summary(m)

#Notice we have to do the diagnostic and make sure it passes all the items
#before we make any conclusions 
#--------------------------------Diagnostic------------------------------------
#-------------------------Check multicollinearity------------------------------
vif(m)
#Notice there is a big multicollinearity here (Yun should have more info about 
#this, I think so). Here we decided not to consider yrentry since first, it
#presents a lot of multicollinearity with cit and second, because the data is very
#disproportional, people who was born in the US has yrentry 0 compared to those
#who entered the US having a yrentry of 19**

#-------------------Fit a new model without yrentry---------
m2 <-lm(wageinc ~ age + sex + educ + wkswrkd 
        + engl + cit + occ, data = data )
vif(m2) #now we are good in terms of multicollinearity


residualPlots(m2, ~1, type="rstudent", id=list(labels=row.names(data)))

#This plot suggests there is a nonlinear relationship 

#plot each predictor against the residual plots to consider adding quadratic
#terms if needed
plot(data$age, residuals(m2))
plot(data$educ, residuals(m2))
plot(data$wkswrkd, residuals(m2))

#After plotting each predictor against the residuals, we can add a quadratic
#term to the model. More precisely age^2 and fit a new model with age2
data$age2 <- data$age ^ 2

m3 <- lm(wageinc ~ age + age2 + sex + educ + wkswrkd 
         + engl + cit + occ, data = data )
summary(m3)
vif(m3)
#Multicollinearity is caused by the quadratic term age2. So it is fine

#-----------------------Cooks'D----------------------------
car::influencePlot(m3, id=list(labels=data$instant)) 
#We want to check whether the observation with the highest Cook's D have excessive 
#influence in the model. For this, we check if it exceeds the 50th percentile 


p <- length(coef(m3))
n <- nrow(data)
100 * pf(q = 0.0133839134, df1 = p, df2 = (n - p))
#No overly influential -> 2.446446e-13. then we do not need to remove this observation

#-----------------------dfBetas----------------------------
#We want to see which observations have the highest influence on each 
#predictor coefficient.

my.dfbetas <- dfbetas(m3)

cutoff <- 2/sqrt(n)
data.frame(data[which(abs(my.dfbetas[, "wkswrkd"]) > cutoff), ], 
           my.dfbetas[which(abs(my.dfbetas[,"wkswrkd"]) > cutoff), ])
#some obvious erroneous data -> index = 25, 46, 207, 508, 603, 606, 610... 


wrong.wks <- data.frame(data[which(abs(my.dfbetas[, "wkswrkd"]) > cutoff), ], 
           my.dfbetas[which(abs(my.dfbetas[,"wkswrkd"]) > cutoff), ])
subset(wrong.wks, wageinc <= 10000 & wkswrkd > 20)
#Notice there are several observations that have high influence in the slope
#coefficient for weeks worked that seems to have erroneous data.


#--------------Most influential observations on some predictors----------------
max.wkswrkd <- which.max(abs(my.dfbetas[, "wkswrkd"])) #index 7152 #This observation seems erroneous
max.age <- which.max(abs(my.dfbetas[, "age"]))
max.sexfemale <- which.max(abs(my.dfbetas[, "sexfemale"]))
max.educ.1 <- which.max(abs(my.dfbetas[, "educ"]))  #index 16873 #This observation seems erroneous
max.age2 <- which.max(abs(my.dfbetas[, "age2"]))
max.engl <- which.max(abs(my.dfbetas[, "occ"]))



subset(data[max.wkswrkd,]);subset(data[max.age,]); subset(data[max.sexfemale,]);subset(data[max.educ.1,])

#Looking at the influential observations for wkswrkd, we could see several erroneous
#data where people might have entered low income with high number of weeks worked
#For this, let's create a new cleaned data where we remove observations 
# (income <= 10,000 AND wkswrkd > 20). This may reduce the erroneous data. 
#Also, we want to delete the highest influential observation for wkswrkd and educ, since
#they seem erroneous as well.

delete <- c(7152, 16873)
data.c <- subset(data, !(index %in% delete))
data.c <- subset(data.c, !(wageinc <= 10000 & wkswrkd > 20))

#--------------Let's fit the model with this new data------------------
mc <- lm(wageinc ~ age + age2 + sex + educ + wkswrkd 
         + engl + cit + occ, data = data.c )
summary(mc)
vif(mc) #multicollinearity only in age2 and age

#------------Check residuals
car::residualPlots(mc, ~1, type="rstudent", pch=16) 

#It seems like there is Heteroskedasticity. Let's transform to log

#----------------------Log transformation model---------------------------
data.c$log.inc <- log(data.c$wageinc)
mlog <- lm(log.inc ~ age + age2 + sex + educ + wkswrkd 
           + engl + cit + occ, data = data.c)
car::residualPlots(mlog, ~1, type="rstudent", pch=16, id=list(labels=row.names(data.c))) 

#check for those data points
subset(data.c, index == 19829)
subset(data.c, index == 14937)

#Run the influence plot to check if those points have a high influence
influencePlot(mlog, id=list(labels=data.c$instant)) 
n2 <- nrow(data.c)
#Highest Cook'sD is 0.054546750, let's check whether it exceeds the 50th percentile
100 * pf(q = 0.054546750, df1 = p, df2 = (n2 - p)) 

#It does not exceed the 50th percentile, then none of those points are
#overly influential for the model, so we can leave them as it is

#------------Check normal distribution of the residuals
hist(residuals(mlog))

hist(residuals(mlog), 
     col = c("#4DB6AC"),         
     border = "white",        
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

car::qqPlot(mlog)

#We can see the residuals follow a normal distribution. 
#Now we can proceed to compare models and find the best one.

summary(mlog)

#See if we can drop occ and cit
mR <- lm(log.inc ~ age + age2 + sex + educ + wkswrkd 
            + engl , data = data.c)
summary(mR)
anova(mlog,mR)
#anova table shows the full model fits the data better than the reduced model


#See if we can drop only occ
mR2 <- lm(log.inc ~ age + age2 + sex + educ + wkswrkd 
         + engl + cit , data = data.c)
anova(mlog,mR2)
summary(mR2)
#Same as previous model. The full model fits the data better than the reduced model

#Important: 
#Remember we want to build a model that has a balance between interpretability
#and prediction power. Even if the anova table tell us the full model fits the data
#better, we want to avoid overfitting. So we might consider a reduced model to avoid
#this and make interpretations easier.

#------------------Final model--------------------
mFinal <- lm(log.inc ~ age + age2 + sex + educ + wkswrkd 
         + engl , data = data.c)
summary(mFinal)

#--------------Make predictions--------------------
new.data <- data.frame(age = 35, age2 = 35^2,
                       educ = 12,
                       sex = "female",
                       wkswrkd = 52,
                       engl = "Very-well")

exp(predict(mFinal, newdata=new.data, interval="predict", level=.95))

#just for "fun" let's see the prediction for male, with the same characteristics
new.data2 <- data.frame(age = 35, age2 = 35^2,
                       educ = 12,
                       sex = "male",
                       wkswrkd = 52,
                       engl = "Very-well")

exp(predict(mFinal, newdata=new.data2, interval="predict", level=.95))
#prediction for male is higher than for women, sad

#-------------test for age-------------------
new.data3 <- data.frame(age = 35, age2 = 35^2,
                       educ = 12,
                       sex = "female",
                       wkswrkd = 52,
                       engl = "Very-well")

exp(predict(mFinal, newdata=new.data3, interval="predict", level=.95))


new.data4 <- data.frame(age = 50, age2 = 50^2,
                        educ = 12,
                        sex = "female",
                        wkswrkd = 52,
                        engl = "Very-well")

exp(predict(mFinal, newdata=new.data4, interval="predict", level=.95))

new.data5 <- data.frame(age = 65, age2 = 65^2,
                        educ = 12,
                        sex = "female",
                        wkswrkd = 52,
                        engl = "Very-well")

exp(predict(mFinal, newdata=new.data5, interval="predict", level=.95))

#Notice at the beginning as age increases, the predicted wage increases as well,
#after certain age, income starts decreasing, which makes sense.
