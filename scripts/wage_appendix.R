##########################################################################################
##########Advanced Data Analysis Porject:#################################################
############Analysis of the Determinants of Wage from 1985 Current Population Survey######
##########Group Member:###################################################################
############Lin, Xiaotong(xl2506)#########################################################
############Yin, Yiqiao(yy2502)###########################################################
##########05/08/2019######################################################################
##########################################################################################

#rm(list = ls())
library(dplyr)
library(mltools)
setwd("/Users/tong/Desktop/ADA/Project/wage")
wage <- read.csv("/Users/tong/Desktop/ADA/Project/wage/wagedata.csv")

#######scatter plot #######
library("PerformanceAnalytics")
pairs(wage)
chart.Correlation(wage, histogram=TRUE, pch=19)

scat <- select(wage, WAGE.HR, EDUC, EXPERIENCE, AGE)
pairs(scat)
chart.Correlation(scat, histogram=TRUE, pch=19)


#######Model Compare#######

# #1#KNN
# library(DMwR)
# MSE<-NULL
# for (i in 1:10){
#   MSE_test<-NULL
#   all <- 1:534
#   for (j in 1:5){
#     temp=sample(all, 106)
#     all<- all[-temp]
#     test <- as.data.frame(wage[temp,])
#     train <- as.data.frame(wage[-temp,])
#     fit <- kNN(WAGE.HR ~ .,train,test,norm=FALSE,k=2)
#     MSE_test <- c(MSE_test,  mse(as.numeric(fit),as.numeric(test$WAGE.HR))  )
#   }
#   MSE <- c(MSE, mean(MSE_test)) 
# } 
# mean(MSE)  #10171.63

#2#CART
library(rpart)
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    fit <- rpart(WAGE.HR ~., method="poisson", data=train)
    cart <- predict(fit, test, type = c("vector"))
    MSE_test <- c(MSE_test,  mse(as.numeric(cart),as.numeric(test$WAGE.HR))   )
  }
  MSE <- c(MSE, mean(MSE_test)) #20.04448
} 
mean(MSE)  #22.6662

#3#SVM
library(e1071)
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    fit <- svm(WAGE.HR ~., data=train, type = 'nu-regression')
    svm <- predict(fit, test)
    MSE_test <- c(MSE_test, mse(as.numeric(svm),as.numeric(test$WAGE.HR))  )
  }
  MSE <- c(MSE, mean(MSE_test)) 
} 
mean(MSE)  #20.11773

#4#Logistic Regression
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    fit <- glm(WAGE.HR ~., data = train, family = poisson())
    lg <- predict(fit, test[-1])
    MSE_test <- c(MSE_test, mse(as.numeric(lg),as.numeric(test$WAGE.HR))  )
  }
  MSE <- c(MSE, mean(MSE_test)) 
} 
mean(MSE)  #74.06517

#5#Ridge&Lasso
library(glmnet)

MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    fit <- glmnet(as.matrix(train[-1]), as.numeric(train$WAGE.HR), alpha = 1, lambda = 0)
    RL <- predict(fit, s=50, newx = as.matrix(test[-1]))
    MSE_test <- c(MSE_test, mean(((as.numeric(RL) - as.numeric(test$WAGE.HR))^2)  ))  #mse(as.numeric(RL),as.numeric(test$WAGE.HR))
  }
  MSE <- c(MSE, mean(MSE_test)) 
} 
mean(MSE) #20.03559

#6#PC+linear regression
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    
    pc=princomp(train[,-1], scale. = T)
    sd = pc$sdev
    var <- sd^2/sum(sd^2)
    plot(var, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")
    
    pc_train <- data.frame(WAGE.HR = train$WAGE.HR, pc$scores)[,1:11]
    fit <- lm(WAGE.HR ~ .,as.data.frame(pc_train))
    
    PC1 <- rowSums(pc$loadings[,1]*test[,-1])
    PC2 <- rowSums(pc$loadings[,2]*test[,-1])
    PC3 <- rowSums(pc$loadings[,3]*test[,-1])
    PC4 <- rowSums(pc$loadings[,4]*test[,-1])
    PC5 <- rowSums(pc$loadings[,5]*test[,-1])
    PC6 <- rowSums(pc$loadings[,6]*test[,-1])
    PC7 <- rowSums(pc$loadings[,7]*test[,-1])
    PC8 <- rowSums(pc$loadings[,8]*test[,-1])
    PC9 <- rowSums(pc$loadings[,9]*test[,-1])
    PC10 <- rowSums(pc$loadings[,10]*test[,-1])    
    pc_test <- as.data.frame(cbind(PC1, PC2, PC3, PC4,PC5, PC6, PC7, PC8,PC9,PC10))
    
    pred <- fit$coefficients[1]+fit$coefficients[2]*PC1
    +fit$coefficients[3]*PC2
    +fit$coefficients[4]*PC3
    +fit$coefficients[5]*PC4
    +fit$coefficients[6]*PC5
    +fit$coefficients[7]*PC6
    +fit$coefficients[8]*PC7
    +fit$coefficients[9]*PC8
    +fit$coefficients[10]*PC9
    +fit$coefficients[11]*PC10
    
    MSE_test <- c(MSE_test,  mse(pred,as.numeric(test$WAGE.HR))  )
  }
  MSE <- c(MSE, mean(MSE_test)) 
} 
mean(MSE)  #28

MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    
    pc=princomp(train[,-1], scale. = T)
    sd = pc$sdev
    var <- sd^2/sum(sd^2)
    plot(var, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")
    
    pc_train <- data.frame(WAGE.HR = train$WAGE.HR, pc$scores)[,1:11]
    fit <- lm(WAGE.HR ~ .,as.data.frame(pc_train))
    
    pc_test=princomp(test[,-1], scale. = T)
    
    
    
    MSE_test <- c(MSE_test,  sqrt(mean((test$WAGE.HR - predict(fit, data.frame(pc_test$score)))^2))  )
  }
  MSE <- c(MSE, mean(MSE_test)) 
} 
mean(MSE)  #5.381621e+29

#7#Linear Regression (the best)
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage[temp,])
    train <- as.data.frame(wage[-temp,])
    fit <- lm(WAGE.HR ~ .,as.data.frame(train))
    MSE_test <- c(MSE_test,  mse(predict(fit, test),as.numeric(test$WAGE.HR))  )
  }
  MSE <- c(MSE, mean(MSE_test)) #20.04448
} 
mean(MSE)  #19.97


########Correlation Test########
library(Hmisc)
library(corrplot)

wage1 <- select(wage, WAGE.HR, AGE, EDUC,EXPERIENCE, SEX,MARITAL,SOUTH,UNION)
res <- cor(wage1, method = c("pearson"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
p.mat <- rcorr(as.matrix(wage1), type = c("pearson"))
p.mat$P[1,1] <- 0
p.mat$P[2,2] <- 0
p.mat$P[3,3] <- 0
p.mat$P[4,4] <- 0
p.mat$P[5,5] <- 0
p.mat$P[6,6] <- 0
p.mat$P[7,7] <- 0
p.mat$P[8,8] <- 0
corrplot(res, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat$P, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

col<- colorRampPalette(c("blue", "white", "red"))(20)

#pearson
rcorr(as.matrix(wage), type = c("pearson"))
res <- cor(wage, method = c("pearson"))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
heatmap(x = res, col = col, symm = TRUE)


#spearman
rcorr(as.matrix(wage), type = c("spearman"))
res <- cor(wage, method = c("spearman"))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
heatmap(x = res, col = col, symm = TRUE)

#kendall
#rcorr(as.matrix(wage), type = c("kendall"))
res <- cor(wage, method = c("kendall"))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
heatmap(x = res, col = col, symm = TRUE)


#######Two-sample Test#######
wage1 <- wage
# wage1$WAGE.HR[which(wage1$WAGE.HR <5)] <- 0
# wage1$WAGE.HR[which(wage1$WAGE.HR >=20)] <- 4
# wage1$WAGE.HR[which(wage1$WAGE.HR >=15)] <- 3
# wage1$WAGE.HR[which(wage1$WAGE.HR >=10)] <- 2
# wage1$WAGE.HR[which(wage1$WAGE.HR >=5)] <- 1
# 
# wage1$EXPERIENCE[which(wage1$EXPERIENCE <10)] <- 0
# wage1$EXPERIENCE[which(wage1$EXPERIENCE >=50)] <- 5
# wage1$EXPERIENCE[which(wage1$EXPERIENCE >=40)] <- 4
# wage1$EXPERIENCE[which(wage1$EXPERIENCE >=30)] <- 3
# wage1$EXPERIENCE[which(wage1$EXPERIENCE >=20)] <- 2
# wage1$EXPERIENCE[which(wage1$EXPERIENCE >=10)] <- 1
# 
# wage1$AGE[which(wage1$AGE <20)] <- 1
# wage1$AGE[which(wage1$AGE >=60)] <- 6
# wage1$AGE[which(wage1$AGE >=50)] <- 5
# wage1$AGE[which(wage1$AGE >=40)] <- 4
# wage1$AGE[which(wage1$AGE >=30)] <- 3
# wage1$AGE[which(wage1$AGE >=20)] <- 2

agex <- wage1[wage1$WAGE.HR <= 10,]$AGE
agey <- wage1[wage1$WAGE.HR > 10,]$AGE

edux <- wage1[wage1$WAGE.HR <= 10,]$EDUC
eduy <- wage1[wage1$WAGE.HR > 10,]$EDUC

expx <- wage1[wage1$WAGE.HR <= 10,]$EXPERIENCE
expy <- wage1[wage1$WAGE.HR > 10,]$EXPERIENCE

sexx <- wage1[wage1$WAGE.HR <= 10,]$SEX
sexy <- wage1[wage1$WAGE.HR > 10,]$SEX

ethx <- wage1[wage1$WAGE.HR <= 10,]$ETHNIC
ethy <- wage1[wage1$WAGE.HR > 10,]$ETHNIC

marx <- wage1[wage1$WAGE.HR <= 10,]$MARITAL
mary <- wage1[wage1$WAGE.HR > 10,]$MARITAL

ocpx <- wage1[wage1$WAGE.HR <= 10,]$OCCUPATION
ocpy <- wage1[wage1$WAGE.HR > 10,]$OCCUPATION

southx <- wage1[wage1$WAGE.HR <= 10,]$SOUTH
southy <- wage1[wage1$WAGE.HR > 10,]$SOUTH

unix <- wage1[wage1$WAGE.HR <= 10,]$UNION
uniy <- wage1[wage1$WAGE.HR > 10,]$UNION

secx <- wage1[wage1$WAGE.HR <= 10,]$SECTOR
secy <- wage1[wage1$WAGE.HR > 10,]$SECTOR

#2-sample t-test
t.test(agex, agey, alternative = "less")
t.test(edux, eduy, alternative = "less")
t.test(expx, expy, alternative = "less")
t.test(sexx, sexy, alternative = "greater")  
t.test(ethx, ethy, alternative = "less")
t.test(marx, mary, alternative = "less")
t.test(ocpx, ocpy, alternative = "greater")  #rej
t.test(southx, southy, alternative = "greater") 
t.test(unix, uniy, alternative = "less")
t.test(secx, secy, alternative = "less")

#wilcoxon test
wilcox.test(agex, agey, alternative = "less")
wilcox.test(edux, eduy, alternative = "less")
wilcox.test(expx, expy, alternative = "less")
wilcox.test(sexx, sexy, alternative = "greater")
wilcox.test(ethx, ethy, alternative = "less")
wilcox.test(marx, mary, alternative = "less")
wilcox.test(ocpx, ocpy, alternative = "greater") # NO diff
wilcox.test(southx, southy, alternative = "greater")
wilcox.test(unix, uniy, alternative = "less")
wilcox.test(secx, secy, alternative = "less")

#Kruskal-Wallis Test
kruskal.test(WAGE.HR ~ AGE, data = wage) 
kruskal.test(WAGE.HR ~ EDUC, data = wage) 
kruskal.test(WAGE.HR ~ EXPERIENCE, data = wage) 
kruskal.test(WAGE.HR ~ SEX, data = wage) 
kruskal.test(WAGE.HR ~ ETHNIC, data = wage) 
kruskal.test(WAGE.HR ~ MARITAL, data = wage) 
kruskal.test(WAGE.HR ~ OCCUPATION, data = wage) 
kruskal.test(WAGE.HR ~ SOUTH, data = wage) 
kruskal.test(WAGE.HR ~ UNION, data = wage) 
kruskal.test(WAGE.HR ~ SECTOR, data = wage)  #rej


####### Interaction Test#######
summary(aov(wage$WAGE.HR~wage$EDUC*wage$SOUTH*wage$SEX*wage$EXPERIENCE*wage$UNION*wage$AGE*wage$ETHNIC*wage$OCCUPATION*wage$SECTOR*wage$MARITAL))



####### 2x2 Contingency Table #######
#box plot
boxplot(wage$WAGE.HR~wage$SEX)      #male has higher wage      
boxplot(wage$EXPERIENCE~wage$SEX)  #female have more experience

#interaction plot
interaction.plot(wage$SEX,wage$WAGE.HR,wage$EXPERIENCE)

#        wage
#male

##Male  with same exp have higher salary
median(wage$EXPERIENCE) #15
low_exp <- wage[which(wage$EXPERIENCE<10),]
med_exp <- wage[which(wage$EXPERIENCE>=10),]
med_exp <- wage[which(med_exp$EXPERIENCE<20),]
high_exp <- wage[which(wage$EXPERIENCE>=20),]

low_exp_male <- wage[which(low_exp$SEX==0),]
low_exp_wag <- wage[which(low_exp$WAGE.HR>=10),]
low_exp_wag_male <- wage[which(low_exp_wag$SEX==0),]

sex_wage_low_exp <- rbind(
  c(nrow(low_exp_wag_male ) ,  (nrow(low_exp_male)- nrow(low_exp_wag_male)  )   ),
  c((nrow(low_exp_wag) -nrow(low_exp_wag_male)  ),       (nrow(low_exp)-nrow(low_exp_wag_male )- (nrow(low_exp_male)- nrow(low_exp_wag_male)  )  - (nrow(low_exp_wag) -nrow(low_exp_wag_male)  )))
)
chisq.test(sex_wage_low_exp) # => fail rej =>  #male no more wage on low exp
fisher.test(sex_wage_low_exp)


med_exp_male <- wage[which(med_exp$SEX==0),]
med_exp_wag <- wage[which(med_exp$WAGE.HR>=10),]
med_exp_wag_male <- wage[which(med_exp_wag$SEX==0),]

sex_wage_med_exp <- rbind(
  c(nrow(med_exp_wag_male ) ,  (nrow(med_exp_male)- nrow(med_exp_wag_male)  )   ),
  c((nrow(med_exp_wag) -nrow(med_exp_wag_male)  ),       (nrow(med_exp)-nrow(med_exp_wag_male )- (nrow(med_exp_male)- nrow(med_exp_wag_male)  )  - (nrow(med_exp_wag) -nrow(med_exp_wag_male)  )))
)
chisq.test(sex_wage_med_exp) # => fail rej =>  #male have no more wage on med exp
fisher.test(sex_wage_med_exp)


high_exp_male <- wage[which(high_exp$SEX==0),]
high_exp_wag <- wage[which(high_exp$WAGE.HR>=10),]
high_exp_wag_male <- wage[which(high_exp_wag$SEX==0),]

sex_wage_high_exp <- rbind(
  c(nrow(high_exp_wag_male ) ,  (nrow(high_exp_male)- nrow(high_exp_wag_male)  )   ),
  c((nrow(high_exp_wag) -nrow(high_exp_wag_male)  ),       (nrow(high_exp)-nrow(high_exp_wag_male )- (nrow(high_exp_male)- nrow(high_exp_wag_male)  )  - (nrow(high_exp_wag) -nrow(high_exp_wag_male)  )))
)
chisq.test(sex_wage_high_exp) # => rej =>  #male have more wage on high exp
fisher.test(sex_wage_high_exp)

exp <- wage$EXPERIENCE
for (i in 1:534){
  if (exp[i] <10) {
    exp[i]=-1
  } else if (exp[i] >=20){
    exp[i]=1
  } else {
    exp[i]=0
  }
}

wag <- wage$WAGE.HR
for (i in 1:534){
  if (wag[i] <10) {
    wag[i]=0
  } else {
    wag[i]=1
  }
}

wage <- cbind(wage, as.factor(exp),as.factor(wag))
mytable<-xtabs(~as.factor(wag)+as.factor(exp)+SEX,data=wage)
mantelhaen.test(mytable)
#p-value = 1.277e-06 =>  Male  with same exp have higher salary




#######Final Model Compare#######

#excluding occupation & sector
wage2 <- select(wage, WAGE.HR, EDUC,SOUTH,SEX,EXPERIENCE,UNION, AGE, ETHNIC,MARITAL)
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage2[temp,])
    train <- as.data.frame(wage2[-temp,])
    fit <- lm(WAGE.HR ~ .,as.data.frame(train))
    MSE_test <- c(MSE_test,  mse(predict(fit, test),as.numeric(test$WAGE.HR))  )
  }
  MSE <- c(MSE, mean(MSE_test))  
} 
sqrt(mean(MSE))  #4.454213

# Interaction Test (excluding Ethnic, Occupation, and Marital)
wage3 <- select(wage, WAGE.HR, EDUC,SOUTH,SEX,EXPERIENCE,UNION,SECTOR, AGE)
wage3 <- as.data.frame(cbind(wage3, wage$EXPERIENCE*wage$AGE,wage$EXPERIENCE*wage$SEX,wage$EDUC*wage$AGE))
MSE<-NULL
for (i in 1:10){
  MSE_test<-NULL
  all <- 1:534
  for (j in 1:5){
    temp=sample(all, 106)
    all<- all[-temp]
    test <- as.data.frame(wage3[temp,])
    train <- as.data.frame(wage3[-temp,])
    fit <- lm(WAGE.HR ~.,as.data.frame(train))
    MSE_test <- c(MSE_test,  mse(predict(fit, test),as.numeric(test$WAGE.HR))  )
  }
  MSE <- c(MSE, mean(MSE_test))  
} 
sqrt(mean(MSE))  #4.228378
