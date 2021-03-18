rm(list = ls())
#loading data set
adv <- read.csv('C:\\Users\\hp\\Desktop\\R_programming\\Advertising.csv')
adv['X'] <- NULL
#EDA
dim(adv)
names(adv)
str(adv)

# user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, 
           stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,
           q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}
#myst <- data.frame(mystats(adv))
test <- data.frame(apply(adv, 2, mystats))

sum(is.na(adv))
#no missing values

#outliers
adv$Newspaper[adv$Newspaper>95.889] <-95.88986

#visualization
par(mar=c(1,1,1,1))
hist(adv$Sales)
#data seems to be normally distibuted

install.packages("corrplot")

require(corrplot)
corrplot(cor(adv,use="pairwise.complete.obs"), method="circle", tl.cex = 0.2)

install.packages('car')

require(car)
scatterplotMatrix(adv)

adv2<-cbind(adv)
#train and splitting
set.seed(999)# 

train_ind <- sample(1:nrow(adv2), size = floor(0.70 * nrow(adv2)))

adv2[train_ind,]

length(train_ind)

training<-adv2[train_ind,]
testing <-adv2[-train_ind,]

# Multiple Linear Regression Example 
#___________________________________________________________________________
library(dplyr)
fit <- lm(Sales ~ TV ,Radio , Newspaper, data = training)

library("Metrics")

predictions <- fit %>% predict(testing)
#rmse
rmse(predictions, testing$Sales)
#mse
mse(predictions, testing$Sales)
sum<-summary(fit)
#MSE
mean(sum$residuals^2)
#RMSE
sqrt(mean(sum$residuals^2))

#without newspaper data
fit2 <- lm(Sales ~ TV ,Radio , data = training)
predictions2 <- fit2 %>% predict(testing)
#rmse
rmse(predictions2, testing$Sales)
#mse
mse(predictions2, testing$Sales)
sum<-summary(fit2)
