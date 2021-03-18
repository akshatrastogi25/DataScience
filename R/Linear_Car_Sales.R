#_________linear regression on car dataset ________________________

# Objective - We are looking to find the KPI [Key performance Indicators] of the Sales
# who are the major contributors/ important factors 
# who are driving Sales of the car
# 
sessionInfo()
x= 10
# Clear environment
rm(list = ls())

setwd("E:\\70 - boot camp\\Linear  Logistic In R")
getwd()

mydata<-read.csv("car_sales.csv", header = T)

View(mydata)
dim(mydata)
names(mydata)
str(mydata)
#________________________________________________________________-
 # Step 1 - EDA
 # _____________________________________________________________
 
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

# ------------------------------------------------------------
names(mydata) # this function will give me the names of all the variables

# Step - I just want to hold the numeric variables
# its your headache to get them whatever teh way you like

vars <- c( "Sales_in_thousands" , 
           "X__year_resale_value" , 
           "Price_in_thousands",   
           "Engine_size" , 
           "Horsepower", 
           "Wheelbase" , 
           "Width" ,
           "Power_perf_factor" , 
           "Length" , 
           "Curb_weight" , 
           "Fuel_capacity", 
           "Fuel_efficiency" )

test <- apply(mydata[vars], 2, mystats)

class(test)
diag_stats <- t(data.frame(test))
View(diag_stats)

#diag_stats<-data.frame(apply(mydata[vars], 2, mystats))

write.csv("")

## OUTLIERS

# Technically, you should treat all the outliers where FLAG = 1
#
# Capping** of outliers with the Upper Cap/ Lower cap

mydata$Sales_in_thousands[mydata$Sales_in_thousands>257.086342425636] <-257.086342425636
mydata$X__year_resale_value[mydata$X__year_resale_value>52.4331275042866] <-52.4331275042866
mydata$Price_in_thousands[mydata$Price_in_thousands>70.4457144064253] <-70.4457144064253

## Missing value treatment
## 
mydata<- mydata[!is.na(mydata$Sales_in_thousands),] # dropping obs where DV=missing

install.packages("Hmisc")
require(Hmisc)

#Imputing missings with mean for IV's
mydata1 <- data.frame(apply(mydata[vars],2, function(x) impute(x, mean))) 

mydat2<-cbind(mydata1, Vehicle_type=mydata$Vehicle_type )
str(mydat2)
_______________________________________________________________________
View(mydat2)
#R code for categorical variables(Converting as factor variable)
# Why ? - this is Encoding - alternative for Dummy Variable
# _______________________________________________________________
class(mydat2$Vehicle_type)

mydat2$Vehicle_type <- factor(mydat2$Vehicle_type)

levels(mydat2$Vehicle_type) <- c("Car","Passenger")

#__________________________________________________________________
# Playing with the Assumptions 
# Assumption 1 - my Y should be normal
hist(mydat2$Sales_in_thousands)

# ln OR LOG OR sqrt OR log(log()) 
#INcase interested to learn More:
#Kindly go through this link:
#https://stats.stackexchange.com/questions/33392/improving-transformation-of-dependent-variable-and-robust-regression

hist(log(mydat2$Sales_in_thousands))

mydat2$ln_sales<-log(mydat2$Sales_in_thousands)


install.packages("corrplot")

c <- cor(mydat2[,vars])

require(corrplot)
corrplot(cor(mydat2[,vars],use="pairwise.complete.obs"), method="circle", tl.cex = 0.2)

require(car)
scatterplotMatrix(mydat2)

#Homework: >>  Can also use GGally Pacakge having a function - GGpair
#______________________________________________________________________
View(mydat2)

#Splitting data into [Training] and [Validaton/Testing] Dataset
#
#
set.seed(999)# 

train_ind <- sample(1:nrow(mydat2), size = floor(0.70 * nrow(mydat2)))

mydata[train_ind,]

length(train_ind)

training<-mydat2[train_ind,]
testing <-mydat2[-train_ind,]

install.packages("car")
require(car) # Companion to Applied Regression
scatterplotMatrix(mydat2) # regading my assumption, you could use cor() - 
                          # for correlation matrix

# Multiple Linear Regression Example 
#___________________________________________________________________________

fit <- lm(Sales_in_thousands ~ X__year_resale_value + Price_in_thousands + 
            Engine_size+Horsepower+Wheelbase+Width
            +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, 
          data=training)



options(scipen=999)
summary(fit)


fit2 <- lm(ln_sales ~ X__year_resale_value + 
             Price_in_thousands+ Engine_size+Wheelbase
           +Length+Curb_weight+Fuel_capacity+
             Fuel_efficiency+Vehicle_type, data=training)

summary(fit) # show results
summary(fit2)
library(car)
vif(fit2) # this will only let you know the multicolliniearity
           # this is not a way to treat the Multicollinearity
           # Txt- 2, but practically - we can hold till 4
           # any variable's IF score greater then 4 is the source 
           # of multicollinearity
           # ____________________________________________________________
# Now to remove the nuances from the data
# some variables are there which are creating noise in the data/ analysis
# such variables are called as - 'insignificant Variables'
# -----
# they might be there because of Multicolooniearity as well
#  when you are creating new variables called as - derived variables
#  To deal with the insignificant variables 
#  we have three methods:
#       a)Factor Analysis
#       b) VIF- Variance inflation Factor
#       c) stepAIC Method 
#                    Direction -  Forward
#                                 Backward
#                                 Both

# Modern Applied Statistics with S - MASS 

install.packages("MASS")
require(MASS)
library(MASS)

?stepAIC() - # Performs stepwise model selection by AIC.
             # AIC - Akaike Information Criteria
                     # + Reward - if variable helps in model explanation or improvisation
                     # - Penalty - if variable is not singnificnat for my model    
  
step3<- stepAIC(fit2, direction="both")

ls(step3)
step3$anova

#as per my result of this AIC function, 
#I will take the least score among multile iterations
#Step:  AIC = 6.46
#ln_sales ~ Price_in_thousands + Engine_size + Wheelbase + Fuel_efficiency + 
#  Vehicle_type


fit3<-lm(ln_sales~ 
           Price_in_thousands + Engine_size + 
           Wheelbase +  Fuel_efficiency + 
           Vehicle_type, 
           data = training)

summary(fit3)

#Multicollinierity Check using VIF
library(car)
vif(fit3) # Variance inflation Factor
             # the value of VIF on standard basis we go is  - 1.60
             # otherwise on a high level - we check with 2
             # 
#________________________________________________________________
# MODEL VALIDATION STEPS :

####################### SCORING USING PREDICT FUNCTION

t1<-cbind(training, pred_sales = exp(predict(fit3,training)))
names(t1)

t1<- transform(t1, APE = abs(pred_sales - Sales_in_thousands)/Sales_in_thousands)
mean(t1$APE)
View(t1)

t2<-cbind(testing, pred_sales=exp(predict(fit3,testing)))
t2<- transform(t2, APE = abs(pred_sales - Sales_in_thousands)/Sales_in_thousands)

mean(t2$APE)
View(t2)

################################## Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_sales,c(-Inf,decLocations, Inf))
View(t1)

require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(Sales_in_thousands) as avg_Actual_sales
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA,"mydata1_DA.csv")


##################################Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_sales,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(Sales_in_thousands) as avg_Actual_sales
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"t2_DA.csv")


# Other useful functions 

coefficients(fit3) # model coefficients
confint(fit3, level=0.95) # CIs for model parameters 
fitted(fit3) # predicted values
residuals(fit3) # residuals
anova(fit3) # anova table 
influence(fit3) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

#########SCORING USING MATHEMATICAL EQUATION############

#Creating dummy varibles

mydata1$VT[mydata1$Vehicle_type ==  "Passenger"] <- 1
mydata1$VT[mydata1$Vehicle_type ==  "Car"] <- 0

summary(fit)


############################### SCoring Data sets/Predicting the sales#####################################
mydata1$Ln_pre_sales<- (-2.321593  +
                          mydata1$Price_in_thousands* -0.054988 +
                          mydata1$Engine_size*0.254696  +
                          mydata1$Wheelbase*0.047546	+
                          mydata1$Fuel_efficiency*0.068975+
                          mydata1$VT*-0.573255)
mydata1$Pre_sales= exp(mydata1$Ln_pre_sales);


###################################END OF REGRESSION case study 








