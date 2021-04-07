# Objective: 
# We have to predict whether a New Customer will Default or Not ?
# New_Cust = All those rows[150 records at the end] where Default is missing 
# ___________________________________________________________________

#Set Directory
setwd("E:\\70 - boot camp\\Linear  Logistic In R")

# Read CSV file
mydata<-read.csv("bankloans.csv",header=T)

# View(mydata)
View(mydata)
str(mydata)

# Education is actually a descrete variable, 
# hence we are converting that to Factor Data type
mydata$ed <- factor(mydata$ed)  # this is categorical variable

str(mydata)

#________________________ DATA AUDIT _____________________
# EDA EXERCISE
#Create user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)           # number of rows in each column
    nmiss<-sum(is.na(x))   # N# of mising values
    mean<-mean(x, na.rm=T)
    std<-sd(x, na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T) # 1st percentile
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)     # P50 - 50th percentile/ Q2/ D5/ Median/ 
    q3<-quantile(x,0.75,na.rm=T)  
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T) # outlier upper range
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)  # outlier lower range
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr # Q3 - Q1 [50% of the data]
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)             # Frequency
    prop<-prop.table(table(x)) ## proportion of Freq
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

#Vector of numerical variables
num_var= sapply(mydata,is.numeric)  # suitable apply function - gives vector most of the times
Other_var= !sapply(mydata,is.numeric)

#Applying above defined function on numerical variables
#
my_num_data <- t(data.frame(apply(mydata[num_var], 2, var_Summ)))

my_cat_data<-data.frame(t(apply(mydata[Other_var], 2, var_Summ)))

View(my_num_data)
View(my_cat_data)

# missing values
# 
apply(is.na(mydata[,]),2,sum)

mydata1 <- mydata[!is.na(mydata$default),]

New_cust <- mydata[is.na(mydata$default),]

View(mydata1)
View(New_cust)

#Missing Value Treatment
#
mydata1[,num_var] <- apply(data.frame(mydata1[,num_var]), 
                           2, 
                           function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})

# Replacing the categorical missing values with the MODE 

mydata1[,Other_var] <-apply(data.frame(mydata1[,Other_var]), 
                      2, 
                      function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


# OUTLIERS TREATMENT - WHERE P1 AND P99 ARE THE MAX AND MIN LIMITS
#____________________________________

M1_fun <- function(x){
           quantiles <- quantile(x, c(.01, .99 ),na.rm=TRUE )
           # Above line will calc the P1 and P99
  
  x[x < quantiles[1] ] <- quantiles[1]  # if value < P1, then P1
  x[ x > quantiles[2] ] <- quantiles[2]  # if value > P99, then P99
  x
}

# Applying the func for Outlier Treatment
mydata1[,num_var] <- apply(data.frame(mydata1[,num_var]), 2, M1_fun) 

#Splitting data into Training, Validaton and Testing Dataset
#_________________________________________________________________________

set.seed(999)

train_ind <- sample(1:nrow(mydata1), size = floor(0.70 * nrow(mydata1)))

training <- mydata1[train_ind,]
 testing <- mydata1[-train_ind,]

nrow(training) # Development dataset records
nrow(testing)  # # of records in validation dataset

#Building Models for training dataset

fit<-glm(default ~ age+ed+employ+address+income+debtinc+creddebt+othdebt,
         data = training,
         family = binomial(logit))

#Output of Logistic Regression
summary(fit)
ls(fit)
my_model <- fit$model

train_Check <- cbind(training, Prob = predict(fit, type="response")) 

View(train_Check)


coeff<-fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

#Checking for concordance 

# Concordance actually gives us the fitness of the model
# this parameter is used for identifying the Best Fit Model
# you need to read this concordance like - R2 in linear regression
# any concordance rate typically greater then 60%, 
# you can consider that model to be taken


# Analogy : Just like - %include in SAS macros 

source("C:\\Users\\Ankur\\Documents\\MyDataScience\\Concordance.R")
Concordance(fit)  #NOTE: To run these command, first run concordance function in Concordance.R 
# Minimum Cutoff we consider is .60 as concordance ration

#Stepwise regression
step1=step(fit, direction = "both")

# AIC - Akaike information criterion
# The Akaike information criterion (AIC) is an 
# estimator of the relative quality of statistical models 
# for a given set of data. Given a collection of models for the data, 
# AIC estimates the quality of each model, 
# relative to each of the other models. 
# Thus, AIC provides a means for model selection.



# Get the below equation from the step(fit) result with the  min. AIC value
# default ~ age + employ + address + debtinc + creddebt

#Final Model
#______________________________________________________________________
fit2<-glm(default ~ age + employ + address + income + creddebt + othdebt, 
          data = training,
         family = binomial(logit))

summary(fit2)
source(choose.files())
Concordance(fit2)


################################ VALIDATION and scoring of my model ##############################
#Decile Scoring for 
##Training dataset

train1<- cbind(training, Prob = predict(fit2, type="response")) 
View(train1)



##Creating Deciles
(decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1)))

train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))

View(train1)

require(dplyr)

train1$decile<-factor(train1$decile)

tr <- train1[order(train1$decile),]

require(dplyr)

decile_grp<-group_by(train1,decile)

decile_summ_train<-summarize(decile_grp, 
                             total_cnt = n(), 
                             min_prob = min(p=Prob),
                             max_prob = max(Prob), 
                             default_cnt= sum(default), 
                              non_default_cnt = total_cnt -default_cnt )
View(decile_summ_train)

# SORTING THE DATA IN DESCEINDING  ORDER BASED ON dECILES
decile_summ_train<-arrange(decile_summ_train, desc(decile))

View(decile_summ_train)

write.csv(decile_summ_train,"fit_train_DA1.csv",row.names = F)

getwd()

#Decile Analysis Reports
require(sqldf)
fit_train_DA <- sqldf("select decile, min(Prob) as Min_prob
                       , max(Prob) as max_prob
                       , sum(default) as default_Count
                       , (count(decile)-sum(default)) as Non_default_Count 
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)

##Testing dataset

test1<- cbind(testing, Prob=predict(fit2,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
View(test1)


test1$decile<-factor(test1$decile)

decile_grp<-group_by(test1,decile)

decile_summ_test<-summarize(decile_grp, 
                             total_cnt = n(), 
                             min_prob = min(p=Prob),
                             max_prob = max(Prob), 
                             default_cnt= sum(default), 
                             non_default_cnt = total_cnt -default_cnt )


# SORTING THE DATA IN DESCEINDING  ORDER BASED ON dECILES
decile_summ_test<-arrange(decile_summ_test, desc(decile))

View(decile_summ_test)

write.csv(decile_summ_test,"fit_test_DA1.csv",row.names = F)

# Confusion matrix
table(train1$Prob>0.28, train1$default)


#______________________________________________________
# Application and predicting the values with model for new customers

New_cust1<-cbind(New_cust, Prob=predict(fit2, New_cust, type="response"))

View(New_cust1)

New_cust1$default <- ifelse(New_cust1$Prob>0.28, 1,0)

View(New_cust1)
sum(New_cust1$default)







table(test1$Prob>0.22, test1$default)

#Performance of the model
##Some Formulas
#### TPR =TP/TP+FN
#### TNR = TN/TN+FP
#### FPR = 1-TPR
#### PRECISION = TP/TP+FP
#### ACCURACY = TP+TN/P+N

train1<- cbind(training, Prob=predict(fit, type="response")) 
View(train1)
require(ROCR)
pred_train_fit2 <- prediction(train1$Prob, train1$default)
perf_fit2 <- performance(pred_train_fit2, "tpr", "fpr")
plot(perf_fit2)
abline(0, 1)
performance(pred_train_fit2, "auc")@y.values
