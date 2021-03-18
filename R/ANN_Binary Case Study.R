# ANN  Case Study with R
# -------------------------------------------------------- 

data <- read.csv(file.choose())

View(data)
str(data)


#  Min Max normalization
#  Normalisation means we are reducing the scale of the values and making them in range
#  between 0 to 1

data$gre <- (data$gre - min(data$gre))/(max(data$gre)-min(data$gre)) 
hist(data$gre)


data$gpa <- (data$gpa - min(data$gpa))/(max(data$gpa)-min(data$gpa)) 
hist(data$gpa)


data$rank <- (data$rank - min(data$rank))/(max(data$rank)-min(data$rank)) 
hist(data$rank)


# Splitting the data
set.seed(222)

ind = sample(2,nrow(data), replace = T, prob= c(0.8, 0.2))
table(ind)

train = data[ind ==1,]
test = data[ind==2,]


# How to create Neural Network
# ___________________________________________
# 
install.packages("neuralnet", dependencies = T)
library(neuralnet)
require(neuralnet)

n <- neuralnet(admit ~ gre + gpa + rank,
               data = train,
               stepmax = 500,
               rep = 2,
               hidden = 3, # Number of hidden layer
               linear.output = F)


n

plot(n)

# Predicting those results
# ___________________________________

output <- compute(n, train[,-1])

head(output$net.result)

head(train[1,])



#---------------------------------------
# Confusion Matrix and 
# Missclassification Error - training

p1 <- output$net.result

pred1 <- ifelse( p1>0.5, 1, 0 )

tab1 <- table(pred1, train$admit)

tab1


1 - sum(diag(tab1)/sum(tab1))


#-----------------------------------------------------
# recreating the model with additional arguments to improve
#  the model performance and error percentage

n <- neuralnet(admit ~ gre + gpa + rank,
              data = train,
              hidden = 5,
              err.fct = "ce", # Cross Entropy, alternative approach for error calculation
              linear.output = F,
              lifesign = "full",
              rep = 5)

plot(n)
# Predict and compare the 
# error matrix and confusion matrix

