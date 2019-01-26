# Problem 2 - Leave
#Data file

data = read.csv(file = "Project-predictive-survey- filter-usa1.csv")
str(data)
data = data.frame(data$leave, data$mental_vs_physical, data$supervisor, data$Age, data$Male, data$Female)
head(data)

logit <- glm(data$data.leave ~ data$data.mental_vs_physical+data$data.supervisor, data = data, family = binomial (link = "logit"))
summary(logit)

#Prediction
pred1 <- predict(logit, data, type= 'response')
head(pred1)
head(data)

#Misclassification Error - data
p1 <- ifelse(pred1> 0.5,1,0)

#Confusion matrix
tab1 <- table(Predicted = p1, Actual = data$data.leave)
tab1
#below table is the tab1 result copied here

#Actual
#Predicted   0   1
#0 363  124
#1  40 181
# Error values are 40+124 = 164


#error percent is  27% only. 
Errorprecent <- (1- sum(diag(tab1))/sum(tab1))*100
paste("error percent", Errorprecent)

#Goodness of fit

library(InformationValue)

library(plotROC)
plotROC(data$data.leave, pred1)

optCutOff <- optimalCutoff(data$data.leave, pred1)[1] 
sensitivity <- sensitivity(data$data.leave, pred1, threshold = optCutOff)
specificity <-specificity(data$data.leave, pred1, threshold = optCutOff)
paste("sensitivity",sensitivity)
paste("specificity",specificity)

Concordance(data$data.leave, pred1)

glm_coef(logit)

pvalue <- with(logit, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
paste("pvalue is", pvalue)
#p-value is less than alpha value and Confidence level is high, means good fit. 


# Psuedo R2 - Logit
logit2 <- update(logit, formula = data$data.leave~1)
R2 <- 1-as.vector(logLik(logit)/logLik(logit2))
paste('Pseudo R2',R2)

# Probit 
probit <- glm(data$data.leave ~ data$data.mental_vs_physical+data$data.supervisor, data = data, family = binomial (link = "probit"))
summary(probit)

#Prediction
pred2 <- predict(probit, data, type= 'response')
head(pred2)
head(data)

#Goodness of fit

with(logit, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))

#p-value is less than alpha value and Confidence level is high, means good fit. 

# Psuedo R2
probit0 <- update(probit, formula = data$data.leave~1)
R2pro <- 1-as.vector(logLik(probit)/logLik(probit0))
R2pro

