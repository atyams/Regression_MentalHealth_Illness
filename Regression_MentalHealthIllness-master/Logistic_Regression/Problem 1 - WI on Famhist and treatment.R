# Problem 1 - Work inference impact due to family history and treatment
#Data file
data = read.csv(file = "Project-predictive-survey- filter-usa1.csv")
summary(data)

data = data.frame(data$work_interfere, data$family_history, data$treatment)
head(data)


# Logit model
logit1 <- glm(data$data.work_interfere ~ data$data.family_history+data$data.treatment+data$data.family_history*data$data.treatment, data = data, family = binomial (link = "logit"))
summary(logit1)

# Since X3 is insignifcant, remove x3.

logit <- glm(data$data.work_interfere ~ data$data.family_history+data$data.treatment, data = data, family = binomial)
summary(logit)

#install.packages('lattice')

#Prediction
pred1 <- predict(logit, data, type= 'response')
head(pred1)
head(data)

# probability calculation validating... 
#y<- -1.2456+(0.9987*0)+(3.9010*1)
#y

# probability of employee having WI is 
#exp(y)/(1+exp(y))

#Misclassification Error - data
p1 <- ifelse(pred1> 0.5,1,0)

#Confusion matrix
tab1 <- table(Predicted = p1, Actual = data$data.work_interfere)
tab1
# Below table is the tab1 result copied here

#Predicted   #Actual
#             0   1
#   0         199  75
#   1         14 323
# Error values are 75+14 = 89


#error percent is  14%. 
errpct = 1- sum(diag(tab1))/sum(tab1)
paste('error percent',errpct)

#install.packages('InformationValue')
library(InformationValue)

library(plotROC)
plotROC(data$data.work_interfere, pred1)

optCutOff <- optimalCutoff(data$data.work_interfere, pred1)[1] 
sensitivity <- sensitivity(data$data.work_interfere, pred1, threshold = optCutOff)
specificity <-specificity(data$data.work_interfere, pred1, threshold = optCutOff)
paste("sensitivity",sensitivity)
paste("specificity",specificity)

# maximum likelihood

Concordance(data$data.work_interfere, pred1)

glm_coef(logit)
library(pubh) 

# Psuedo R2 - Logit
logit0 <- update(logit, formula = ~1)
R2 <- 1-as.vector(logLik(logit)/logLik(logit0))
paste('Pseudo R2',R2)

probit <- glm(data$data.work_interfere ~ data$data.family_history+data$data.treatment, data = data, family = binomial (link = "probit"))
summary(probit)

#Prediction
pred2 <- predict(probit, data, type= 'response')
head(pred2)
head(data)

# probability calculation validating... 
y<- -1.2456+(0.9987*0)+(3.9010*1)
#y

# probability of employee having WI is 
p <- exp(y)/(1+exp(y))
p 

#Goodness of fit

pvalue <- with(logit, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
paste("pvalue logit",pvalue)
#p-value is less than alpha value and Confidence level is high, means good fit. 

# Psuedo R2

probit0 <- update(probit, formula = data$data.work_interfere~1)
PsuedoR2 <- 1-as.vector(logLik(probit)/logLik(probit0))
paste("Pseudo R2 Probit:",PsuedoR2)

