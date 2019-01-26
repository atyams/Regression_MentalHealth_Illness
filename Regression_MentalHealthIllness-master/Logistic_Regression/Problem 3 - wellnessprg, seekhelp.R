
#Data file
data = read.csv(file ="Project-predictive-survey- filter-usa1.csv")
summary(data)

data = data.frame(data$wellness_program, data$seek_help, data$care_options, data$benefits)
head(data)

#plot(data$data.work_interfere, data$data.family_history, xlab = "family history", ylab = "Work Interference")


# Logit model
logit <- glm(data$data.wellness_program ~ data$data.seek_help+data$data.care_options+data$data.benefits, data = data, family = binomial (link = "logit"))
summary(logit)

# Since care.options is insignifcant, remove x3.

logit1 <- glm(data$data.wellness_program ~ data$data.seek_help+data$data.benefits, data = data, family = binomial (link = "logit"))
summary(logit1)

#Prediction
pred1 <- predict(logit1, data, type= 'response')
head(pred1)
head(data)


#Misclassification Error - data
p1 <- ifelse(pred1> 0.5,1,0)

#Confusion matrix
tab1 <- table(Predicted = p1, Actual = data$data.wellness_program)
paste(tab1)
tab1
#below table is the tab1 result copied here


#error percent is %only. 
Error <- (1- sum(diag(tab1))/sum(tab1))*100
paste(Error, "is Error%")

#Goodness of fit

pvalue <- with(logit1, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
paste("pvalue logit",pvalue)

#p-value is 2.00xe-46,  it is less than alpha value and Confidence level is high, means good fit. 

#error percent is  14%. 
errpct = 1- sum(diag(tab1))/sum(tab1)
paste('error percent',errpct)
paste('error percent',errpct)

#install.packages('InformationValue')
library(InformationValue)

library(plotROC)
plotROC(data$data.wellness_program, pred1)

optCutOff <- optimalCutoff(data$data.wellness_program, pred1)[1] 
sensitivity <- sensitivity(data$data.wellness_program, pred1, threshold = optCutOff)
specificity <-specificity(data$data.wellness_program, pred1, threshold = optCutOff)
paste("sensitivity",sensitivity)
paste("specificity",specificity)

Concordance(data$data.wellness_program, pred1)

glm_coef(logit1)

# Psuedo R2
logit1_0 <- update(logit1, formula = data$data.wellness_program~1)
R2 <- 1-as.vector(logLik(logit1)/logLik(logit1_0))
paste('Pseudo R2',R2)

#sensitivity

#a/a+c

a <- 437
b <- 38
c <- 47
d <- 89

sensitivity <- a/a+c
paste(sensitivity)

#Specificity

specificity <- d/b+d
paste(specificity)

