# Problem statement 4 - Does Age and Gender impact the work interference 

#Data file
data = read.csv(file ="Project-predictive-survey- filter-usa1.csv")
summary(data)

str(data)

data = data.frame(data$work_interfere, data$family_history, data$treatment, data$Age, data$Male, data$Female)
head(data)

# Logit model
logit <- glm(data$data.work_interfere ~ data$data.Age+data$data.Male+data$data.Female, data = data, family = binomial (link = "logit"))
summary(logit)

# Probit model
probit <- glm(data$data.work_interfere ~ data$data.Age+data$data.Male+data$data.Female, data = data, family = binomial (link = "probit"))
summary(probit)

# We believe that there might be ommitted variable bias hence we include other varaibles - Family History and treatment 

#Data file
data = read.csv(file = "Project-predictive-survey- filter-usa1.csv")
summary(data)

str(data)

data = data.frame(data$work_interfere, data$family_history, data$treatment, data$Age, data$Male, data$Female)
head(data)


# Logit model
logit <- glm(data$data.work_interfere ~ data$data.Age+data$data.family_history+data$data.treatment+data$data.Male+data$data.Female, data = data, family = binomial (link = "logit"))
summary(logit)

pred1 <- predict(logit, data, type= 'response')

#Misclassification Error - data
p1 <- ifelse(pred1> 0.5,1,0)

#Confusion matrix
tab1 <- table(Predicted = p1, Actual = data$data.work_interfere)
paste(tab1)
tab1
#below table is the tab1 result copied here


#error percent is %only. 
Error <- (1- sum(diag(tab1))/sum(tab1))*100
paste(Error, "is Error%")
paste('error percent',Error)
#Goodness of fit

pvalue <- with(logit, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
paste("pvalue logit",pvalue)

#p-value is 2.00xe-46,  it is less than alpha value and Confidence level is high, means good fit. 

#error percent is  14%. 
errpct = 1- sum(diag(tab1))/sum(tab1)
paste('error percent',errpct)
paste('error percent',errpct)

#install.packages('InformationValue')
library(InformationValue)

library(plotROC)

plotROC(data$data.work_interfere, pred1)

sp <-ggplot(data = data, aes(y =data$data.work_interfere, x =pred1),
            color=pclass, xlab="Predicted Values")
sp+stat_smooth(mapping = aes(y =data$data.work_interfere, x =pred1),
               method = "glm", method.args = list(family = "binomial"), 
               se = FALSE)+geom_point()


library(quadprog)
library(popbio)

logi.hist.plot(data$data.family_history,data$data.work_interfere,boxp=FALSE,type="hist",col="gray")

glm_coef(logit)

# Psuedo R2
logit0 <- update(logit, formula = data$data.work_interfere~1)
R2 <- 1-as.vector(logLik(logit)/logLik(logit0))
paste('Pseudo R2',R2)



