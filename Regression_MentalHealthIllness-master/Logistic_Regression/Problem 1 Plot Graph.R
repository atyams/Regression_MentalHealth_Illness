#install.packages('pander')
library(Epi)
library(pubh)
library(pander)
library(survival)

data = read.csv(file = "Project-predictive-survey- filter-usa.csv")
summary(data)

data = data.frame(data$work_interfere, data$family_history, data$treatment,data$Age)

levels(data$data.family_history) <- c("No","Yes")
levels(data$data.treatment) <-  c("No","Yes")

model_clogit <- clogit(data.work_interfere ~ data.family_history+data.treatment++ strata(data.Age), data = data)
glm_coef(model_clogit)

pander(glm_coef(model_clogit, labels = c("Family History", "Treatment")), 
       caption = "Parameter estimates from conditional logistic regression.")

grid <- with(data, expand.grid(
  data.family_history = levels(data.family_history),
  data.treatment = levels(data.treatment),
  data.Age = sample(18:63, 1)
))

pred <- predict(model_clogit, grid, type = "lp", se.fit = TRUE)
grid$fit <- inv_logit(pred$fit)
grid$se <- inv_logit(pred$se)
grid$lo <- inv_logit(pred$fit - 1.96 * pred$se)
grid$up <- inv_logit(pred$fit + 1.96 * pred$se)


xyplot(cbind(fit, lo, up) ~ data.family_history|data.treatment, data = grid,  pch = 20, 
       panel = panel.errbars, xlab = "FH and Treatment", ylab = "P (work_interference)")


# Interpretation
# 1 No FH No T - impact on WI is negligible
# 2 If FH , does have a slight impact on WI
# 3 If emp has sought med Trmnt for Mentl illness means he did suffer from MI, 
# which had considerable impact on WI
# 4 Presence FH and T indicate high prob of WI