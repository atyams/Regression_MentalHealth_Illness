#install.packages('pander')
library(Epi)
library(pubh)
library(pander)
library(survival)

data = read.csv(file = "Project-predictive-survey- filter-usa.csv")
summary(data)

data = data.frame(data$leave, data$mental_vs_physical, data$supervisor, data$Age, data$Male, data$Female)
head(data)

levels(data$data.mental_vs_physical) <- c("No MVsP","MVsP")
levels(data$data.supervisor) <-  c("No Supervisor","Supervisor")

model_clogit <- clogit(data.leave ~ data.mental_vs_physical+data.supervisor++ strata(data.Age), data = data)
glm_coef(model_clogit)

pander(glm_coef(model_clogit, labels = c("MentalVsPhisical", "Supervisor")), 
       caption = "Parameter estimates from conditional logistic regression.")

grid <- with(data, expand.grid(
  data.mental_vs_physical = levels(data.mental_vs_physical),
  data.supervisor = levels(data.supervisor),
  data.Age = sample(18:63, 1)
))

pred <- predict(model_clogit, grid, type = "lp", se.fit = TRUE)
grid$fit <- inv_logit(pred$fit)
grid$se <- inv_logit(pred$se)
grid$lo <- inv_logit(pred$fit - 1.96 * pred$se)
grid$up <- inv_logit(pred$fit + 1.96 * pred$se)


xyplot(cbind(fit, lo, up) ~ data.mental_vs_physical|data.supervisor, data = grid,  pch = 20, 
       panel = panel.errbars, xlab = "MentalVsPhisical and Supervisor", ylab = "P (Leave)")
