#install.packages('pander')
library(Epi)
library(pubh)
library(pander)
library(survival)

data = read.csv(file = "Project-predictive-survey- filter-usa.csv")
summary(data)

data = data.frame(data$wellness_program, data$seek_help, data$benefits, data$Age)
head(data)

levels(data$data.seek_help) <- c("No SeekHelp","SeekHelp")
levels(data$data.benefits) <-  c("No Benefits","Benefits")
#survival
model_clogit <- clogit(data.wellness_program ~ data.seek_help+data.benefits+ strata(data.Age), data = data)
#pubh
glm_coef(model_clogit)

pander(glm_coef(model_clogit, labels = c("Seek Help", "Benefits")), 
       caption = "Parameter estimates from conditional logistic regression.")

grid <- with(data, expand.grid(
  data.seek_help = levels(data.seek_help),
  data.benefits = levels(data.benefits),
  data.Age = sample(18:63, 1)
))

pred <- predict(model_clogit, grid, type = "lp", se.fit = TRUE)
grid$fit <- inv_logit(pred$fit)
grid$se <- inv_logit(pred$se)
grid$lo <- inv_logit(pred$fit - 1.96 * pred$se)
grid$up <- inv_logit(pred$fit + 1.96 * pred$se)
