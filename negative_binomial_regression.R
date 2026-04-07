#import libraries
library(readr)
library(MASS)
library(zoo)
library(dplyr)
library(lubridate)
library(sandwich)

#import data
data <- read_csv("monthly_moving_violation.csv")

#remove outliers and format month field
data <- data[data$Stop_Count>3133,]
data$Month <- as.yearmon(data$Month)
data$month_numeric <-format(data$Month,"%m")

#negative binomial regression model
monthly_trend <- glm.nb(Stop_Count ~ month_numeric,data=data)
summary(monthly_trend)
#output confirms June, November, and December are significant

#test against null model to confirm significances aren't due to overfitting
null_model <- glm.nb(Stop_Count ~ 1,data=data)
anova(null_model, monthly_trend)
#confirm that the models significantly differ

#graph general trend between Jan 2007 and Jun 2016
library(ggplot2)
ggplot(data, aes(x = Month, y = Stop_Count)) +
  +   geom_point() +
  +   geom_smooth(method = "glm.nb", formula = y ~ x) +
  +   theme_minimal() +
  +   labs(title = "Negative Binomial Trendline")
#trends downward

#check model assumptions
#test that NBR is appropriate with overdispersion
poisson_mod <- glm(Stop_Count ~ month_numeric, family = "poisson", data = data)
nb_mod <- glm.nb(Stop_Count ~ month_numeric, data = data)
pchisq(2 * (logLik(nb_mod) - logLik(poisson_mod)), df = 1, lower.tail = FALSE) 

#check for outliers
plot(cooks.distance(nb_mod), type="h", main="Cook's Distance")
#cook's distance plot confirms no influential outliers greater than 0.5
