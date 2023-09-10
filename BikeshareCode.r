rm(list=ls())
library(readxl)
library(gridExtra)
library(dplyr)
library(corrplot)
library(PerformanceAnalytics)
d = read_excel("Bikeshare.xlsx")
str(d)
summary(d)

colSums(is.na(d))                         # Check for missing values
d$weather = as.factor(d$weather)          #To answer 5.a
d$workday = as.factor(d$workday)          #To answer 5.b

#To answer 5.b,5.c and 5.d we need to split the date variable into month and   day.
d$date <- as.Date(d$date)
d$month = as.factor(format(d$date, "%m"))
levels(d$month) <- list(jan="01", 
                        feb="02", 
                        march="03",
                        april="04",
                        may="05",
                        jun="06",
                        july="07",
                        aug="08",
                        sept="09",
                        oct="10",
                        nov="11",
                        dec="12")
d$workday = as.factor(d$workday)
d$day = as.factor(format(d$date, "%a"))
d$day <- relevel(d$day, "Sun")
d$hour <- as.factor(d$hour)
d$year <- factor(format(d$date, "%Y"))
d$year <- relevel(d$year, "2011")

str(d)
#We have 3 dependent variables, casual, registered and count. Lets look at their distributions.

#Looking at casual
p1 = histogram(~casual, data=d)
p2 = histogram(~log(casual), data=d)
summary(d$casual)

#Looking at registered
p3 = histogram(~registered, data =d)
p4 = histogram(~log(registered), data=d)
summary(d$registered)

#Looking at count
p5 = histogram(~count, data =d)
p6 = histogram(~log(count), data=d)
summary(d$count)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

#Checking correlations among the numeric variables
numeric_cols <- d %>%
  select_if(is.numeric)
corr_matrix <- cor(numeric_cols)
corrplot(corr_matrix, type="upper", method="number", order="hclust")
chart.Correlation(numeric_cols, histogram=TRUE, pch=19)

#High correlation between fltemp and temp, among the DVs.

#Variable Selection
#'Weather	+/-	Rentals may be high on a clear day compared to a misty or a rainy day.
#'Fltemp	+	People may prefer to rent a bike on warmer days than colder or rainy days.
#'Humidity	+/-	High humidity can decrease bike rental counts due to discomfort and difficulty in riding in hot conditions. However, in cooler temperatures, it may increase demand as it can make the weather feel more pleasant and reduce the risk of dehydration.
#'Windspeed	+/-	People would not prefer to rent bikes on a windy day compared to non-windy days.
#'Hour	+/-	Rentals can be high during the early hours compared to late hours.
#'Year	+/-	The year can impact bike rentals in either direction due to changes in weather, economic conditions, trends in transportation and leisure activities, and location-specific factors.
#'Month	+/-	Some months like Spring, Summer and Fall months may see a rise and a decline in Winter or Rainy months.
#'Day	+/-	People might prefer to rent on weekends compared to weekdays.
#'Workday	+/-	Same reason as Day.
#'#'The distributions of casual, registered and count 
#' are right-skewed, and hence, OLS regression will not be suitable. 
#' The distributions of log-transformed of these variables are close to normal, 
#' and therefore, more suited for OLS regression.

casual_ols = lm(log(casual + 1) ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year , data = d)
summary(casual_ols)
#987 zeroes in casual.
plot(casual_ols)

#m2

registered_ols = lm(log(registered + 1) ~  workday + weather + fltemp + humidity + windspeed + hour + day + month + year, data = d)
summary(registered_ols)
#15 zeroes in registered
plot(registered_ols)

count_ols = lm(log(count) ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year, data = d)
summary(count_ols)

library(stargazer)
stargazer(casual_ols, registered_ols, count_ols, type="text", single.row=TRUE)

#We are doing log + 1 which is not a good practice to avoid zeroes. 

#' Regression Assumptions 

plot(count_ols)                                  # Linearity: passed, has omitted variables

# Normality
qqnorm(count_ols$residuals,pch=19,
       main="Normality Plot, Counts")
qqline(count_ols$residuals,lwd=3,col="red")                 # Normality: Fails and lower and upper tails. But can be passe 

library("lmtest")
bptest(count_ols)                                # Breusch-Pagan test: failed homoskedasticity

vif(count_ols)                                   # VIF test: failed multi-collinearity but we could give it a pass. 
                                                # The workday and day are highly correlated which is expected but we need them in the model to answer the questions.

library(lmtest)                           # Durbin-Watson test: Failed independence
dwtest(count_ols)  

plot(registered_ols)                                  # Linearity: passed, has omitted variables

# Normality
qqnorm(registered_ols$residuals,pch=19,
       main="Normality Plot, Registered")
qqline(registered_ols$residuals,lwd=3,col="red")                 # Normality:Failed 

library("lmtest")
bptest(registered_ols)                                # Breusch-Pagan test: failed homoskedasticity

vif(registered_ols)                                   # VIF test: failed multi-collinearity but we could give it a pass. 
# The workday and day are highly correlated which is expected but we need them in the model to answer the questions.

library(lmtest)                           # Durbin-Watson test: Failed independence
dwtest(registered_ols)  

plot(casual_ols)                                  # Linearity: failed, has omitted variables

# Normality
qqnorm(casual_ols$residuals,pch=19,
       main="Normality Plot, Casual")
qqline(casual_ols$residuals,lwd=3,col="red")                 # Normality:Failed 

library("lmtest")
bptest(casual_ols)                                # Breusch-Pagan test: failed homoskedasticity

vif(casual_ols)                                   # VIF test: failed multi-collinearity but we could give it a pass. 
# The workday and day are highly correlated which is expected but we need them in the model to answer the questions.

library(lmtest)                           # Durbin-Watson test: Failed independence
dwtest(casual_ols)  

#Even though the adj. R-square is decent for the models, the normality assumption is violated by all the 3 models.

#' Perhaps we should try GLM with a different (Poisson) dist. Since Poisson regression is a good choice for count variables like casual,
#' registered and count.
casual_pois =  glm(casual ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year,family = poisson(link = log), data = d)
summary(casual_pois)

registered_pois = glm(registered ~  workday + weather + fltemp + humidity + windspeed + hour + day + month + year,family = poisson(link = log), data = d)
summary(registered_pois) 

count_pois = glm(count ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year,family = poisson(link = log), data = d)
summary(count_pois)

stargazer(casual_pois, registered_pois, count_pois, type="text", single.row=TRUE)
#overdispersion test
library(AER)
dispersiontest(casual_pois) #Overdispersion exists with dispersion parameter = 7.8
dispersiontest(registered_pois) #overdispersion exists with theta = 24
dispersiontest(count_pois) #overdispersion exists with theta = 31
#' Because of overdispersion (devratio=8,24 and 30), Poisson estimates are not reliable.

# No excess zero problem.

#To overcome overdispersion we need either a quassi-poisson model or a negative binomial model.

casual_quasi = glm(casual ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year, family = quasipoisson(link = log), data = d)
summary(casual_quasi)
registered_quasi = glm(registered ~  workday + weather + fltemp + humidity + windspeed + hour + day + month + year, family = quasipoisson(link = log), data = d)
summary(registered_quasi)
count_quasi = glm(count ~  workday + weather + fltemp + humidity + windspeed + hour + day + month + year, family = quasipoisson(link = log), data = d)

stargazer(casual_quasi, registered_quasi, count_quasi, type="text", single.row=TRUE)

#Overdispersion taken care of.

#Test for multi-collinearity and Independence
vif(casual_quasi) #Fail but expected bue to day and Workday
dwtest(count_quasi) # Durbin-Watson test: Failed independence

vif(registered_quasi)
dwtest(registered_quasi) # Durbin-Watson test: Failed independence

vif(count_quasi) #fail
dwtest(count_quasi) #fail

#The independence assumption fails for all the 3 quasi-models. Let's look at nb models.

library(MASS)
casual_nb = glm.nb(casual ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year, data = d)
registered_nb = glm.nb(registered ~ workday + weather + fltemp + humidity + windspeed + hour + day + month + year, data = d)
count_nb = glm.nb(count ~ workday+ weather+ fltemp + humidity + windspeed + hour + day + month + year, data = d)
summary(count_nb)
stargazer(casual_nb, registered_nb, count_nb, type="text", single.row=TRUE)

vif(casual_nb) #Fail but letting it pass.
dwtest(casual_nb) #Fail
vif(registered_nb)#Fail but letting it pass.
dwtest(registered_nb) #Fail
vif(count_nb) #Fail but letting it pass
dwtest(count_nb) #Fail

#We probably need to run Time-series models like ARIMA.

#Independnce still not passed in these models. 

