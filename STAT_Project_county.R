# Load the dataset
county <- read.csv("county.csv", h = T)

#Essential libraries
library(ggplot2)
library(gridExtra)
library(rpart)
library(Hmisc)
library(usmap)
library(maps)
library(mapproj)
library(tidyverse)
library(dplyr)

# View the first few rows of the dataset
head(county)

# Summary statistics for numeric variables
summary(county)

# Structure of the dataset
str(county)

#As na.omit remove the entire record so we used impute function to replace the Missing values(NA) with the median value


#Now calculate missing values
county <- na.omit(county)

#Scatter plot after using NA.omit
ggplot(county, aes(y=poverty, x=median_hh_income))+geom_boxplot()
  geom_smooth(method=lm, se=FALSE)
#Use log for better result
ggplot(county, aes(y=log(poverty), x=median_hh_income))+geom_point()+
  geom_smooth(method=lm, se=FALSE)
#----------------------------------------------------------------------------------------------------

#Histogram of median hh income 

  hist(county$median_hh_income, probability = T)
#Higher bars with high probability density represents where most of the observations falls in terms of household income

#Histogram of median hh income with log
hist(log(county$median_hh_income), probability = T)
#Higher bars with high probability density represents where most of the observations falls in terms of household income but, with log we are getting the clear values

#Histogram of uemployment rate with log 
hist(log(county$unemployment_rate), probability = T)

#Histogram of poverty with log
hist(log(county$poverty), probability = T)
#----------------------------------------------------------------------------------------------------------

#Poverty rate across the state

#get state data and arrests data
state <- map_data("county") 
arrests <- USArrests

#adjust case for matching 
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))


#merge and sort (plots in order, sort ensures states filled in)
arrests.geo <- merge(state, arrests, sort = FALSE, by = "region")
arrests.geo <- arrests.geo[order(arrests.geo$order), ]


#plot
ggplot(arrests.geo, aes(long, lat))+
  geom_polygon(aes(group = group, fill = assault))+
  coord_map()
#-------------------------------------------------------------------------------------------------------------
  
#Correlation between unemployment rate and median household income
correlation <- cor(county$unemployment_rate, county$median_hh_income)
correlation
#A negative correlation implies that as unemployment rate increases, the median HH income tends to decrease.

ggplot(county, aes(x = log(unemployment_rate), y = log(median_hh_income))) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Unemployment Rate vs. Median Household Income",
       x = "Unemployment Rate", y = "Median Household Income") +
  theme_minimal()
#This implies that unemployment rate increases, then median HH income decreases.
#-------------------------------------------------------------------------------------------------------------
  
# Homeownership rate comparison between metropolitan and non-metropolitan areas
ggplot(county, aes(x = factor(metro), y = homeownership, fill = metro)) +
  geom_boxplot() +
  labs(title = "Comparison of Homeownership Rate in Metropolitan vs. Non-Metropolitan Areas",
       x = "Metropolitan Area", y = "Homeownership Rate", fill = "Metropolitan Area") +
  theme_minimal()

#The median line is lower for metropolitan areas, it suggests that, on average,
#non-metropolitan areas have higher homeownership rates.

#---------------------------------------------------------------------------------------------------------------

head(county)
summary(county)
str(county)
# Example analysis

# Calculate the average population change from 2010 to 2017
avg_pop_change <- mean(county$pop_change, na.rm = TRUE)
avg_pop_change
#From 2010 to 2017 the average change in population is 58.4%

# Convert 'poverty' variable to numeric
county$poverty <- as.numeric(as.character(county$poverty))

# Filter counties with poverty rate greater than 20%
high_poverty_counties <- county %>% filter(poverty > 20)
high_poverty_counties

# Plotting

# Example: Scatter plot of per capita income vs. median household income
plot(county$per_capita_income, county$median_hh_income, 
     xlab = "Per Capita Income", ylab = "Median Household Income",
     main = "Per Capita Income vs. Median Household Income")

# Example: Bar plot of unemployment rate by state
unemployment_by_state <- county %>% group_by(state) %>% summarize(avg_unemployment_rate = mean(unemployment_rate, na.rm = TRUE))
unemployment_by_state
barplot(unemployment_by_state$avg_unemployment_rate, names.arg = unemployment_by_state$state, 
        xlab = "State", ylab = "Average Unemployment Rate",
        main = "Average Unemployment Rate by State" ,las = 2)


#-------------------------------------------------------------------------------------------------------

## Create Training and testing set
N<-dim(county)
N
set.seed(123)
train_index<-sample(1:2560, 0.8*2560)
train_data<-county[train_index,]
test_data<-county[-train_index,]
#----------------------------------------------------------------------------------------------------------

# Interaction between variables
model_lm_1 <- lm( log(poverty)~median_edu , data = county)
ggplot(county, aes(x=median_edu, y=log(poverty)))+geom_boxplot()
#here 71.428%---->((3.6-2.1)/2.1) of population is having more poverty rate as compared to bachelors. Bachelors are having less poverty rate compared to below_hs, hs_diploma and some_college

summary(model_lm_1)
anova(model_lm_1)

model_lm_2 <- lm( log(poverty)~median_edu+median_hh_income , data = county)
summary(model_lm_2)
anova(model_lm_2)

model_lm_3 <- lm( log(poverty)~median_edu + median_hh_income + unemployment_rate , data = county)
summary(model_lm_3)
anova(model_lm_3)

model_lm_4 <- lm( log(poverty)~median_edu + median_hh_income + unemployment_rate + metro , data = county)
summary(model_lm_4)
anova(model_lm_4)

#comment by checking r2 and mse for all in terms of  %

#----------------------------------------------------------------------------------------------------------
plot(county$poverty)

# Assuming your data is stored in a vector named "poverty_data"
# Convert the vector into a dataframe
df <- data.frame(county$poverty)

# Decide on a poverty threshold or range
# For example, let's consider a threshold of 20% poverty rate
poverty_threshold <- 20

# Create a new variable/column to identify "poor" and "rich" individuals
df$Income_Status <- ifelse(df >= poverty_threshold, "Rich", "Poor")


#Converting poverty to binary. 

county$poverty <- as.factor(county$poverty)
data<-data.frame(yes_no_var=sample(c("Rich","Poor"),2560, replace=TRUE))
data$binary_var <- lapply(data$yes_no_var, function(x) ifelse(x == "Rich", 1, 0))
#-------------------------------------------------------------------------------------------------------------

#train an test data set
set.seed(123)
train_index<-sample(1:2560, 0.8*2560)
train_data<-county[train_index,]
test_data<-county[-train_index,]
train1<-na.omit(train_data) 
dim(county); dim(train1)

fit<-lm(poverty~median_hh_income, data=train1)
fit
# As the median_hh_income increases the poverty rate decrease by 0.00362 units

ggplot(train1, aes(x=median_hh_income))+
  geom_jitter(aes(y=poverty), alpha= 0.5)+
  geom_line(aes(y=stats::predict(fit)))
#I"m not able to understand the interpretation of this graph

#logistic Regression
mod1<- glm(poverty~median_edu + median_hh_income + unemployment_rate + metro,family="binomial", data=train1)
summary(mod1)$coef
(exp(summary(mod1)$coef[,1])*5-1)*100
table(train_data$poverty)
contrasts(train1$poverty)

# Prediction Accuracy Using Confusion matrix
prob1<-stats::predict(mod1, newdata=test_data)#  this will result in log odds ratio
prob1<-stats::predict(mod1, newdata=test_data, type="response") 
# `type=response` provides predicted probability 


pred1<- rep(0,dim(test_data)[1]) ## create a zero vector, there are 2048 rows  in the training set
## doubt: Getting probability greater than 1
pred1[prob1>0.999]=1 
tab1<-table(pred1, test_data$poverty) # confusion matrix
addmargins(tab1)
#sum(diag(tab1))/sum(tab1)# (512)/2048--> Probability of low poverty rate is --->25%


