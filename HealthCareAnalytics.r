#' Multi-Level Models using lme4
rm(list=ls())
library("readxl")
library(dplyr)
library(nlme)
library(lme4)                  
library(merTools)
library(stargazer)
#' Read data sets from nlme library
#'#' Merge the two data sets into a common data frame
#'my_data represents all the countries
my_data <- read_excel("OECD_Cleaned.xlsx",sheet="Sheet2")
#we have taken different sheets for different countries
Expenditure <- read_excel("MixedData.xlsx", sheet="AUS")
Expenditure_ISR <- read_excel("MixedData.xlsx", sheet="Israil")
Expenditure_UK <- read_excel("MixedData.xlsx", sheet="UK")
Expenditure_CND <- read_excel("MixedData.xlsx", sheet="Canada")
Expenditure_US <- read_excel("MixedData.xlsx", sheet="USA")

#code to test using MultilevelModelling
#View(my_data)
my_data <- my_data[my_data$Year>2000, ] #filtering column to take data after 2000
my_data[is.na(my_data)] = 0 #replacing na with 0
my_data$Expenditure_In_Millions <- my_data$Expenditure_In_Millions * 1000 #Scaling the Data to thousands 
my_data$Pharma_sales_in_millions <- my_data$Pharma_sales_in_millions *1000 #Scaling the Data to thousands 


# 3 models with taking  total_deaths predictor as independent variable
final_Total_deaths1 <- lmer(Total_deaths ~ Employment_Head_Count+Number_of_Hospitals+Total_deaths+Expenditure_In_Millions
                            +LifeExp_Years+  Number_of_persons+
                              (1 | Country)+(1|Year), data=my_data, REML=FALSE)

summary(final_Total_deaths1)

confint(final_Total_deaths1)
AIC(final_Total_deaths1)
fixef(final_Total_deaths1)                                       # Magnitude of fixed effect
ranef(final_Total_deaths1)                                       # Magnitude of random effect
coef(final_Total_deaths1)                                # Multicolliearity
library(lmtest)



final_Total_deaths2 <- lmer(Total_deaths ~ LifeExp_Years+Number_of_Hospitals+Expenditure_In_Millions+LifeExp_Years+
                              (1 | Country)+(1|Year), data=my_data, REML=FALSE)

summary(final_Total_deaths2)
confint(final_Total_deaths2)
AIC(final_Total_deaths2)
fixef(final_Total_deaths2)                                       # Magnitude of fixed effect
ranef(final_Total_deaths2)                                       # Magnitude of random effect
coef(final_Total_deaths2)


final_Total_deaths3 <- lmer(Total_deaths ~ Employment_Head_Count+Total_deaths+Expenditure_In_Millions
                            +  Number_of_persons+Number_of_persons+
                              (1 | Country)+(1|Year), data=my_data, REML=FALSE)

summary(final_Total_deaths3)
confint(final_Total_deaths3)
AIC(final_Total_deaths3)
fixef(final_Total_deaths3)                                       # Magnitude of fixed effect
ranef(final_Total_deaths3)                                       # Magnitude of random effect
coef(final_Total_deaths3)


#star gazer output for all the three models.
stargazer(final_Total_deaths1, final_Total_deaths2, final_Total_deaths3, type="text", single.row=TRUE)


# 3 models with taking expenditure as dpepndent variable
final_Expenditure1 <- lmer(Pharma_sales_in_millions
 ~ Employment_Head_Count+Number_of_Hospitals+Total_deaths+Total_deaths+LifeExp_Years+Number_of_persons+Number_of_persons+
                              (1 | Country)+(1|Year), data=my_data, REML=FALSE)

summary(final_Expenditure1)

confint(final_Expenditure1)
AIC(final_Expenditure1)
fixef(final_Expenditure1)                                       # Magnitude of fixed effect
ranef(final_Expenditure1)                                       # Magnitude of random effect
coef(final_Expenditure1)



final_Expenditure2 <- lmer(Pharma_sales_in_millions/1000 ~
 Number_of_Hospitals+Total_deaths+LifeExp_Years+Expenditure_In_Millions+
(1 | Country)+(1|Year), data=my_data, REML=FALSE)



summary(final_Expenditure2)
confint(final_Expenditure2)
AIC(final_Expenditure2)
fixef(final_Expenditure2)                                       # Magnitude of fixed effect
ranef(final_Expenditure2)                                       # Magnitude of random effect
coef(final_Expenditure2)



final_Expenditure3 <- lmer(Pharma_sales_in_millions
 ~ Employment_Head_Count+Total_deaths+Total_deaths
  +  Number_of_persons+Number_of_persons+(1 | Country)+(1|Year), data=my_data, REML=FALSE)

summary(final_Expenditure3)
confint(final_Expenditure3)
AIC(final_Expenditure3)
fixef(final_Expenditure3)                                       # Magnitude of fixed effect
ranef(final_Expenditure3)                                       # Magnitude of random effect
coef(final_Expenditure3)                                        # Magnitude of Total effect


stargazer(final_Expenditure1, final_Expenditure2, final_Expenditure3, type="text", single.row=TRUE)



#time series model for Australia
colnames(Expenditure)=tolower(make.names(colnames(Expenditure)))



colSums(is.na(Expenditure))
Expenditure <- subset(Expenditure, Expenditure$year!=2020)
acf(Expenditure$expenditure_in_millions)
pacf(Expenditure$expenditure_in_millions)

#To rescale and eliminate exponential numbers,these two columns are converted into Thousands.
Expenditure$expenditure_in_thousands=Expenditure$expenditure_in_millions*1000
Expenditure$pharma_sales_in_thousands=Expenditure$pharma_sales_in_millions*1000 

#The errors are not Correlated soo much and we have the PACF 1.
#After researching about the Lag we found that a lag more than 3 is not practical. So we are taking a lag of 3
#here we are taking 20 years of data , we know it is a time series even though it looks linear.So we need to use Time Series Techniques
n = nrow(Expenditure)#Numbr of rows
Expenditure$expenditure_in_thousands_lag1 <- c(NA, Expenditure$expenditure_in_thousands[1:n-1])#One lag
Expenditure$expenditure_in_thousands_lag2 <- c(NA,NA, Expenditure$expenditure_in_thousands[1:38])#Second Lag
Expenditure$expenditure_in_thousands_lag3 <- c(NA,NA,NA, Expenditure$expenditure_in_thousands[1:37])#Third Lag

#Now we have added the lag values to account for the Time Series/Effect of the pervious year on current year.

#Now we will run a time series model to check for the expenditure next year.

#Train Test Split
#we will take the 1980-2014 for trainset and 2015-2019 as test set
ExpenditureTrain = Expenditure[0:35,]
ExpenditureTest = Expenditure[36:38,]

colnames(ExpenditureTrain)
#nurses_counts+
ExpenditureTrainModelWith1LAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1+employment_head_count
                                   +no_of_persons_with_govtinsurance+number_of_hospitals+lifeexp_years+
                                     total_deaths+pharma_sales_in_thousands,data = ExpenditureTrain)
summary(ExpenditureTrainModelWith1LAg)#one LAg

ExpenditureTrainModel1With1LAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                    +no_of_persons_with_govtinsurance+lifeexp_years+
                                      total_deaths,data = ExpenditureTrain)
summary(ExpenditureTrainModel1With1LAg)
predicted=predict(ExpenditureTrainModel1With1LAg,ExpenditureTest)
predicted


ExpenditureTrainModelWithTwoLAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                     +expenditure_in_thousands_lag2+no_of_persons_with_govtinsurance+lifeexp_years+
                                       total_deaths,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithTwoLAg)
predicted=predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest)
predicted


ExpenditureTrainModelWithThreeLags = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                        +expenditure_in_thousands_lag2+expenditure_in_thousands_lag3+no_of_persons_with_govtinsurance+lifeexp_years+
                                          total_deaths,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithThreeLags)
predicted=predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest)
predicted

#Lets Evaluate the model with Root means Square error.
RMSE_LAg1 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModel1With1LAg,ExpenditureTest))^2)
RMSE_LAg1

RMSE_LAg2 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest))^2)
RMSE_LAg2

RMSE_LAg3 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest))^2)
RMSE_LAg3




#time series model for USA
colnames(Expenditure_US)=tolower(make.names(colnames(Expenditure_US)))


colSums(is.na(Expenditure_US))
Expenditure_US <- subset(Expenditure_US, Expenditure_US$year!=2020)
acf(Expenditure_US$expenditure_in_millions)
pacf(Expenditure_US$expenditure_in_millions)

Expenditure_US$expenditure_in_thousands=Expenditure_US$expenditure_in_millions*1000
#Expenditure_US$pharma_sales_in_thousands=Expenditure_US$pharma_sales_in_millions*1000 


n = nrow(Expenditure_US)#Numbr of rows
Expenditure_US$expenditure_in_thousands_lag1 <- c(NA, Expenditure_US$expenditure_in_thousands[1:n-1])#One lag
Expenditure_US$expenditure_in_thousands_lag2 <- c(NA,NA, Expenditure_US$expenditure_in_thousands[1:38])#Second Lag
Expenditure_US$expenditure_in_thousands_lag3 <- c(NA,NA,NA, Expenditure_US$expenditure_in_thousands[1:37])#Third Lag


#n = nrow(ExpenditureTrain)#Numbr of rows
#ExpenditureTrain$expenditure_in_thousands_lag1 <- c(NA, ExpenditureTrain$expenditure_in_thousands[1:n-1])#One lag
#ExpenditureTrain$expenditure_in_thousands_lag2 <- c(NA,NA, ExpenditureTrain$expenditure_in_thousands[1:33])#Second Lag
#ExpenditureTrain$expenditure_in_thousands_lag3 <- c(NA,NA,NA, ExpenditureTrain$expenditure_in_thousands[1:32])#Third Lag

#Train Test Split
#we will take the 1980-2014 for trainset and 2015-2019 as test set
ExpenditureTrain = Expenditure_US[0:35,]
ExpenditureTest = Expenditure_US[36:38,]

colnames(ExpenditureTrain)
#nurses_counts+
ExpenditureTrainModelWith1LAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1+
                                     +number_of_hospitals+lifeexp_years+
                                     total_deaths,data = ExpenditureTrain)
summary(ExpenditureTrainModelWith1LAg)#one LAg


predicted=predict(ExpenditureTrainModelWith1LAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag2
ExpenditureTrainModelWithTwoLAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                     +expenditure_in_thousands_lag2+no_of_persons_with_govtinsurance+lifeexp_years+
                                       total_deaths+expenditure_in_thousands_lag2,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithTwoLAg)

predicted=predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag3
ExpenditureTrainModelWithThreeLags = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                        +expenditure_in_thousands_lag2+no_of_persons_with_govtinsurance+lifeexp_years+
                                          total_deaths+expenditure_in_thousands_lag2+expenditure_in_thousands_lag3,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithThreeLags)
predicted=predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest)
predicted

#Lets Evaluate the model with Root means Square error.
RMSE_LAg1 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWith1LAg,ExpenditureTest))^2)
RMSE_LAg1

RMSE_LAg2 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest))^2)
RMSE_LAg2

RMSE_LAg3 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest))^2)
RMSE_LAg3





#time series model for UK
colnames(Expenditure_UK)=tolower(make.names(colnames(Expenditure_UK)))


colSums(is.na(Expenditure_UK))
Expenditure_UK <- subset(Expenditure_UK, Expenditure_UK$year!=2020)
acf(Expenditure_UK$expenditure_in_millions)
pacf(Expenditure_UK$expenditure_in_millions)

Expenditure_UK$expenditure_in_thousands=Expenditure_UK$expenditure_in_millions*1000
#Expenditure_UK$pharma_sales_in_thousands=Expenditure_UK$pharma_sales_in_millions*1000 


n = nrow(Expenditure_UK)#Numbr of rows
Expenditure_UK$expenditure_in_thousands_lag1 <- c(NA, Expenditure_UK$expenditure_in_thousands[1:n-1])#One lag
Expenditure_UK$expenditure_in_thousands_lag2 <- c(NA,NA, Expenditure_UK$expenditure_in_thousands[1:38])#Second Lag
Expenditure_UK$expenditure_in_thousands_lag3 <- c(NA,NA,NA, Expenditure_UK$expenditure_in_thousands[1:37])#Third Lag


#n = nrow(ExpenditureTrain)#Numbr of rows
#ExpenditureTrain$expenditure_in_thousands_lag1 <- c(NA, ExpenditureTrain$expenditure_in_thousands[1:n-1])#One lag
#ExpenditureTrain$expenditure_in_thousands_lag2 <- c(NA,NA, ExpenditureTrain$expenditure_in_thousands[1:33])#Second Lag
#ExpenditureTrain$expenditure_in_thousands_lag3 <- c(NA,NA,NA, ExpenditureTrain$expenditure_in_thousands[1:32])#Third Lag

#Train Test Split
#we will take the 1980-2014 for trainset and 2015-2019 as test set
ExpenditureTrain = Expenditure_UK[0:35,]
ExpenditureTest = Expenditure_UK[36:38,]

colnames(ExpenditureTrain)
#nurses_counts+
ExpenditureTrainModelWith1LAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                     ,data = ExpenditureTrain)
summary(ExpenditureTrainModelWith1LAg)#one LAg


predicted=predict(ExpenditureTrainModelWith1LAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag2
ExpenditureTrainModelWithTwoLAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                     +expenditure_in_thousands_lag2+
                                       expenditure_in_thousands_lag2,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithTwoLAg)

predicted=predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag3
ExpenditureTrainModelWithThreeLags = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                        +expenditure_in_thousands_lag2+expenditure_in_thousands_lag3,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithThreeLags)
year=c(2020,2021,2022,2023)
TestFrame = data.frame(year)
predicted=predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest)
predicted
x=coef(ExpenditureTrainModelWithThreeLags)

#Lets Evaluate the model with Root means Square error.
RMSE_LAg1 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWith1LAg,ExpenditureTest))^2)
RMSE_LAg1

RMSE_LAg2 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest))^2)
RMSE_LAg2

RMSE_LAg3 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest))^2)
RMSE_LAg3




#time series model for Israel
colnames(Expenditure_ISR)=tolower(make.names(colnames(Expenditure_ISR)))


colSums(is.na(Expenditure_ISR))
Expenditure_ISR <- subset(Expenditure_ISR, Expenditure_ISR$year!=2020)
acf(Expenditure_ISR$expenditure_in_millions)
pacf(Expenditure_ISR$expenditure_in_millions)

Expenditure_ISR$expenditure_in_thousands=Expenditure_ISR$expenditure_in_millions*1000
#Expenditure_ISR$pharma_sales_in_thousands=Expenditure_ISR$pharma_sales_in_millions*1000 


n = nrow(Expenditure_ISR)#Numbr of rows
Expenditure_ISR$expenditure_in_thousands_lag1 <- c(NA, Expenditure_ISR$expenditure_in_thousands[1:n-1])#One lag
Expenditure_ISR$expenditure_in_thousands_lag2 <- c(NA,NA, Expenditure_ISR$expenditure_in_thousands[1:38])#Second Lag
Expenditure_ISR$expenditure_in_thousands_lag3 <- c(NA,NA,NA, Expenditure_ISR$expenditure_in_thousands[1:37])#Third Lag


#n = nrow(ExpenditureTrain)#Numbr of rows
#ExpenditureTrain$expenditure_in_thousands_lag1 <- c(NA, ExpenditureTrain$expenditure_in_thousands[1:n-1])#One lag
#ExpenditureTrain$expenditure_in_thousands_lag2 <- c(NA,NA, ExpenditureTrain$expenditure_in_thousands[1:33])#Second Lag
#ExpenditureTrain$expenditure_in_thousands_lag3 <- c(NA,NA,NA, ExpenditureTrain$expenditure_in_thousands[1:32])#Third Lag

#Train Test Split
#we will take the 1980-2014 for trainset and 2015-2019 as test set
ExpenditureTrain = Expenditure_ISR[0:35,]
ExpenditureTest = Expenditure_ISR[36:38,]

colnames(ExpenditureTrain)
#nurses_counts+,+number_of_hospitals+lifeexp_years+total_deaths
ExpenditureTrainModelWith1LAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                   ,data = ExpenditureTrain)
summary(ExpenditureTrainModelWith1LAg)#one LAg


predicted=predict(ExpenditureTrainModelWith1LAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag2
ExpenditureTrainModelWithTwoLAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                     +expenditure_in_thousands_lag2
                                       ,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithTwoLAg)

predicted=predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag3
ExpenditureTrainModelWithThreeLags = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                        +expenditure_in_thousands_lag2+expenditure_in_thousands_lag3,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithThreeLags)
predicted=predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest)
predicted

#Lets Evaluate the model with Root means Square error.
RMSE_LAg1 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWith1LAg,ExpenditureTest))^2)
RMSE_LAg1

RMSE_LAg2 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest))^2)
RMSE_LAg2

RMSE_LAg3 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest))^2)
RMSE_LAg3

stargazer(ExpenditureTrainModelWith1LAg, ExpenditureTrainModelWithTwoLAg, ExpenditureTrainModelWithThreeLags, type="text", single.row=TRUE)




#time series model for Canada
colnames(Expenditure_CND)=tolower(make.names(colnames(Expenditure_CND)))


colSums(is.na(Expenditure_CND))
Expenditure_CND <- subset(Expenditure_CND, Expenditure_CND$year!=2020)
acf(Expenditure_CND$expenditure_in_millions)
pacf(Expenditure_CND$expenditure_in_millions)

Expenditure_CND$expenditure_in_thousands=Expenditure_CND$expenditure_in_millions*1000
#Expenditure_CND$pharma_sales_in_thousands=Expenditure_CND$pharma_sales_in_millions*1000 


n = nrow(Expenditure_CND)#Numbr of rows
Expenditure_CND$expenditure_in_thousands_lag1 <- c(NA, Expenditure_CND$expenditure_in_thousands[1:n-1])#One lag
Expenditure_CND$expenditure_in_thousands_lag2 <- c(NA,NA, Expenditure_CND$expenditure_in_thousands[1:38])#Second Lag
Expenditure_CND$expenditure_in_thousands_lag3 <- c(NA,NA,NA, Expenditure_CND$expenditure_in_thousands[1:37])#Third Lag


#n = nrow(ExpenditureTrain)#Numbr of rows
#ExpenditureTrain$expenditure_in_thousands_lag1 <- c(NA, ExpenditureTrain$expenditure_in_thousands[1:n-1])#One lag
#ExpenditureTrain$expenditure_in_thousands_lag2 <- c(NA,NA, ExpenditureTrain$expenditure_in_thousands[1:33])#Second Lag
#ExpenditureTrain$expenditure_in_thousands_lag3 <- c(NA,NA,NA, ExpenditureTrain$expenditure_in_thousands[1:32])#Third Lag

#Train Test Split
#we will take the 1980-2014 for trainset and 2015-2019 as test set
ExpenditureTrain = Expenditure_CND[0:35,]
ExpenditureTest = Expenditure_CND[36:38,]

colnames(ExpenditureTrain)
#nurses_counts+
ExpenditureTrainModelWith1LAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1+
                                     +number_of_hospitals+lifeexp_years+
                                     total_deaths,data = ExpenditureTrain)
summary(ExpenditureTrainModelWith1LAg)#one LAg


predicted=predict(ExpenditureTrainModelWith1LAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag2
ExpenditureTrainModelWithTwoLAg = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                     +expenditure_in_thousands_lag2+
                                       expenditure_in_thousands_lag2,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithTwoLAg)

predicted=predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest)
predicted

#expenditure_in_thousands_lag3
ExpenditureTrainModelWithThreeLags = lm(expenditure_in_thousands~year+expenditure_in_thousands_lag1
                                        +expenditure_in_thousands_lag2+expenditure_in_thousands_lag3,data = ExpenditureTrain)
summary(ExpenditureTrainModelWithThreeLags)
predicted=predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest)
predicted

#Lets Evaluate the model with Root means Square error.
RMSE_LAg1 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWith1LAg,ExpenditureTest))^2)
RMSE_LAg1

RMSE_LAg2 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithTwoLAg,ExpenditureTest))^2)
RMSE_LAg2

RMSE_LAg3 = sqrt(mean(ExpenditureTest$expenditure_in_thousands-predict(ExpenditureTrainModelWithThreeLags,ExpenditureTest))^2)
RMSE_LAg3








