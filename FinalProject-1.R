#Import ETF Data
MFs <-read.csv("~/Downloads/Updated Mutual Fund .csv")
summary(MFs)
names(MFs)

# as we are trying to predict 2019 returns
# dropping columns which include data for 2019 in it 

drop <- c("category_treynor_ratio_3years", "category_treynor_ratio_5years", "category_treynor_ratio_10years", "fund_treynor_ratio_3years","fund_treynor_ratio_5years", "fund_treynor_ratio_10years", "fund_sharpe_ratio_3years","fund_sharpe_ratio_5years",
          "fund_sharpe_ratio_10years", "category_sharpe_ratio_3years","category_sharpe_ratio_5years", "category_sharpe_ratio_10years",
          "fund_r_squared_3years","fund_r_squared_5years", "fund_r_squared_10years",
          "fund_mean_annual_return_3years", "fund_mean_annual_return_5years", "fund_mean_annual_return_10years",
          "fund_alpha_3years","fund_alpha_5years", "fund_alpha_10years", "fund_beta_3years" ,  "fund_beta_5years", "fund_beta_10years",
          "quarters_up", "quarters_down", "rating", "return_rating", "risk_rating", "investment_strategy","currency", "top10_holdings", "fund_yield", 
          "fund_return_ytd","category_return_ytd","fund_return_1month", "category_return_1month", "fund_return_3months",  "category_return_3months" , "fund_return_1year" ,"category_return_1year",   "fund_return_3years" ,                "category_return_3years"        ,      "fund_return_5years"   ,              
"category_return_5years"     ,         "fund_return_10years" ,               
"category_return_10years" , "bond_duration", "years_up", "years_down", "bond_maturity")
                        
MFdata = MFs[,!(names(MFs) %in% drop)]
summary(MFdata)
names(MFdata)

#removing NAs from the data and replacing them with the column's median

for(i in 8:ncol(MFdata)){
  MFdata[is.na(MFdata[,i]), i] <- median(MFdata[,i], na.rm = TRUE)
}
summary(MFdata)

#visualizing interesting interactions in the data
library(ggplot2)
ggplot(MFdata, aes(x=price_earnings_ratio , y=price_book_ratio)) + geom_point()
ggplot(MFdata, aes(x=median_market_cap , y=price_earnings_ratio)) + geom_point()
ggplot(MFdata, aes(x=investment_type , y=fund_standard_deviation_10years)) + geom_point()


#running a regression to find significant variable

regression <- glm(fund_return_2019 ~ . 
                  -fund_symbol -fund_extended_name -fund_family -inception_date  -category -investment_type 
                  -size_type - category_return_2019, data = MFdata)
summary(regression)

results_df <-summary.glm(regression)$coefficients


#based on Bonferroni correction we are only picking variables with p value>0.001 
#we are keeping returns for all the years even if they are no significant

regression <- glm(fund_return_2019 ~ fund_net_annual_expense_ratio
                  +category_net_annual_expense_ratio
                  +price_earnings_ratio
                  +size_type
                  +investment_type
                  +price_book_ratio
                  +price_cashflow_ratio
                  +median_market_cap
                  +fund_return_2018
                  +category_return_2018
                  +fund_return_2017
                  +category_return_2017
                  +fund_return_2016
                  +category_return_2016
                  +fund_return_2015
                  +category_return_2015
                  +fund_return_2014
                  +category_return_2014
                  +fund_return_2013
                  +category_return_2013
                  +fund_return_2012
                  +category_return_2012
                  +fund_return_2011
                  +category_return_2011
                  +fund_return_2010
                  +category_return_2010
                  +category_r_squared_3years
                  +category_r_squared_5years
                  +category_r_squared_10years
                  +fund_standard_deviation_10years, data = MFdata)

summary(regression)
results_df <-summary.glm(regression)$coefficients

#creating test and train dataset to train coefficients
install.packages("caret")
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(MFdata$fund_return_2019, p = .8, list = FALSE, times = 1)

Train <- MFdata[ trainIndex,]
Test  <- MFdata[-trainIndex,]


#Running regresssion on train data set
model <- glm(fund_return_2019 ~ fund_net_annual_expense_ratio
                  +category_net_annual_expense_ratio
                  +price_earnings_ratio
                  +price_book_ratio
                  +price_cashflow_ratio
                  +median_market_cap
                  +fund_return_2018
                  +category_return_2018
                  +fund_return_2017
                  +category_return_2017
                  +fund_return_2016
                  +category_return_2016
                  +fund_return_2015
                  +category_return_2015
                  +fund_return_2014
                  +category_return_2014
                  +fund_return_2013
                  +category_return_2013
                  +fund_return_2012
                  +category_return_2012
                  +fund_return_2011
                  +category_return_2011
                  +fund_return_2010
                  +category_return_2010
                  +category_r_squared_3years
                  +category_r_squared_5years
                  +category_r_squared_10years
                  +fund_standard_deviation_10years, data = Train)

summary(model)

#In-sample R-square
InsampleR2 <- with(summary(model), 1 - deviance/null.deviance)
InsampleR2


#Running Random-Forest
install.packages("randomForest")
library(randomForest)

modelRF <- randomForest(fund_return_2019 ~ fund_net_annual_expense_ratio
                        +category_net_annual_expense_ratio
                        +price_earnings_ratio
                        +price_book_ratio
                        +price_cashflow_ratio
                        +median_market_cap
                        +fund_return_2018
                        +category_return_2018
                        +fund_return_2017
                        +category_return_2017
                        +fund_return_2016
                        +category_return_2016
                        +fund_return_2015
                        +category_return_2015
                        +fund_return_2014
                        +category_return_2014
                        +fund_return_2013
                        +category_return_2013
                        +fund_return_2012
                        +category_return_2012
                        +fund_return_2011
                        +category_return_2011
                        +fund_return_2010
                        +category_return_2010
                        +category_r_squared_3years
                        +category_r_squared_5years
                        +category_r_squared_10years
                        +fund_standard_deviation_10years, data = Train)







#Predicting with Linear Regression & Random Forest
Test$model_prob <- predict(model, Test, type = "response")
Test$model_rf <- predict(modelRF, Test)

#Creating an CSV file for test for exploration
write.csv(Test,file="~/Downloads/TestResult.csv")


#OOS R-square for Linear Model 
test.pred <- Test$model_prob
test.y    <- Test$fund_return_2019

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
SS.regression <- sum((test.pred - mean(test.y))^2)
SS.total - (SS.regression+SS.residual)
OOS_R2_LM <- 1 - SS.residual/SS.total  
OOS_R2_LM
# OOS R-square is 0.70


#OOS R-square for Random Forest
test.pred <- Test$model_rf
test.y    <- Test$fund_return_2019

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
SS.regression <- sum((test.pred - mean(test.y))^2)
SS.total - (SS.regression+SS.residual)
OOS_R2_RF <- 1 - SS.residual/SS.total  
OOS_R2_RF

# OOS R-square is 0.914
# We will use Random Forest for prediction

modelRF <- randomForest(fund_return_2019 ~ fund_net_annual_expense_ratio
                        +category_net_annual_expense_ratio
                        +price_earnings_ratio
                        +price_book_ratio
                        +price_cashflow_ratio
                        +median_market_cap
                        +category_r_squared_3years
                        +category_r_squared_5years
                        +category_r_squared_10years
                        +fund_standard_deviation_10years, data = Train)







#Predicting with Linear Regression & Random Forest
Test$model_prob <- predict(model, Test, type = "response")
Test$model_rf <- predict(modelRF, Test)

#Creating an CSV file for test for exploration
write.csv(Test,file="~/Downloads/TestResult.csv")


#OOS R-square for Linear Model 
test.pred <- Test$model_prob
test.y    <- Test$fund_return_2019

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
SS.regression <- sum((test.pred - mean(test.y))^2)
SS.total - (SS.regression+SS.residual)
OOS_R2_LM <- 1 - SS.residual/SS.total  
OOS_R2_LM
# OOS R-square is 0.70


#OOS R-square for Random Forest
test.pred <- Test$model_rf
test.y    <- Test$fund_return_2019

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
SS.regression <- sum((test.pred - mean(test.y))^2)
SS.total - (SS.regression+SS.residual)
OOS_R2_RF <- 1 - SS.residual/SS.total  
OOS_R2_RF







modelRF <- randomForest(fund_return_2015 ~ fund_net_annual_expense_ratio
                        +category_net_annual_expense_ratio
                        +price_earnings_ratio
                        +price_book_ratio
                        +price_cashflow_ratio
                        +median_market_cap
                        +category_r_squared_3years
                        +category_r_squared_5years
                        +category_r_squared_10years
                        +fund_standard_deviation_10years, data = Train)







#Predicting with Linear Regression & Random Forest
Test$model_prob <- predict(model, Test, type = "response")
Test$model_rf <- predict(modelRF, Test)

#Creating an CSV file for test for exploration
write.csv(Test,file="~/Downloads/TestResult.csv")


#OOS R-square for Linear Model 
test.pred <- Test$model_prob
test.y    <- Test$fund_return_2015

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
SS.regression <- sum((test.pred - mean(test.y))^2)
SS.total - (SS.regression+SS.residual)
OOS_R2_LM <- 1 - SS.residual/SS.total  
OOS_R2_LM
# OOS R-square is 0.70


#OOS R-square for Random Forest
test.pred <- Test$model_rf
test.y    <- Test$fund_return_2015

SS.total      <- sum((test.y - mean(test.y))^2)
SS.residual   <- sum((test.y - test.pred)^2)
SS.regression <- sum((test.pred - mean(test.y))^2)
SS.total - (SS.regression+SS.residual)
OOS_R2_RF <- 1 - SS.residual/SS.total  
OOS_R2_RF

