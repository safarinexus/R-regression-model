setwd("/Users/edgr/Library/Mobile Documents/com~apple~CloudDocs/SMU tings/current/statprog/statprog proj/R_regression_model/R_regression_model/data") #set this to wherever your data is

##================SCRIPT START===============##
#===========PACKAGES & DEPENDENCIES===========#
install.packages("corrplot")
install.packages("caTools") 
install.packages("ROCR")	 
library(caTools)
library(ROCR)
library(dplyr)
library(readr)
library(rpart)
library(corrplot)
base <- read_csv("teamproject_final.csv")

#==========REMOVE FINANCIAL INDUSTRY==========#
#Overarching assumption: something something
base_new <- base %>% filter(!FFI12_desc == "Money")

#============PRELIMNARY CLEANING==============#
#Overarching assumption: We don't want outlier companies, or irregular companies influencing the model
#!!need to elaborate more on assumptions!! 
#assumption: no company can have 0 total assets
base_clean <- base_new %>% filter(!is.na(at)) #remove rows where total assets is NA
#assumption: no company can have 0 equity
base_clean1 <- base_clean %>% filter(!is.na(ceq)) #remove rows where equity is NA
#assumption: something
base_clean2 <- base_clean1 %>% filter(!is.na(lt)) #remove rows where total liabilities is NA
#assumption: something
base_clean3 <- base_clean2 %>% mutate(clean_act = ifelse(is.na(act), che + rect + invt ,act)) 
#assumption: something
base_clean4 <- base_clean3 %>% filter(!is.na(sale)) #remove zero sales because
#assumption: something
base_clean5 <- base_clean4 %>% filter(!is.na(oancf)) #remove zero operating net cash flow because
#assumption: something
#!!do something with dltt na rows!!

#Final cleaned data renaming 
base1 <- base_clean5

#===========ADDING VARIABLES==================#
#Add gross profit variable 
base2 <- base1%>%arrange(cid, fyear)%>%mutate(gpm = (sale-cogs) / sale)%>%ungroup() 

#Add long-term debt  
base3 <- base2%>%arrange(cid, fyear)%>%group_by(cid)%>%mutate(dltt_lag = lag(dltt, n = 1)) 
base4 <- base3%>%mutate(dltt_change = dltt - dltt_lag)%>%mutate(dltt_dummy = ifelse(dltt_change > 0, 0, 1))
basedltt <- filter(base4, dltt == 0) #no dltt = 0 rows, so assuming NA = 0

#=======REMOVING UNNECESSARY COLUMNS===========#
base5 <- select(base4, c(datadate, cid, fyear, fyr, mob, gpm, dltt_dummy, n_aef, ghg))
View(base5)

#!=================HYPOTHESIS #1==============!#
#================REGRESSION MODEL==============#
#train test split of data 
df <- data.frame(base5)
split <- sample.split(df, SplitRatio = 0.8)

train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")

#training the model 
logistic_model <- glm(mob ~ gpm, data = train_reg, family = "binomial")
logistic_model

summary(logistic_model)

#testing the model with sample 
predict_reg <- predict(logistic_model, test_reg, type = "response")
predict_reg

#!=================HYPOTHESIS #2==============!#
#================REGRESSION MODEL==============#
#train test split of data 
split <- sample.split(df, SplitRatio = 0.8)

train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")

#training the model 
logistic_model <- glm(mob ~ n_aef, data = train_reg, family = "binomial")
logistic_model

summary(logistic_model)

#testing the model with sample 
predict_reg <- predict(logistic_model, test_reg, type = "response")
predict_reg

#!=================HYPOTHESIS #3==============!#
#================REGRESSION MODEL==============#
#train test split of data 
split <- sample.split(df, SplitRatio = 0.8)

train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")

#training the model 
logistic_model <- glm(mob ~ dltt_dummy, data = train_reg, family = "binomial")
logistic_model

summary(logistic_model)

#testing the model with sample 
predict_reg <- predict(logistic_model, test_reg, type = "response")
predict_reg

#!=================HYPOTHESIS #4==============!#
#================REGRESSION MODEL==============#
#train test split of data 
split <- sample.split(df, SplitRatio = 0.8)

train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")

#training the model 
logistic_model <- glm(mob ~ ghg, data = train_reg, family = "binomial")
logistic_model

summary(logistic_model)

#testing the model with sample 
predict_reg <- predict(logistic_model, test_reg, type = "response")
predict_reg

##==================SCRIPT END================##