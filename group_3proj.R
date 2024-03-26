setwd("/Users/edgr/Library/Mobile Documents/com~apple~CloudDocs/SMU tings/current/statprog/statprog proj/R_regression_model/R_regression_model/data") #set this to wherever your data is

##=====================SCRIPT START===================##
#================PACKAGES & DEPENDENCIES===============#
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

#===============REMOVE FINANCIAL INDUSTRY==============#
#Overarching assumption: something something
base <- base %>% filter(!FFI12_desc == "Money")

#=================PRELIMINARY CLEANING=================#
#Overarching assumption: We don't want outlier companies, or irregular companies influencing the model
#Removing all NA and negative values from Dataset
na_rows <- which(is.na(base$datadate)|is.na(base$fyear)|is.na(base$fyr)|is.na(base$act)|is.na(base$at)|is.na(base$capx)
                 |is.na(base$ceq)|is.na(base$che)|is.na(base$sale)|is.na(base$cogs)|is.na(base$dlc)|is.na(base$dltt)
                 |is.na(base$dp)|is.na(base$dvc)|is.na(base$gdwl)|is.na(base$ib)|is.na(base$intan)|is.na(base$intano)
                 |is.na(base$invt)|is.na(base$lct)|is.na(base$lt)|is.na(base$ni)|is.na(base$oancf)|is.na(base$oiadp)
                 |is.na(base$ppent)|is.na(base$re)|is.na(base$rect)|is.na(base$txdb)|is.na(base$txp)|is.na(base$rdq)
                 |is.na(base$INST_OWN)|is.na(base$n_aef)|is.na(base$gc)|is.na(base$icw)|is.na(base$restate)
                 |is.na(base$big4)|is.na(base$cid)|is.na(base$ghg)|base$ceq < 0|base$lt < 0|base$capx < 0|base$che < 0
                 |base$sale < 0|base$cogs < 0|base$dlc < 0|base$dltt < 0|base$dp < 0|base$dvc < 0|base$gdwl < 0
                 |base$intan < 0|base$intano < 0|base$invt < 0|base$lct < 0|base$lt < 0|base$ppent < 0|base$rect < 0
                 |base$txp < 0|base$xad < 0|base$xpr < 0|base$xrd < 0|base$xsga < 0|base$prcc_f < 0|base$rdq < 0
                 |base$INST_OWN < 0 |base$n_aef < 0 |base$gc < 0 |base$icw < 0 |base$restate < 0 |base$big4 < 0 
                 |base$cid < 0 |base$ghg < 0)
  
companies_to_remove <- unique(base$cid[na_rows])
base1 <- base[!(base$cid %in% companies_to_remove), ]
summary(base1)
#Final cleaned data renaming 
cdata <- base1

#===================ADDING VARIABLES===================#
#Add Dummy Variable for FF12 
install.packages("caret")
library(caret) 
unique(base$FFI12_desc)
base_list <- dummyVars(" ~ FFI12_desc ", data = cdata) 
View(base_list )
cdata_dummy <- data.frame(predict(base_list, cdata))
View(cdata_dummy)
base_final <- bind_cols(cdata, cdata_dummy)
View(base_final)
summary(base_final)

#Add gross profit margin column
cdata1 <- base_final%>%arrange(cid, fyear)%>%mutate(gpm = (sale-cogs) / sale)%>%ungroup() 

#Add long-term debt column 
cdata2 <- cdata1%>%arrange(cid, fyear)%>%group_by(cid)%>%mutate(dltt_lag = lag(dltt, n = 1)) 
cdata3 <- cdata2%>%mutate(dltt_change = dltt - dltt_lag)%>%mutate(dltt_dummy = ifelse(dltt_change > 0, 0, 1))

#=============REMOVING UNNECESSARY COLUMNS=============#
final_data <- select(cdata3, c(datadate, cid, fyear, fyr, mob, sale, cogs, gpm, dltt_dummy, INST_OWN, n_aef, ghg))
df <- data.frame(final_data)

#===================Checking for skewness and Log()===================#
#Checking x-variables (GPM)
summary(df$gpm)
plot(density(df$gpm)) 
#GPM graph quite left skewed; median higher than mean
# Filter out rows with negative values of gpm becus cannot do becus got negative gpm
df <- df[df$gpm >= 0, ]
gpm_logged <- df %>% mutate(log_gpm = log(1+gpm))
summary(gpm_logged$log_gpm)
plot(density(gpm_logged$log_gpm))

#Checking x-variables (dltt_dummy)
summary(gpm_logged$dltt_dummy)
#need to remove NAs to see graph
selected_rows <- gpm_logged[!is.na(gpm_logged$dltt_dummy), ] 
summary(selected_rows$dltt_dummy)
plot(density(selected_rows$dltt_dummy)) 
#dltt_dummy graph quite unique; #got a bimodal graph; two humps 

#Checking x-variables (n_aef)
summary(selected_rows$n_aef)
plot(density(selected_rows$n_aef)) 
#n_aef graph quite right skewed; mean higher than median
n_aef_logged <- selected_rows %>% mutate(log_n_aef = log(1+n_aef))
summary(n_aef_logged$log_n_aef)
plot(density(n_aef_logged$log_n_aef))

#Checking x-variables (ghg)
summary(n_aef_logged$ghg)
plot(density(n_aef_logged$ghg)) 
#ghg graph quite right skewed; mean higher than median
ghg_logged <- n_aef_logged %>% mutate(log_ghg = log(1+ghg))
summary(ghg_logged$log_ghg)
plot(density(ghg_logged$log_ghg))

#Checking x-variables (INST_OWN)
summary(ghg_logged$INST_OWN)
plot(density(ghg_logged$INST_OWN)) 
#INST_OWN graph quite right skewed; mean higher than median
INST_OWN_logged <- ghg_logged %>% mutate(log_INST_OWN = log(1+INST_OWN))
summary(INST_OWN_logged$log_INST_OWN)
plot(density(INST_OWN_logged$log_INST_OWN))

#Checking y-variables (mob)
summary(INST_OWN_logged$mob)
plot(density(INST_OWN_logged$mob)) 
#mob graph quite unique; #got a bimodal graph; two humps 
#same as dltt_dummy when these kind of bimodal graph with two humps even with log function -> 
#it will still have similar graph pattern so doesnt matter log or not 

#!====================HYPOTHESIS #1===================!#
#====================REGRESSION MODEL==================#
logistic_model <- glm(mob ~ gpm, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!====================HYPOTHESIS #2===================!#
#====================REGRESSION MODEL==================#
#train test split of data 
logistic_model <- glm(mob ~ n_aef, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!======================HYPOTHESIS #3=================!#
#=====================REGRESSION MODEL=================#
#train test split of data 
logistic_model <- glm(mob ~ dltt_dummy, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!======================HYPOTHESIS #4=================!#
#=====================REGRESSION MODEL=================#
logistic_model <- glm(mob ~ ghg, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!=====================ENDOGENEITY====================!#
#use better judgement to determine what "hidden" variables might affect dependent variables 
#just run correlation plots to highlight endogeneity
df1 <- data.frame(cdata3)
numeric_cols <- sapply(df1, is.numeric)
View(numeric_cols)
exclude_cols <- c("datadate", "fyear", "fyr", "FFI12_desc")
numeric_df <- df1[, numeric_cols]
correlation_matrix <- cor(numeric_df)
corrplot(cor(numeric_df))
# INST_OWN, big4, ghg, gdwl,intan

#!=====================MULTICOLLINEARITY====================!#
library(car)
install.packages("stargazer")
library(stargazer)
model1<- lm(mob ~ gc+ cid+ rdq+ icw+ gpm+ ppent+ txdb+ txp+ capx, data = df1)
vif_values1 <- vif(model1)
vif_values1 <- car::vif(model1)
model2 <- lm(mob ~ n_aef+ dvc+ dp+ restate+ ni+ ib+ intano+ oancf+ dlc+ ceq+ cogs+ che+ dltt+ oiadp+ re+ at+ lt+ invt+ lct+ rect+ sale, data = df1)
vif_values2 <- vif(model2)
vif_values2 <- car::vif(model2)
stargazer(vif_values1, vif_values2, type="text",title="Multicollinearity results",omit = c("Constant"), digits=2,  no.space = TRUE)

##=====================SCRIPT END=====================##









options(scipen=999, digits=4) # avoid scientific display, keep 4 digits in display
rm(list=ls())

#===============Start of project==============#
library(dplyr)
library(readr)
base <- read_csv("teamproject_final.csv")

base <- read_csv("teamproject_final.csv")

#===============REMOVE FINANCIAL INDUSTRY==============#
#Overarching assumption: something something
base <- base %>% filter(!FFI12_desc == "Money")

#=================PRELIMINARY CLEANING=================#
#Overarching assumption: We don't want outlier companies, or irregular companies influencing the model
#Removing all NA and negative values from Dataset
na_rows <- which(is.na(base$datadate)|is.na(base$fyear)|is.na(base$fyr)|is.na(base$act)|is.na(base$at)|is.na(base$capx)
                 |is.na(base$ceq)|is.na(base$che)|is.na(base$sale)|is.na(base$cogs)|is.na(base$dlc)|is.na(base$dltt)
                 |is.na(base$dp)|is.na(base$dvc)|is.na(base$gdwl)|is.na(base$ib)|is.na(base$intan)|is.na(base$intano)
                 |is.na(base$invt)|is.na(base$lct)|is.na(base$lt)|is.na(base$ni)|is.na(base$oancf)|is.na(base$oiadp)
                 |is.na(base$ppent)|is.na(base$re)|is.na(base$rect)|is.na(base$txdb)|is.na(base$txp)|is.na(base$rdq)
                 |is.na(base$INST_OWN)|is.na(base$n_aef)|is.na(base$gc)|is.na(base$icw)|is.na(base$restate)
                 |is.na(base$big4)|is.na(base$cid)|is.na(base$ghg)|base$ceq < 0|base$lt < 0|base$capx < 0|base$che < 0
                 |base$sale < 0|base$cogs < 0|base$dlc < 0|base$dltt < 0|base$dp < 0|base$dvc < 0|base$gdwl < 0
                 |base$intan < 0|base$intano < 0|base$invt < 0|base$lct < 0|base$lt < 0|base$ppent < 0|base$rect < 0
                 |base$txp < 0|base$xad < 0|base$xpr < 0|base$xrd < 0|base$xsga < 0|base$prcc_f < 0|base$rdq < 0
                 |base$INST_OWN < 0 |base$n_aef < 0 |base$gc < 0 |base$icw < 0 |base$restate < 0 |base$big4 < 0 
                 |base$cid < 0 |base$ghg < 0)

companies_to_remove <- unique(base$cid[na_rows])
base1 <- base[!(base$cid %in% companies_to_remove), ]
summary(base1)
#Final cleaned data renaming 
cdata <- base1

#===================ADDING VARIABLES===================#
#Add Dummy Variable for FF12 
install.packages("caret")
library(caret) 
install.packages("dplyr") 
library(dplyr)
unique(base$FFI12_desc)
base_list <- dummyVars(" ~ FFI12_desc ", data = cdata) 
View(base_list )
cdata_dummy <- data.frame(predict(base_list, cdata))
View(cdata_dummy)
base_final <- bind_cols(cdata, cdata_dummy)
View(base_final)
summary(base_final)

#Add gross profit margin column
cdata1 <- base_final%>%arrange(cid, fyear)%>%mutate(gpm = (sale-cogs) / sale)%>%ungroup() 

#Add long-term debt column 
cdata2 <- cdata1%>%arrange(cid, fyear)%>%group_by(cid)%>%mutate(dltt_lag = lag(dltt, n = 1)) 
cdata3 <- cdata2%>%mutate(dltt_change = dltt - dltt_lag)%>%mutate(dltt_dummy = ifelse(dltt_change > 0, 0, 1))

#=============REMOVING UNNECESSARY COLUMNS=============#
final_data <- select(cdata3, c(datadate, cid, fyear, fyr, at, mob, sale, cogs, gpm, dltt_dummy, INST_OWN, n_aef, ghg, FFI12_descEnrgy))
df <- data.frame(final_data)

#===================Checking for skewness and Log()===================#
#Checking x-variables (GPM)
summary(df$gpm)
plot(density(df$gpm)) 
#GPM graph quite left skewed; median higher than mean
# Filter out rows with negative values of gpm becus cannot do becus got negative gpm
df <- df[df$gpm >= 0, ]
gpm_logged <- df %>% mutate(log_gpm = log(1+gpm))
summary(gpm_logged$log_gpm)
plot(density(gpm_logged$log_gpm))

#Checking x-variables (dltt_dummy)
summary(gpm_logged$dltt_dummy)
#need to remove NAs to see graph
selected_rows <- gpm_logged[!is.na(gpm_logged$dltt_dummy), ] 
summary(selected_rows$dltt_dummy)
plot(density(selected_rows$dltt_dummy)) 
#dltt_dummy graph quite unique; #got a bimodal graph; two humps 

#Checking x-variables (n_aef)
summary(selected_rows$n_aef)
plot(density(selected_rows$n_aef)) 
#n_aef graph quite right skewed; mean higher than median
n_aef_logged <- selected_rows %>% mutate(log_n_aef = log(1+n_aef))
summary(n_aef_logged$log_n_aef)
plot(density(n_aef_logged$log_n_aef))

#Checking x-variables (ghg)
summary(n_aef_logged$ghg)
plot(density(n_aef_logged$ghg)) 
#ghg graph quite right skewed; mean higher than median
ghg_logged <- n_aef_logged %>% mutate(log_ghg = log(1+ghg))
summary(ghg_logged$log_ghg)
plot(density(ghg_logged$log_ghg))

#Checking x-variables (INST_OWN)
summary(ghg_logged$INST_OWN)
plot(density(ghg_logged$INST_OWN)) 
#INST_OWN graph quite right skewed; mean higher than median
INST_OWN_logged <- ghg_logged %>% mutate(log_INST_OWN = log(1+INST_OWN))
summary(INST_OWN_logged$log_INST_OWN)
plot(density(INST_OWN_logged$log_INST_OWN))

#Checking y-variables (mob)
summary(INST_OWN_logged$mob)
plot(density(INST_OWN_logged$mob)) 
#mob graph quite unique; #got a bimodal graph; two humps 
#same as dltt_dummy when these kind of bimodal graph with two humps even with log function -> 
#it will still have similar graph pattern so doesnt matter log or not 

#========Linear Regression of the 5 variables and some with log()=======#

reg1 = lm(formula = mob ~ gpm + dltt_dummy + n_aef + ghg + INST_OWN, data=INST_OWN_logged)
summary(reg1)
install.packages("car")
library(car)
vif(reg1) # vif less than 10 so no significant concern for multicollinearity issue 

reg2_with_log = lm(formula = mob ~ log_gpm + dltt_dummy + log_n_aef + log_ghg + log_INST_OWN, data=INST_OWN_logged)
summary(reg2_with_log)
vif(reg2_with_log) # vif less than 10 so no significant concern for multicollinearity issue 

reg3_without_logging_gpm = lm(formula = mob ~ gpm + dltt_dummy + log_n_aef + log_ghg + log_INST_OWN, data=INST_OWN_logged)
summary(reg3_without_logging_gpm)
vif(reg3_without_logging_gpm) # vif less than 10 so no significant concern for multicollinearity issue 

#========Endogenity & multicolinearity=======#
# Calculate correlation for mob, gpm, dltt_dummy, n_aef, ghg, INST_OWN
base_endogenity <- INST_OWN_logged %>% select(mob, gpm, dltt_dummy, n_aef, ghg, INST_OWN)

# Calculate correlation matrix
cor(base_endogenity)

# Create a correlation plot
install.packages("corrplot")
library(corrplot)
corrplot(cor(base_endogenity))

#=============Endogenity of at==============#
# Calculate correlation for mob, gpm, dltt_dummy, n_aef, ghg, INST_OWN, at
base_endogenity_2 <- INST_OWN_logged %>% select(mob, gpm, dltt_dummy, n_aef, ghg, INST_OWN, at)

# Calculate correlation matrix
cor(base_endogenity_2)

# Create a correlation plot
corrplot(cor(base_endogenity_2))

#========Run a linear regression with at and the 5 variables====#

reg4_with_at = lm(formula = mob ~ gpm + dltt_dummy + n_aef + ghg + INST_OWN + at, data=INST_OWN_logged)
summary(reg4_with_at) #at is still statistically significant but not as much compared to the rest of the variables 
#so since it is highly correlated with our x variables ghg and affects mob, we therefore need to add total assets into our model
install.packages("car")
library(car)
vif(reg4_with_at) # vif less than 10 so no significant concern for multicollinearity issue 


#======Run a linear regression with ghg but within energy industry====#
# Regression by Industry
# Exercise
table(INST_OWN_logged$FFI12_descEnrgy)

# NOT sure which to use all variables or just ghg but both also not statistically significant
for (i in c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth", "Manuf", "Money", "NoDur", "Other", "Shops", "Telcm", "Utils"))
{
  base_industry <- INST_OWN_logged %>% filter(FFI12_descEnrgy == 1)
  reg_industry <- lm(formula = mob ~ ghg, data = base_industry)
  print(summary(reg_industry))
}

for (i in c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth", "Manuf", "Money", "NoDur", "Other", "Shops", "Telcm", "Utils"))
{
  base_industry <- INST_OWN_logged %>% filter(FFI12_descEnrgy == 1)
  reg_industry <- lm(formula = mob ~ gpm + dltt_dummy + n_aef + ghg + INST_OWN + at, data = base_industry)
  print(summary(reg_industry))
}


#=======Train and test=====#
# we use sample_frac function and anti_join function
train <- sample_frac(INST_OWN_logged, 0.6)
test <- anti_join(INST_OWN_logged, train)
project_reg <- lm(mob ~ gpm + dltt_dummy + n_aef + ghg + INST_OWN + at, data = train) #use the 60% of the data only 
summary(project_reg)
#Prediction for all observations in the test sample
# Using the trained model to make prediction on the test sample
install.packages("forecast")
library(forecast)
project_reg_pred <- predict(project_reg, test) #predicts outcome variable (mob in our case), project reg is from "train" data set, use coefficient from project reg to use to predict out "test" data
View(project_reg_pred)
# we need to calculate the Error as Actual - Predicted
project_reg_error <- test$mob - project_reg_pred #test data set actual price - (coefficient is from "train" data set to predict "test" data set -> the straight line)
# error = actual data points (dots) - predicted values (values on the regression line)
# combine Predicted, Actual, and Error as a dataframe
# they are all vectors, so we can use cbind
project_reg_final <- data.frame(cbind(actual_mob=test$mob , project_reg_pred,  project_reg_error))
View(project_reg_final)

# Compute accuracy
# accuracy(predicted value, actual value)
# accuracy is a part of forecast package
accuracy(project_reg_pred, test$mob) #outputs error measures (we want low values of these measures)
#results are lower than the sample in class so i think it is accurate 
#MPE and MAPE are inf due the division by zero 

