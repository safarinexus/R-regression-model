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

#===============REMOVE FINANCIAL INDUSTRY==============#
#Overarching assumption: something something
base <- base %>% filter(!FFI12_desc == "Money")

#==================PRELIMINARY CLEANING================#
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

#==================ADDING VARIABLES==================#
#Add Dummy Variable for FF12 


#Add gross profit margin column
cdata1 <- cdata%>%arrange(cid, fyear)%>%mutate(gpm = (sale-cogs) / sale)%>%ungroup() 

#Add long-term debt column 
cdata2 <- cdata1%>%arrange(cid, fyear)%>%group_by(cid)%>%mutate(dltt_lag = lag(dltt, n = 1)) 
cdata3 <- cdata2%>%mutate(dltt_change = dltt - dltt_lag)%>%mutate(dltt_dummy = ifelse(dltt_change > 0, 0, 1))

#=============REMOVING UNNECESSARY COLUMNS===========#
final_data <- select(cdata3, c(datadate, cid, fyear, fyr, mob, sale, cogs, gpm, dltt_dummy, n_aef, ghg))
df <- data.frame(final_data)

#!====================HYPOTHESIS #1=================!#
#====================REGRESSION MODEL================#
logistic_model <- glm(mob ~ gpm, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!====================HYPOTHESIS #2=================!#
#====================REGRESSION MODEL================#
#train test split of data 
logistic_model <- glm(mob ~ n_aef, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!=================HYPOTHESIS #3==============!#
#================REGRESSION MODEL==============#
#train test split of data 
logistic_model <- glm(mob ~ dltt_dummy, data = df, family = "binomial")
logistic_model

summary(logistic_model)

#!=================HYPOTHESIS #4==============!#
#================REGRESSION MODEL==============#
logistic_model <- glm(mob ~ ghg, data = df, family = "binomial")
logistic_model

summary(logistic_model)


##==================SCRIPT END================##