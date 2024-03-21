options(scipen=999, digits=4) # avoid scientific display, keep 4 digits in display
rm(list=ls()) # clear Environment
setwd("/Users/edgr/Library/Mobile Documents/com~apple~CloudDocs/SMU tings/current/statprog/statprog proj/R_regression_model/R_regression_model/data")

##================SCRIPT START===============##
#===========PACKAGES & DEPENDENCIES===========#
install.packages('caret', dependencies = TRUE)
install.packages("corrplot")
install.packages("glmnet")
library(caret)
library(dplyr)
library(readr)
library(stats)
library(rpart)
library(glmnet)
library(corrplot)
base <- read_csv("teamproject_final.csv")

#===========PRELIMNARY CLEANING==============#
#Overarching assumption: We don't want outlier companies, or irregular companies influencing the model
#!!need to elaborate more on assumptions!! 
#assumption: no company can have 0 total assets
base_clean <- base %>% filter(!is.na(at)) #remove rows where total assets is NA
#assumption: no company can have 0 equity
base_clean1 <- base_clean %>% filter(!is.na(ceq)) #remove rows where equity is NA
#assumption:
base_clean2 <- base_clean1 %>% filter(!is.na(lt)) #remove rows where total liabilities is NA
#assumption:
base_clean3 <- base_clean2 %>% mutate(clean_act = ifelse(is.na(act), che + rect + invt ,act)) 
#assumption:
base_clean4 <- base_clean3 %>% filter(!is.na(sale))
#assumption: 
base_clean5 <- 

#===========ADDING VARIABLES==================#
#adding gross profit variable 
base1 <- base %>%
  arrange(cid, fyear) %>%
  mutate(gpm = (sale-cogs) / sale) %>% ungroup() 

#adding long-term debt  
base2 <- base1 %>% arrange(cid, fyear) %>% group_by(cid) %>% mutate(dltt_lag = lag(dltt, n = 1)) 
base3 <- base2 %>% mutate(dltt_change = dltt - dltt_lag) %>% mutate(dltt_dummy = ifelse(dltt_change > 0, 0, 1))
basedltt <- filter(base3, dltt == 0) #no 0, so assuming NA = 0 dltt
basetest <- base3 %>% group_by(fyear) %>% select(
  c(datadate, at, cid, fyear, fyr, dltt, lt, dlc, txp, txdb, lct)) 
baseyourmom <- basetest %>% filter(is.na(lt))
View(basetest)
View(baseyourmom)

#================CLEANING DATA=================#
#dltt data cleaning 


base_final_cleaned = na.omit(base_final)


#=======REMOVING UNNECESSARY COLUMNS===========#
base4 <- select(base3, c(datadate, cid, fyear, fyr, mob, gpm, dltt_dummy, n_aef, ghg))
View(base4)

##==================SCRIPT END================##