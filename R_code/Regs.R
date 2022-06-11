rm(list = ls())

library(stargazer)
library(dplyr)
library(tidyr)

df_SG_reg_test <- read.csv('SG_reg_test.csv')
df_SG_reg_test$gsector <- as.factor(df_SG_reg_test$gsector)
df_SG_reg_total <- read.csv('SG_reg_total.csv')
df_SG_reg_total$gsector <- as.factor(df_SG_reg_total$gsector)

Stest <- lm(S_pillar ~ GS.score + factor(gsector) , data = df_SG_reg_test)
Stotal<- lm(S_pillar ~ GS.score + factor(gsector) , data = df_SG_reg_total)
Gtest <- lm(G_pillar ~ GS.score + factor(gsector) , data = df_SG_reg_test)
Gtotal <- lm(G_pillar ~ GS.score + factor(gsector) , data = df_SG_reg_total)

Stest_cluster_se <- as.vector(summary(Stest,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])
Stotal_cluster_se <- as.vector(summary(Stotal,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])
Gtest_cluster_se <- as.vector(summary(Gtest,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])
Gtotal_cluster_se <- as.vector(summary(Gtotal,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])

stargazer(Stest,Stotal,Gtest,Gtotal, #type="text", 
          se=list(Stest_cluster_se,Stotal_cluster_se,
                  Gtest_cluster_se,Gtotal_cluster_se),
          intercept.bottom = FALSE,
          column.sep.width = '-1pt', type = 'text')
###############################################################################
###############################################################################
# add interaction with environmental controversies 
specification <- S_pillar ~ GS.score*env_cont + factor(gsector)   
specificationG <- G_pillar ~ GS.score*env_cont + factor(gsector) 

# 
env_controversies <- read.csv('panel_env_controversies.csv')

SG_test <- merge(df_SG_reg_test, env_controversies, by = c('ISIN', 'year'), all.x = TRUE)
SG_test$env_cont <- ifelse(is.na(SG_test$env_controversies),0,1)

modStest <- lm(specification, data = SG_test)
modGtest <- lm(specificationG, data = SG_test)

modStest_cluster_se <- as.vector(summary(modStest,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])
modGtest_cluster_se <- as.vector(summary(modGtest,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])

stargazer(modStest, modGtest, se = list(modStest_cluster_se, modGtest_cluster_se), type = "text")
# interaction is significant for S not for G 

# total
SG_total <- merge(df_SG_reg_total, env_controversies, by = c('ISIN', 'year'), all.x = TRUE)
SG_total$env_cont <- ifelse(is.na(SG_total$env_controversies),0,1)

modStotal <- lm(specification, data = SG_total)
modGtotal <- lm(specificationG, data = SG_total)

modStotal_cluster_se <- as.vector(summary(modStotal,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])
modGtotal_cluster_se <- as.vector(summary(modGtotal,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])

stargazer(modStotal, modGtotal, se = list(modStotal_cluster_se, modGtotal_cluster_se), type = "text")

stargazer(modStest, modStotal, modGtest, modGtotal, 
          se = list(modStest_cluster_se, modStotal_cluster_se,  
                    modGtest_cluster_se, modGtotal_cluster_se), 
          type = 'text', 
          intercept.bottom = FALSE,
          column.sep.width = '-1pt')
###############################################################################
###############################################################################
###############################################################################
rm(list = ls())
# roa 
All_ctrl_roa <- read.csv('All_ctrl_roa.csv')
test_ctrl_roa <- read.csv('ctrl_roa.csv')
roa <- read.csv('panel_roa.csv')
df_SG_reg_test <- read.csv('SG_reg_test.csv')
df_SG_reg_total <- read.csv('SG_reg_total.csv')

test <- merge(df_SG_reg_test, roa, by = c("ISIN", "year"), all.x = TRUE)
test <- merge(test, All_ctrl_roa, by = c("ISIN", "year"), all.x = TRUE)
test$log_sales <- log(test$Rev)
test$cogs_sales <- test$cogs/test$Rev

total <- merge(df_SG_reg_total, roa, by = c("ISIN", "year"), all.x = TRUE)
total <- merge(total, All_ctrl_roa, by = c("ISIN", "year"), all.x = TRUE)
total$log_sales <- log(total$Rev)
total$cogs_sales <- total$cogs/total$Rev
str(test)

roatest <- lm(roa ~ GS.score + log_sales + Debt_TotCap + stlt_lia + curr_rat + cogs_sales , data = test)
roatotal <- lm(roa ~ GS.score + log_sales + Debt_TotCap + stlt_lia + curr_rat + cogs_sales , data = total)

roatest_cluster_se <- as.vector(summary(roatest,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])
roatotal_cluster_se <- as.vector(summary(roatotal,cluster = c("ISIN", "year"))$coefficients[,"Std. Error"])


# this one is good to report 
stargazer(roatest, roatotal, se = list(roatest_cluster_se, roatotal_cluster_se),
          intercept.bottom = FALSE)#, type = "text" )

