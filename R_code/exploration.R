rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(panelr)
library(data.table)

wd <- setwd("C:/Users/kylia/OneDrive/Msc BAM/Thesis/Data/")

# From WRDS
df_WRDS <- read.csv("Thesisdf.csv")
df_WRDS <- df_WRDS[!duplicated(df_WRDS$isin),]

energy_data <- read.csv("Energy_Data.csv")
names(energy_data)[1] <- "Country"

# Change column names 
names(energy_data)[names(energy_data) == "X2008"] <- "2008"
names(energy_data)[names(energy_data) == "X2009"] <- "2009"
names(energy_data)[names(energy_data) == "X2010"] <- "2010"
names(energy_data)[names(energy_data) == "X2011"] <- "2011"
names(energy_data)[names(energy_data) == "X2012"] <- "2012"
names(energy_data)[names(energy_data) == "X2013"] <- "2013"
names(energy_data)[names(energy_data) == "X2014"] <- "2014"
names(energy_data)[names(energy_data) == "X2015"] <- "2015"
names(energy_data)[names(energy_data) == "X2016"] <- "2016"
names(energy_data)[names(energy_data) == "X2017"] <- "2017"
names(energy_data)[names(energy_data) == "X2018"] <- "2018"
names(energy_data)[names(energy_data) == "X2019"] <- "2019"
names(energy_data)[names(energy_data) == "X2020"] <- "2020"

panel_energy <- melt(setDT(energy_data), measure.vars = as.character(2008:2020), variable.name = "year")
names(panel_energy)[3] <- "FuelInten"

isin_panel <- read.csv("isin.csv")
isin_panel$iso <- substr(isin_panel$x, start =1, stop = 2)

countrycodes <- read.csv("country_iso.csv")

isin_match <- merge(x = isin_panel, y = countrycodes, by.x = "iso", by.y = 2, all.x = TRUE)
names(isin_match)[4] <- "Country"
names(isin_match)[3] <- "ISIN"
isin_match <- select(isin_match, -"X")

# Income group from world bank 
# Source:
# https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html#:~:text=The%20World%20Bank%20classifies%20economies,%2Dmiddle%2C%20and%20high%20income.

incomegroup <- read.csv("data-XHzgJ.csv")
names(incomegroup)[1] <- "Country"
incomegroup <- subset(incomegroup, select = c("Country", "Income.group"))

# CO2 law
# Source:
# https://carbonpricingdashboard.worldbank.org/map_data
CO2law <- read.csv("CO2law.csv")
names(CO2law)[1] <- "Country"
names(CO2law)[2] <- "CO2_imp"

# Eikon csv
# Scope 1 and 2 
scope12 <- read.csv("eikon_csvs/scope12.csv")
scope12 <- subset(scope12, Name!= "#ERROR")
scope12 <- subset(scope12, select = -c(CURRENCY, Name))

scopes_all <- scope12

scopes_all$ISIN <- substr(scopes_all$Code, start = 1, stop = 12)
scopes_all$varcode <- substr(scopes_all$Code, start = 14, stop = 22)

years_s1 <- subset(scopes_all, scopes_all$varcode == "ENERDP024")

# Change column names 
names(years_s1)[names(years_s1) == "X2008"] <- "2008"
names(years_s1)[names(years_s1) == "X2009"] <- "2009"
names(years_s1)[names(years_s1) == "X2010"] <- "2010"
names(years_s1)[names(years_s1) == "X2011"] <- "2011"
names(years_s1)[names(years_s1) == "X2012"] <- "2012"
names(years_s1)[names(years_s1) == "X2013"] <- "2013"
names(years_s1)[names(years_s1) == "X2014"] <- "2014"
names(years_s1)[names(years_s1) == "X2015"] <- "2015"
names(years_s1)[names(years_s1) == "X2016"] <- "2016"
names(years_s1)[names(years_s1) == "X2017"] <- "2017"
names(years_s1)[names(years_s1) == "X2018"] <- "2018"
names(years_s1)[names(years_s1) == "X2019"] <- "2019"
names(years_s1)[names(years_s1) == "X2020"] <- "2020"

# wide to long
years_s1 <- years_s1[,!(names(years_s1) %in% c("varcode","Code"))]

panel_s1 <- melt(setDT(years_s1), measure.vars = as.character(2008:2020), variable.name = "year")

panel_s1$value <- as.numeric(as.character(panel_s1$value))
names(panel_s1)[names(panel_s1) == "value"] <- "scope1"

# Scope 2
years_s2 <- subset(scopes_all, scopes_all$varcode == "ENERDP025")
names(years_s2)[names(years_s2) == "X2008"] <- "2008"
names(years_s2)[names(years_s2) == "X2009"] <- "2009"
names(years_s2)[names(years_s2) == "X2010"] <- "2010"
names(years_s2)[names(years_s2) == "X2011"] <- "2011"
names(years_s2)[names(years_s2) == "X2012"] <- "2012"
names(years_s2)[names(years_s2) == "X2013"] <- "2013"
names(years_s2)[names(years_s2) == "X2014"] <- "2014"
names(years_s2)[names(years_s2) == "X2015"] <- "2015"
names(years_s2)[names(years_s2) == "X2016"] <- "2016"
names(years_s2)[names(years_s2) == "X2017"] <- "2017"
names(years_s2)[names(years_s2) == "X2018"] <- "2018"
names(years_s2)[names(years_s2) == "X2019"] <- "2019"
names(years_s2)[names(years_s2) == "X2020"] <- "2020"

# wide to long
years_s2 <- years_s2[,!(names(years_s2) %in% c("varcode","Code"))]

panel_s2 <- melt(setDT(years_s2), measure.vars = as.character(2008:2020), variable.name = "year")

panel_s2$value <- as.numeric(as.character(panel_s2$value))
names(panel_s2)[names(panel_s2) == "value"] <- "scope2"

panel_all <- merge(panel_s1, panel_s2, by = c("ISIN", "year"), all = TRUE)

##################################################
##################################################
##################################################
# scope 3 and total 
###
scope3t <- read.csv("eikon_csvs/scope3total.csv")
scope3t <- subset(scope3t, scope3t[1]!= "#ERROR")
scope3t <- subset(scope3t, select = -c(CURRENCY))
scope3t <- subset(scope3t, select = -c(1))

scopes_all <- scope3t

scopes_all$ISIN <- substr(scopes_all$Code, start = 1, stop = 12)
scopes_all$varcode <- substr(scopes_all$Code, start = 14, stop = 22)

years_s3 <- subset(scopes_all, scopes_all$varcode == "ENERDP096")

# Change column names 
names(years_s3)[names(years_s3) == "X2008"] <- "2008"
names(years_s3)[names(years_s3) == "X2009"] <- "2009"
names(years_s3)[names(years_s3) == "X2010"] <- "2010"
names(years_s3)[names(years_s3) == "X2011"] <- "2011"
names(years_s3)[names(years_s3) == "X2012"] <- "2012"
names(years_s3)[names(years_s3) == "X2013"] <- "2013"
names(years_s3)[names(years_s3) == "X2014"] <- "2014"
names(years_s3)[names(years_s3) == "X2015"] <- "2015"
names(years_s3)[names(years_s3) == "X2016"] <- "2016"
names(years_s3)[names(years_s3) == "X2017"] <- "2017"
names(years_s3)[names(years_s3) == "X2018"] <- "2018"
names(years_s3)[names(years_s3) == "X2019"] <- "2019"
names(years_s3)[names(years_s3) == "X2020"] <- "2020"

# wide to long
years_s3 <- years_s3[,!(names(years_s3) %in% c("varcode","Code"))]

panel_s3 <- melt(setDT(years_s3), measure.vars = as.character(2008:2020), variable.name = "year")

panel_s3$value <- as.numeric(as.character(panel_s3$value))
names(panel_s3)[names(panel_s3) == "value"] <- "scope3"

panel_all <- merge(panel_all, panel_s3, by = c("ISIN", "year"), all = TRUE)

# Scope t
years_st <- subset(scopes_all, scopes_all$varcode == "ENERDP023")
names(years_st)[names(years_st) == "X2008"] <- "2008"
names(years_st)[names(years_st) == "X2009"] <- "2009"
names(years_st)[names(years_st) == "X2010"] <- "2010"
names(years_st)[names(years_st) == "X2011"] <- "2011"
names(years_st)[names(years_st) == "X2012"] <- "2012"
names(years_st)[names(years_st) == "X2013"] <- "2013"
names(years_st)[names(years_st) == "X2014"] <- "2014"
names(years_st)[names(years_st) == "X2015"] <- "2015"
names(years_st)[names(years_st) == "X2016"] <- "2016"
names(years_st)[names(years_st) == "X2017"] <- "2017"
names(years_st)[names(years_st) == "X2018"] <- "2018"
names(years_st)[names(years_st) == "X2019"] <- "2019"
names(years_st)[names(years_st) == "X2020"] <- "2020"

# wide to long
years_st <- years_st[,!(names(years_st) %in% c("varcode","Code"))]

panel_st <- melt(setDT(years_st), measure.vars = as.character(2008:2020), variable.name = "year")

panel_st$value <- as.numeric(as.character(panel_st$value))
names(panel_st)[names(panel_st) == "value"] <- "scope_total"

panel_all <- merge(panel_all, panel_st, by = c("ISIN", "year"), all = TRUE)

panel_all <-  panel_all[ apply( panel_all, 1, function(x) sum(is.na(x))<4 ), ]

# predictors
# Rev and emp
RevEmp <- read.csv("eikon_csvs/RevEmp.csv")
RevEmp <- subset(RevEmp, RevEmp[1]!= "#ERROR")
RevEmp <- subset(RevEmp, select = -c(1))

all <- RevEmp

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)

years_Rev <- subset(all, all$varcode == "WC01001")

# Change column names 
names(years_Rev)[names(years_Rev) == "X2008"] <- "2008"
names(years_Rev)[names(years_Rev) == "X2009"] <- "2009"
names(years_Rev)[names(years_Rev) == "X2010"] <- "2010"
names(years_Rev)[names(years_Rev) == "X2011"] <- "2011"
names(years_Rev)[names(years_Rev) == "X2012"] <- "2012"
names(years_Rev)[names(years_Rev) == "X2013"] <- "2013"
names(years_Rev)[names(years_Rev) == "X2014"] <- "2014"
names(years_Rev)[names(years_Rev) == "X2015"] <- "2015"
names(years_Rev)[names(years_Rev) == "X2016"] <- "2016"
names(years_Rev)[names(years_Rev) == "X2017"] <- "2017"
names(years_Rev)[names(years_Rev) == "X2018"] <- "2018"
names(years_Rev)[names(years_Rev) == "X2019"] <- "2019"
names(years_Rev)[names(years_Rev) == "X2020"] <- "2020"

# wide to long
years_Rev <- years_Rev[,!(names(years_Rev) %in% c("varcode","Code"))]

panel_Rev <- melt(setDT(years_Rev), measure.vars = as.character(2008:2020), variable.name = "year")

panel_Rev$value <- as.numeric(as.character(panel_Rev$value))
names(panel_Rev)[names(panel_Rev) == "value"] <- "Rev"

panel_all <- merge(panel_all, panel_Rev, by = c("ISIN", "year"), all.x = TRUE)

# Emp
years_emp <- subset(all, all$varcode == "WC07011")
years_emp <- subset(years_emp, select = -c(CURRENCY))
names(years_emp)[names(years_emp) == "X2008"] <- "2008"
names(years_emp)[names(years_emp) == "X2009"] <- "2009"
names(years_emp)[names(years_emp) == "X2010"] <- "2010"
names(years_emp)[names(years_emp) == "X2011"] <- "2011"
names(years_emp)[names(years_emp) == "X2012"] <- "2012"
names(years_emp)[names(years_emp) == "X2013"] <- "2013"
names(years_emp)[names(years_emp) == "X2014"] <- "2014"
names(years_emp)[names(years_emp) == "X2015"] <- "2015"
names(years_emp)[names(years_emp) == "X2016"] <- "2016"
names(years_emp)[names(years_emp) == "X2017"] <- "2017"
names(years_emp)[names(years_emp) == "X2018"] <- "2018"
names(years_emp)[names(years_emp) == "X2019"] <- "2019"
names(years_emp)[names(years_emp) == "X2020"] <- "2020"

# wide to long
years_emp <- years_emp[,!(names(years_emp) %in% c("varcode","Code"))]

panel_emp <- melt(setDT(years_emp), measure.vars = as.character(2008:2020), variable.name = "year")

panel_emp$value <- as.numeric(as.character(panel_emp$value))
names(panel_emp)[names(panel_emp) == "value"] <- "Emp"

panel_all <- merge(x = panel_all, y = panel_emp, by = c("ISIN", "year"), all.x = TRUE)

#########################
###  Total Assets and NPPE
#########################
TANPPE <- read.csv("eikon_csvs/TANPPE.csv")
TANPPE <- subset(TANPPE, TANPPE[1]!= "#ERROR")
TANPPE <- subset(TANPPE, select = -c(CURRENCY))
TANPPE <- subset(TANPPE, select = -c(1))

all <- TANPPE

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)

years_TA <- subset(all, all$varcode == "WC02999")

# Change column names 
names(years_TA)[names(years_TA) == "X2008"] <- "2008"
names(years_TA)[names(years_TA) == "X2009"] <- "2009"
names(years_TA)[names(years_TA) == "X2010"] <- "2010"
names(years_TA)[names(years_TA) == "X2011"] <- "2011"
names(years_TA)[names(years_TA) == "X2012"] <- "2012"
names(years_TA)[names(years_TA) == "X2013"] <- "2013"
names(years_TA)[names(years_TA) == "X2014"] <- "2014"
names(years_TA)[names(years_TA) == "X2015"] <- "2015"
names(years_TA)[names(years_TA) == "X2016"] <- "2016"
names(years_TA)[names(years_TA) == "X2017"] <- "2017"
names(years_TA)[names(years_TA) == "X2018"] <- "2018"
names(years_TA)[names(years_TA) == "X2019"] <- "2019"
names(years_TA)[names(years_TA) == "X2020"] <- "2020"

# wide to long
years_TA <- years_TA[,!(names(years_TA) %in% c("varcode","Code"))]

panel_TA <- melt(setDT(years_TA), measure.vars = as.character(2008:2020), variable.name = "year")

panel_TA$value <- as.numeric(as.character(panel_TA$value))
names(panel_TA)[names(panel_TA) == "value"] <- "TA"

panel_all <- merge(panel_all, panel_TA, by = c("ISIN", "year"), all.x = TRUE)

# NPPE
years_NPPE <- subset(all, all$varcode == "WC02501")

names(years_NPPE)[names(years_NPPE) == "X2008"] <- "2008"
names(years_NPPE)[names(years_NPPE) == "X2009"] <- "2009"
names(years_NPPE)[names(years_NPPE) == "X2010"] <- "2010"
names(years_NPPE)[names(years_NPPE) == "X2011"] <- "2011"
names(years_NPPE)[names(years_NPPE) == "X2012"] <- "2012"
names(years_NPPE)[names(years_NPPE) == "X2013"] <- "2013"
names(years_NPPE)[names(years_NPPE) == "X2014"] <- "2014"
names(years_NPPE)[names(years_NPPE) == "X2015"] <- "2015"
names(years_NPPE)[names(years_NPPE) == "X2016"] <- "2016"
names(years_NPPE)[names(years_NPPE) == "X2017"] <- "2017"
names(years_NPPE)[names(years_NPPE) == "X2018"] <- "2018"
names(years_NPPE)[names(years_NPPE) == "X2019"] <- "2019"
names(years_NPPE)[names(years_NPPE) == "X2020"] <- "2020"

# wide to long
years_NPPE <- years_NPPE[,!(names(years_NPPE) %in% c("varcode","Code"))]

panel_NPPE <- melt(setDT(years_NPPE), measure.vars = as.character(2008:2020), variable.name = "year")

panel_NPPE$value <- as.numeric(as.character(panel_NPPE$value))
names(panel_NPPE)[names(panel_NPPE) == "value"] <- "NPPE"

panel_all <- merge(x = panel_all, y = panel_NPPE, by = c("ISIN", "year"), all.x = TRUE)

############################################
######### INTAN TOTAL DEBT/COMMON EQUITY
############################################
INTANDebt_comm_eq <- read.csv("eikon_csvs/IntanDebt.csv")
INTANDebt_comm_eq <- subset(INTANDebt_comm_eq, INTANDebt_comm_eq[1]!= "#ERROR")
INTANDebt_comm_eq <- subset(INTANDebt_comm_eq, select = -c(CURRENCY))
INTANDebt_comm_eq <- subset(INTANDebt_comm_eq, select = -c(1))

all <- INTANDebt_comm_eq

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)

years_INTAN <- subset(all, all$varcode == "WC02649")

# Change column names 
names(years_INTAN)[names(years_INTAN) == "X2008"] <- "2008"
names(years_INTAN)[names(years_INTAN) == "X2009"] <- "2009"
names(years_INTAN)[names(years_INTAN) == "X2010"] <- "2010"
names(years_INTAN)[names(years_INTAN) == "X2011"] <- "2011"
names(years_INTAN)[names(years_INTAN) == "X2012"] <- "2012"
names(years_INTAN)[names(years_INTAN) == "X2013"] <- "2013"
names(years_INTAN)[names(years_INTAN) == "X2014"] <- "2014"
names(years_INTAN)[names(years_INTAN) == "X2015"] <- "2015"
names(years_INTAN)[names(years_INTAN) == "X2016"] <- "2016"
names(years_INTAN)[names(years_INTAN) == "X2017"] <- "2017"
names(years_INTAN)[names(years_INTAN) == "X2018"] <- "2018"
names(years_INTAN)[names(years_INTAN) == "X2019"] <- "2019"
names(years_INTAN)[names(years_INTAN) == "X2020"] <- "2020"

# wide to long
years_INTAN <- years_INTAN[,!(names(years_INTAN) %in% c("varcode","Code"))]

panel_INTAN <- melt(setDT(years_INTAN), measure.vars = as.character(2008:2020), variable.name = "year")

panel_INTAN$value <- as.numeric(as.character(panel_INTAN$value))
names(panel_INTAN)[names(panel_INTAN) == "value"] <- "INTAN"

panel_all <- merge(panel_all, panel_INTAN, by = c("ISIN", "year"), all.x = TRUE)

# Debt_comm_eq
years_Debt_comm_eq <- subset(all, all$varcode == "WC08231")

names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2008"] <- "2008"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2009"] <- "2009"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2010"] <- "2010"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2011"] <- "2011"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2012"] <- "2012"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2013"] <- "2013"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2014"] <- "2014"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2015"] <- "2015"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2016"] <- "2016"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2017"] <- "2017"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2018"] <- "2018"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2019"] <- "2019"
names(years_Debt_comm_eq)[names(years_Debt_comm_eq) == "X2020"] <- "2020"

# wide to long
years_Debt_comm_eq <- years_Debt_comm_eq[,!(names(years_Debt_comm_eq) %in% c("varcode","Code"))]

panel_Debt_comm_eq <- melt(setDT(years_Debt_comm_eq), measure.vars = as.character(2008:2020), variable.name = "year")

panel_Debt_comm_eq$value <- as.numeric(as.character(panel_Debt_comm_eq$value))
names(panel_Debt_comm_eq)[names(panel_Debt_comm_eq) == "value"] <- "Debt_comm_eq"

panel_all <- merge(x = panel_all, y = panel_Debt_comm_eq, by = c("ISIN", "year"), all.x = TRUE)

##########################################
################# CAPEX PPEG
#########################################

CapexPPEG <- read.csv("eikon_csvs/CapexPPEG.csv")
CapexPPEG <- subset(CapexPPEG, CapexPPEG[1]!= "#ERROR")
CapexPPEG <- subset(CapexPPEG, select = -c(CURRENCY))
CapexPPEG <- subset(CapexPPEG, select = -c(1))

all <- CapexPPEG

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_Capex <- subset(all, all$varcode == "DWCX")

# Change column names 
names(years_Capex)[names(years_Capex) == "X2008"] <- "2008"
names(years_Capex)[names(years_Capex) == "X2009"] <- "2009"
names(years_Capex)[names(years_Capex) == "X2010"] <- "2010"
names(years_Capex)[names(years_Capex) == "X2011"] <- "2011"
names(years_Capex)[names(years_Capex) == "X2012"] <- "2012"
names(years_Capex)[names(years_Capex) == "X2013"] <- "2013"
names(years_Capex)[names(years_Capex) == "X2014"] <- "2014"
names(years_Capex)[names(years_Capex) == "X2015"] <- "2015"
names(years_Capex)[names(years_Capex) == "X2016"] <- "2016"
names(years_Capex)[names(years_Capex) == "X2017"] <- "2017"
names(years_Capex)[names(years_Capex) == "X2018"] <- "2018"
names(years_Capex)[names(years_Capex) == "X2019"] <- "2019"
names(years_Capex)[names(years_Capex) == "X2020"] <- "2020"

# wide to long
years_Capex <- years_Capex[,!(names(years_Capex) %in% c("varcode","Code"))]

panel_Capex <- melt(setDT(years_Capex), measure.vars = as.character(2008:2020), variable.name = "year")

panel_Capex$value <- as.numeric(as.character(panel_Capex$value))
names(panel_Capex)[names(panel_Capex) == "value"] <- "Capex"

panel_all <- merge(panel_all, panel_Capex, by = c("ISIN", "year"), all.x = TRUE)

# PPEG
years_PPEG <- subset(all, all$varcode == "WC02301")

names(years_PPEG)[names(years_PPEG) == "X2008"] <- "2008"
names(years_PPEG)[names(years_PPEG) == "X2009"] <- "2009"
names(years_PPEG)[names(years_PPEG) == "X2010"] <- "2010"
names(years_PPEG)[names(years_PPEG) == "X2011"] <- "2011"
names(years_PPEG)[names(years_PPEG) == "X2012"] <- "2012"
names(years_PPEG)[names(years_PPEG) == "X2013"] <- "2013"
names(years_PPEG)[names(years_PPEG) == "X2014"] <- "2014"
names(years_PPEG)[names(years_PPEG) == "X2015"] <- "2015"
names(years_PPEG)[names(years_PPEG) == "X2016"] <- "2016"
names(years_PPEG)[names(years_PPEG) == "X2017"] <- "2017"
names(years_PPEG)[names(years_PPEG) == "X2018"] <- "2018"
names(years_PPEG)[names(years_PPEG) == "X2019"] <- "2019"
names(years_PPEG)[names(years_PPEG) == "X2020"] <- "2020"

# wide to long
years_PPEG <- years_PPEG[,!(names(years_PPEG) %in% c("varcode","Code"))]

panel_PPEG <- melt(setDT(years_PPEG), measure.vars = as.character(2008:2020), variable.name = "year")

panel_PPEG$value <- as.numeric(as.character(panel_PPEG$value))
names(panel_PPEG)[names(panel_PPEG) == "value"] <- "PPEG"

panel_all <- merge(x = panel_all, y = panel_PPEG, by = c("ISIN", "year"), all.x = TRUE)

#######################################################
################ depreciation and other accruals increase/decrease
#######################################################
DprcAccr <- read.csv("eikon_csvs/DeprecAccr.csv")
DprcAccr <- subset(DprcAccr, DprcAccr[1]!= "#ERROR")
DprcAccr <- subset(DprcAccr, select = -c(CURRENCY))
DprcAccr <- subset(DprcAccr, select = -c(1))

all <- DprcAccr

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_Dprc <- subset(all, all$varcode == "WC04049")

# Change column names 
names(years_Dprc)[names(years_Dprc) == "X2008"] <- "2008"
names(years_Dprc)[names(years_Dprc) == "X2009"] <- "2009"
names(years_Dprc)[names(years_Dprc) == "X2010"] <- "2010"
names(years_Dprc)[names(years_Dprc) == "X2011"] <- "2011"
names(years_Dprc)[names(years_Dprc) == "X2012"] <- "2012"
names(years_Dprc)[names(years_Dprc) == "X2013"] <- "2013"
names(years_Dprc)[names(years_Dprc) == "X2014"] <- "2014"
names(years_Dprc)[names(years_Dprc) == "X2015"] <- "2015"
names(years_Dprc)[names(years_Dprc) == "X2016"] <- "2016"
names(years_Dprc)[names(years_Dprc) == "X2017"] <- "2017"
names(years_Dprc)[names(years_Dprc) == "X2018"] <- "2018"
names(years_Dprc)[names(years_Dprc) == "X2019"] <- "2019"
names(years_Dprc)[names(years_Dprc) == "X2020"] <- "2020"

# wide to long
years_Dprc <- years_Dprc[,!(names(years_Dprc) %in% c("varcode","Code"))]

panel_Dprc <- melt(setDT(years_Dprc), measure.vars = as.character(2008:2020), variable.name = "year")

panel_Dprc$value <- as.numeric(as.character(panel_Dprc$value))
names(panel_Dprc)[names(panel_Dprc) == "value"] <- "Dprc"

panel_all <- merge(panel_all, panel_Dprc, by = c("ISIN", "year"), all.x = TRUE)

# Accr
years_Accr <- subset(all, all$varcode == "WC04829")

names(years_Accr)[names(years_Accr) == "X2008"] <- "2008"
names(years_Accr)[names(years_Accr) == "X2009"] <- "2009"
names(years_Accr)[names(years_Accr) == "X2010"] <- "2010"
names(years_Accr)[names(years_Accr) == "X2011"] <- "2011"
names(years_Accr)[names(years_Accr) == "X2012"] <- "2012"
names(years_Accr)[names(years_Accr) == "X2013"] <- "2013"
names(years_Accr)[names(years_Accr) == "X2014"] <- "2014"
names(years_Accr)[names(years_Accr) == "X2015"] <- "2015"
names(years_Accr)[names(years_Accr) == "X2016"] <- "2016"
names(years_Accr)[names(years_Accr) == "X2017"] <- "2017"
names(years_Accr)[names(years_Accr) == "X2018"] <- "2018"
names(years_Accr)[names(years_Accr) == "X2019"] <- "2019"
names(years_Accr)[names(years_Accr) == "X2020"] <- "2020"

# wide to long
years_Accr <- years_Accr[,!(names(years_Accr) %in% c("varcode","Code"))]

panel_Accr <- melt(setDT(years_Accr), measure.vars = as.character(2008:2020), variable.name = "year")

panel_Accr$value <- as.numeric(as.character(panel_Accr$value))
names(panel_Accr)[names(panel_Accr) == "value"] <- "Accr"

panel_all <- merge(x = panel_all, y = panel_Accr, by = c("ISIN", "year"), all.x = TRUE)

############################################################
############################### ESG scores
############################################################

S_pillarE_pillar <- read.csv("eikon_csvs/ESG.csv")
S_pillarE_pillar <- subset(S_pillarE_pillar, S_pillarE_pillar[1]!= "#ERROR")
S_pillarE_pillar <- subset(S_pillarE_pillar, select = -c(CURRENCY))
S_pillarE_pillar <- subset(S_pillarE_pillar, select = -c(1))

all <- S_pillarE_pillar

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_S_pillar <- subset(all, all$varcode == "SOSCORE")

# Change column names 
names(years_S_pillar)[names(years_S_pillar) == "X2008"] <- "2008"
names(years_S_pillar)[names(years_S_pillar) == "X2009"] <- "2009"
names(years_S_pillar)[names(years_S_pillar) == "X2010"] <- "2010"
names(years_S_pillar)[names(years_S_pillar) == "X2011"] <- "2011"
names(years_S_pillar)[names(years_S_pillar) == "X2012"] <- "2012"
names(years_S_pillar)[names(years_S_pillar) == "X2013"] <- "2013"
names(years_S_pillar)[names(years_S_pillar) == "X2014"] <- "2014"
names(years_S_pillar)[names(years_S_pillar) == "X2015"] <- "2015"
names(years_S_pillar)[names(years_S_pillar) == "X2016"] <- "2016"
names(years_S_pillar)[names(years_S_pillar) == "X2017"] <- "2017"
names(years_S_pillar)[names(years_S_pillar) == "X2018"] <- "2018"
names(years_S_pillar)[names(years_S_pillar) == "X2019"] <- "2019"
names(years_S_pillar)[names(years_S_pillar) == "X2020"] <- "2020"

# wide to long
years_S_pillar <- years_S_pillar[,!(names(years_S_pillar) %in% c("varcode","Code"))]

panel_S_pillar <- melt(setDT(years_S_pillar), measure.vars = as.character(2008:2020), variable.name = "year")

panel_S_pillar$value <- as.numeric(as.character(panel_S_pillar$value))
names(panel_S_pillar)[names(panel_S_pillar) == "value"] <- "S_pillar"

panel_all <- merge(panel_all, panel_S_pillar, by = c("ISIN", "year"), all.x = TRUE)

# E_pillar
years_E_pillar <- subset(all, all$varcode == "ENSCORE")

names(years_E_pillar)[names(years_E_pillar) == "X2008"] <- "2008"
names(years_E_pillar)[names(years_E_pillar) == "X2009"] <- "2009"
names(years_E_pillar)[names(years_E_pillar) == "X2010"] <- "2010"
names(years_E_pillar)[names(years_E_pillar) == "X2011"] <- "2011"
names(years_E_pillar)[names(years_E_pillar) == "X2012"] <- "2012"
names(years_E_pillar)[names(years_E_pillar) == "X2013"] <- "2013"
names(years_E_pillar)[names(years_E_pillar) == "X2014"] <- "2014"
names(years_E_pillar)[names(years_E_pillar) == "X2015"] <- "2015"
names(years_E_pillar)[names(years_E_pillar) == "X2016"] <- "2016"
names(years_E_pillar)[names(years_E_pillar) == "X2017"] <- "2017"
names(years_E_pillar)[names(years_E_pillar) == "X2018"] <- "2018"
names(years_E_pillar)[names(years_E_pillar) == "X2019"] <- "2019"
names(years_E_pillar)[names(years_E_pillar) == "X2020"] <- "2020"

# wide to long
years_E_pillar <- years_E_pillar[,!(names(years_E_pillar) %in% c("varcode","Code"))]

panel_E_pillar <- melt(setDT(years_E_pillar), measure.vars = as.character(2008:2020), variable.name = "year")

panel_E_pillar$value <- as.numeric(as.character(panel_E_pillar$value))
names(panel_E_pillar)[names(panel_E_pillar) == "value"] <- "E_pillar"

panel_all <- merge(x = panel_all, y = panel_E_pillar, by = c("ISIN", "year"), all.x = TRUE)

# G_pillar
years_G_pillar <- subset(all, all$varcode == "CGSCORE")

names(years_G_pillar)[names(years_G_pillar) == "X2008"] <- "2008"
names(years_G_pillar)[names(years_G_pillar) == "X2009"] <- "2009"
names(years_G_pillar)[names(years_G_pillar) == "X2010"] <- "2010"
names(years_G_pillar)[names(years_G_pillar) == "X2011"] <- "2011"
names(years_G_pillar)[names(years_G_pillar) == "X2012"] <- "2012"
names(years_G_pillar)[names(years_G_pillar) == "X2013"] <- "2013"
names(years_G_pillar)[names(years_G_pillar) == "X2014"] <- "2014"
names(years_G_pillar)[names(years_G_pillar) == "X2015"] <- "2015"
names(years_G_pillar)[names(years_G_pillar) == "X2016"] <- "2016"
names(years_G_pillar)[names(years_G_pillar) == "X2017"] <- "2017"
names(years_G_pillar)[names(years_G_pillar) == "X2018"] <- "2018"
names(years_G_pillar)[names(years_G_pillar) == "X2019"] <- "2019"
names(years_G_pillar)[names(years_G_pillar) == "X2020"] <- "2020"

# wide to long
years_G_pillar <- years_G_pillar[,!(names(years_G_pillar) %in% c("varcode","Code"))]

panel_G_pillar <- melt(setDT(years_G_pillar), measure.vars = as.character(2008:2020), variable.name = "year")

panel_G_pillar$value <- as.numeric(as.character(panel_G_pillar$value))
names(panel_G_pillar)[names(panel_G_pillar) == "value"] <- "G_pillar"

panel_all <- merge(x = panel_all, y = panel_G_pillar, by = c("ISIN", "year"), all.x = TRUE)

#####################################################
################### Net Cashflow Operating Activities
#####################################################
NCFOAAccr <- read.csv("eikon_csvs/NetCFOpAct_Accr.csv")
NCFOAAccr <- subset(NCFOAAccr, NCFOAAccr[1]!= "#ERROR")
NCFOAAccr <- subset(NCFOAAccr, select = -c(CURRENCY))
NCFOAAccr <- subset(NCFOAAccr, select = -c(1))

all <- NCFOAAccr

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_NCFOA <- subset(all, all$varcode == "WC04860")

# Change column names 
names(years_NCFOA)[names(years_NCFOA) == "X2008"] <- "2008"
names(years_NCFOA)[names(years_NCFOA) == "X2009"] <- "2009"
names(years_NCFOA)[names(years_NCFOA) == "X2010"] <- "2010"
names(years_NCFOA)[names(years_NCFOA) == "X2011"] <- "2011"
names(years_NCFOA)[names(years_NCFOA) == "X2012"] <- "2012"
names(years_NCFOA)[names(years_NCFOA) == "X2013"] <- "2013"
names(years_NCFOA)[names(years_NCFOA) == "X2014"] <- "2014"
names(years_NCFOA)[names(years_NCFOA) == "X2015"] <- "2015"
names(years_NCFOA)[names(years_NCFOA) == "X2016"] <- "2016"
names(years_NCFOA)[names(years_NCFOA) == "X2017"] <- "2017"
names(years_NCFOA)[names(years_NCFOA) == "X2018"] <- "2018"
names(years_NCFOA)[names(years_NCFOA) == "X2019"] <- "2019"
names(years_NCFOA)[names(years_NCFOA) == "X2020"] <- "2020"

# wide to long
years_NCFOA <- years_NCFOA[,!(names(years_NCFOA) %in% c("varcode","Code"))]

panel_NCFOA <- melt(setDT(years_NCFOA), measure.vars = as.character(2008:2020), variable.name = "year")

panel_NCFOA$value <- as.numeric(as.character(panel_NCFOA$value))
names(panel_NCFOA)[names(panel_NCFOA) == "value"] <- "NCFOA"

panel_all <- merge(panel_all, panel_NCFOA, by = c("ISIN", "year"), all.x = TRUE)

###########################################################################
#################################### Debt/Total Capital
###########################################################################

Debt_TotCap <- read.csv("eikon_csvs/Debt%TotCap.csv")
Debt_TotCap <- subset(Debt_TotCap, Debt_TotCap[1]!= "#ERROR")
Debt_TotCap <- subset(Debt_TotCap, select = -c(CURRENCY))
Debt_TotCap <- subset(Debt_TotCap, select = -c(1))

all <- Debt_TotCap

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_Debt_TotCap <- subset(all, all$varcode == "WC08221")

# Change column names 
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2008"] <- "2008"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2009"] <- "2009"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2010"] <- "2010"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2011"] <- "2011"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2012"] <- "2012"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2013"] <- "2013"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2014"] <- "2014"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2015"] <- "2015"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2016"] <- "2016"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2017"] <- "2017"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2018"] <- "2018"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2019"] <- "2019"
names(years_Debt_TotCap)[names(years_Debt_TotCap) == "X2020"] <- "2020"

# wide to long
years_Debt_TotCap <- years_Debt_TotCap[,!(names(years_Debt_TotCap) %in% c("varcode","Code"))]

panel_Debt_TotCap <- melt(setDT(years_Debt_TotCap), measure.vars = as.character(2008:2020), variable.name = "year")

panel_Debt_TotCap$value <- as.numeric(as.character(panel_Debt_TotCap$value))
names(panel_Debt_TotCap)[names(panel_Debt_TotCap) == "value"] <- "Debt_TotCap"

panel_all <- merge(panel_all, panel_Debt_TotCap, by = c("ISIN", "year"), all.x = TRUE)

##############################################################################
####################################### Gross Margin
##############################################################################

GMAR <- read.csv("eikon_csvs/GMAR.csv")
GMAR <- subset(GMAR, GMAR[1]!= "#ERROR")
GMAR <- subset(GMAR, select = -c(CURRENCY))
GMAR <- subset(GMAR, select = -c(1))

all <- GMAR

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_GMAR <- subset(all, all$varcode == "WC08306")

# Change column names 
names(years_GMAR)[names(years_GMAR) == "X2008"] <- "2008"
names(years_GMAR)[names(years_GMAR) == "X2009"] <- "2009"
names(years_GMAR)[names(years_GMAR) == "X2010"] <- "2010"
names(years_GMAR)[names(years_GMAR) == "X2011"] <- "2011"
names(years_GMAR)[names(years_GMAR) == "X2012"] <- "2012"
names(years_GMAR)[names(years_GMAR) == "X2013"] <- "2013"
names(years_GMAR)[names(years_GMAR) == "X2014"] <- "2014"
names(years_GMAR)[names(years_GMAR) == "X2015"] <- "2015"
names(years_GMAR)[names(years_GMAR) == "X2016"] <- "2016"
names(years_GMAR)[names(years_GMAR) == "X2017"] <- "2017"
names(years_GMAR)[names(years_GMAR) == "X2018"] <- "2018"
names(years_GMAR)[names(years_GMAR) == "X2019"] <- "2019"
names(years_GMAR)[names(years_GMAR) == "X2020"] <- "2020"

# wide to long
years_GMAR <- years_GMAR[,!(names(years_GMAR) %in% c("varcode","Code"))]

panel_GMAR <- melt(setDT(years_GMAR), measure.vars = as.character(2008:2020), variable.name = "year")

panel_GMAR$value <- as.numeric(as.character(panel_GMAR$value))
names(panel_GMAR)[names(panel_GMAR) == "value"] <- "GMAR"

panel_all <- merge(panel_all, panel_GMAR, by = c("ISIN", "year"), all.x = TRUE)

##############################################
######### Operating Income Before Depreciation
##############################################

OIBD <- read.csv("eikon_csvs/Operating Income Before Depreciation & Amortization.csv")
OIBD <- subset(OIBD, OIBD[1]!= "#ERROR")
OIBD <- subset(OIBD, select = -c(CURRENCY))
OIBD <- subset(OIBD, select = -c(1))

all <- OIBD

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_OIBD <- subset(all, all$varcode == "WC18155")

# Change column names 
names(years_OIBD)[names(years_OIBD) == "X2008"] <- "2008"
names(years_OIBD)[names(years_OIBD) == "X2009"] <- "2009"
names(years_OIBD)[names(years_OIBD) == "X2010"] <- "2010"
names(years_OIBD)[names(years_OIBD) == "X2011"] <- "2011"
names(years_OIBD)[names(years_OIBD) == "X2012"] <- "2012"
names(years_OIBD)[names(years_OIBD) == "X2013"] <- "2013"
names(years_OIBD)[names(years_OIBD) == "X2014"] <- "2014"
names(years_OIBD)[names(years_OIBD) == "X2015"] <- "2015"
names(years_OIBD)[names(years_OIBD) == "X2016"] <- "2016"
names(years_OIBD)[names(years_OIBD) == "X2017"] <- "2017"
names(years_OIBD)[names(years_OIBD) == "X2018"] <- "2018"
names(years_OIBD)[names(years_OIBD) == "X2019"] <- "2019"
names(years_OIBD)[names(years_OIBD) == "X2020"] <- "2020"

# wide to long
years_OIBD <- years_OIBD[,!(names(years_OIBD) %in% c("varcode","Code"))]

panel_OIBD <- melt(setDT(years_OIBD), measure.vars = as.character(2008:2020), variable.name = "year")

panel_OIBD$value <- as.numeric(as.character(panel_OIBD$value))
names(panel_OIBD)[names(panel_OIBD) == "value"] <- "OIBD"

panel_all <- merge(panel_all, panel_OIBD, by = c("ISIN", "year"), all.x = TRUE)

############################################################################
############################# Operating income and Net sales/Working Capital
############################################################################

OINS_WC <- read.csv("eikon_csvs/OpIncome_NS%WC.csv")
OINS_WC <- subset(OINS_WC, OINS_WC[1]!= "#ERROR")
OINS_WC <- subset(OINS_WC, select = -c(CURRENCY))
OINS_WC <- subset(OINS_WC, select = -c(1))

all <- OINS_WC

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)
years_OI <- subset(all, all$varcode == "WC01250")

# Change column names 
names(years_OI)[names(years_OI) == "X2008"] <- "2008"
names(years_OI)[names(years_OI) == "X2009"] <- "2009"
names(years_OI)[names(years_OI) == "X2010"] <- "2010"
names(years_OI)[names(years_OI) == "X2011"] <- "2011"
names(years_OI)[names(years_OI) == "X2012"] <- "2012"
names(years_OI)[names(years_OI) == "X2013"] <- "2013"
names(years_OI)[names(years_OI) == "X2014"] <- "2014"
names(years_OI)[names(years_OI) == "X2015"] <- "2015"
names(years_OI)[names(years_OI) == "X2016"] <- "2016"
names(years_OI)[names(years_OI) == "X2017"] <- "2017"
names(years_OI)[names(years_OI) == "X2018"] <- "2018"
names(years_OI)[names(years_OI) == "X2019"] <- "2019"
names(years_OI)[names(years_OI) == "X2020"] <- "2020"

# wide to long
years_OI <- years_OI[,!(names(years_OI) %in% c("varcode","Code"))]

panel_OI <- melt(setDT(years_OI), measure.vars = as.character(2008:2020), variable.name = "year")

panel_OI$value <- as.numeric(as.character(panel_OI$value))
names(panel_OI)[names(panel_OI) == "value"] <- "OI"

panel_all <- merge(panel_all, panel_OI, by = c("ISIN", "year"), all.x = TRUE)

# NS_WC
years_NS_WC <- subset(all, all$varcode == "WC08141")

names(years_NS_WC)[names(years_NS_WC) == "X2008"] <- "2008"
names(years_NS_WC)[names(years_NS_WC) == "X2009"] <- "2009"
names(years_NS_WC)[names(years_NS_WC) == "X2010"] <- "2010"
names(years_NS_WC)[names(years_NS_WC) == "X2011"] <- "2011"
names(years_NS_WC)[names(years_NS_WC) == "X2012"] <- "2012"
names(years_NS_WC)[names(years_NS_WC) == "X2013"] <- "2013"
names(years_NS_WC)[names(years_NS_WC) == "X2014"] <- "2014"
names(years_NS_WC)[names(years_NS_WC) == "X2015"] <- "2015"
names(years_NS_WC)[names(years_NS_WC) == "X2016"] <- "2016"
names(years_NS_WC)[names(years_NS_WC) == "X2017"] <- "2017"
names(years_NS_WC)[names(years_NS_WC) == "X2018"] <- "2018"
names(years_NS_WC)[names(years_NS_WC) == "X2019"] <- "2019"
names(years_NS_WC)[names(years_NS_WC) == "X2020"] <- "2020"

# wide to long
years_NS_WC <- years_NS_WC[,!(names(years_NS_WC) %in% c("varcode","Code"))]

panel_NS_WC <- melt(setDT(years_NS_WC), measure.vars = as.character(2008:2020), variable.name = "year")

panel_NS_WC$value <- as.numeric(as.character(panel_NS_WC$value))
names(panel_NS_WC)[names(panel_NS_WC) == "value"] <- "NS_WC"

panel_all <- merge(x = panel_all, y = panel_NS_WC, by = c("ISIN", "year"), all.x = TRUE)

################################################################
###################  
################################################################
write.csv(panel_all, "df_total.csv")
df_total <- read.csv("df_total.csv")

# match isin with country
df_total <- merge(x = df_total, y = isin_match, by = "ISIN", all = TRUE)

# Energy data 
panel_energy$Country <- toupper(panel_energy$Country)
df_total <- merge(x = df_total, y = panel_energy, by = c("Country", "year"), all.x = TRUE)

# Income group
incomegroup$Country <- toupper(incomegroup$Country)
df_total <- merge(x = df_total, y = incomegroup, by = "Country", all.x = TRUE)

# CO2 law
CO2law$Country <- toupper(CO2law$Country)
df_total <- merge(x = df_total, y = CO2law, by = "Country", all.x = TRUE)

# GSIC sectors industries etc.
names(df_WRDS)[names(df_WRDS) == "isin"] <- "ISIN"

df_G <- subset(df_WRDS, select = c("ISIN", "ggroup", "gind", "gsector", "gsubind", "naics", "naicsh"))

final <- df_total %>% 
  left_join(df_G, by = "ISIN")

final <- subset(final, select = -c(X, Accr))
final$FuelInten <- as.numeric(as.character(final$FuelInten))

# No ISIN year duplications
final$concat <- paste(final$ISIN, final$year)
sum(duplicated(final$concat))

final<-subset(final, duplicated(final$concat) == FALSE)
final <- subset(final, select = -concat)

# Final CSV
write.csv(final, "final.csv")

#final <- read.csv("final.csv")
rownames(final) <- paste(final$ISIN, final$year, sep = " - ")

str(final)

###############################################################################
###############################################################################
###############################################################################
# 05-05-2022

# Additional data ROA regression
add <- read.csv("Additional_data.csv")

# seperate: WC03051,WC08106,WC01051
all <- add

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)

years_stlt_lia <- subset(all, all$varcode == "WC03051")

# Change column names 
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2008"] <- "2008"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2009"] <- "2009"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2010"] <- "2010"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2011"] <- "2011"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2012"] <- "2012"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2013"] <- "2013"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2014"] <- "2014"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2015"] <- "2015"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2016"] <- "2016"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2017"] <- "2017"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2018"] <- "2018"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2019"] <- "2019"
names(years_stlt_lia)[names(years_stlt_lia) == "X04.05.2020"] <- "2020"

# wide to long
years_stlt_lia <- years_stlt_lia[,!(names(years_stlt_lia) %in% c("varcode","Code"))]

panel_stlt_lia <- melt(setDT(years_stlt_lia), measure.vars = as.character(2008:2020), variable.name = "year")

panel_stlt_lia$value <- as.numeric(as.character(panel_stlt_lia$value))
names(panel_stlt_lia)[names(panel_stlt_lia) == "value"] <- "stlt_lia"

# Current ratio

years_cur_rat <- subset(all, all$varcode == "WC08106")

# Change column names 
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2008"] <- "2008"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2009"] <- "2009"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2010"] <- "2010"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2011"] <- "2011"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2012"] <- "2012"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2013"] <- "2013"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2014"] <- "2014"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2015"] <- "2015"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2016"] <- "2016"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2017"] <- "2017"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2018"] <- "2018"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2019"] <- "2019"
names(years_cur_rat)[names(years_cur_rat) == "X04.05.2020"] <- "2020"

# wide to long
years_cur_rat <- years_cur_rat[,!(names(years_cur_rat) %in% c("varcode","Code"))]

panel_cur_rat <- melt(setDT(years_cur_rat), measure.vars = as.character(2008:2020), variable.name = "year")

panel_cur_rat$value <- as.numeric(as.character(panel_cur_rat$value))
names(panel_cur_rat)[names(panel_cur_rat) == "value"] <- "cur_rat"

panel_all <- merge(panel_cur_rat, panel_stlt_lia, by = c("ISIN", "year"), all.x = TRUE)

# Cost Of Goods Sold (Excl Depreciation) 

years_cogs <- subset(all, all$varcode == "WC01051")

# Change column names 
names(years_cogs)[names(years_cogs) == "X04.05.2008"] <- "2008"
names(years_cogs)[names(years_cogs) == "X04.05.2009"] <- "2009"
names(years_cogs)[names(years_cogs) == "X04.05.2010"] <- "2010"
names(years_cogs)[names(years_cogs) == "X04.05.2011"] <- "2011"
names(years_cogs)[names(years_cogs) == "X04.05.2012"] <- "2012"
names(years_cogs)[names(years_cogs) == "X04.05.2013"] <- "2013"
names(years_cogs)[names(years_cogs) == "X04.05.2014"] <- "2014"
names(years_cogs)[names(years_cogs) == "X04.05.2015"] <- "2015"
names(years_cogs)[names(years_cogs) == "X04.05.2016"] <- "2016"
names(years_cogs)[names(years_cogs) == "X04.05.2017"] <- "2017"
names(years_cogs)[names(years_cogs) == "X04.05.2018"] <- "2018"
names(years_cogs)[names(years_cogs) == "X04.05.2019"] <- "2019"
names(years_cogs)[names(years_cogs) == "X04.05.2020"] <- "2020"

# wide to long
years_cogs <- years_cogs[,!(names(years_cogs) %in% c("varcode","Code"))]

panel_cogs <- melt(setDT(years_cogs), measure.vars = as.character(2008:2020), variable.name = "year")

panel_cogs$value <- as.numeric(as.character(panel_cogs$value))
names(panel_cogs)[names(panel_cogs) == "value"] <- "cogs"

panel_all <- merge(panel_all, panel_cogs, by = c("ISIN", "year"), all.x = TRUE)


panel_all <- subset(panel_all, select = -c(3,5,7))

write.csv(panel_all, 'ctrl_roa.csv', row.names = FALSE)


###############################################################################
###############################################################################
###############################################################################
# ROA

# roaitional data ROA regression
roa <- read.csv("ROA.csv")

all <- roa

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)

 
years_roa <- subset(all, all$varcode == "WC08326")

# Change column names 
names(years_roa)[names(years_roa) == "X2008"] <- "2008"
names(years_roa)[names(years_roa) == "X2009"] <- "2009"
names(years_roa)[names(years_roa) == "X2010"] <- "2010"
names(years_roa)[names(years_roa) == "X2011"] <- "2011"
names(years_roa)[names(years_roa) == "X2012"] <- "2012"
names(years_roa)[names(years_roa) == "X2013"] <- "2013"
names(years_roa)[names(years_roa) == "X2014"] <- "2014"
names(years_roa)[names(years_roa) == "X2015"] <- "2015"
names(years_roa)[names(years_roa) == "X2016"] <- "2016"
names(years_roa)[names(years_roa) == "X2017"] <- "2017"
names(years_roa)[names(years_roa) == "X2018"] <- "2018"
names(years_roa)[names(years_roa) == "X2019"] <- "2019"
names(years_roa)[names(years_roa) == "X2020"] <- "2020"

# wide to long
years_roa <- years_roa[,!(names(years_roa) %in% c("varcode","Code"))]

panel_roa <- melt(setDT(years_roa), measure.vars = as.character(2008:2020), variable.name = "year")

panel_roa$value <- as.numeric(as.character(panel_roa$value))
names(panel_roa)[names(panel_roa) == "value"] <- "roa"

panel_roa <- subset(panel_roa, select = c(ISIN, year, roa))
write.csv(panel_roa, 'panel_roa.csv', row.names = FALSE)
###############################################################################
###############################################################################
###############################################################################
rm(list = ls())
cogs <- read.csv("eikon_csvs/WC01051.csv")
all <- cogs

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)

years_cogs <- subset(all, all$varcode == "WC01051")
# Change column names 
names(years_cogs)[names(years_cogs) == "X25.05.2008"] <- "2008"
names(years_cogs)[names(years_cogs) == "X25.05.2009"] <- "2009"
names(years_cogs)[names(years_cogs) == "X25.05.2010"] <- "2010"
names(years_cogs)[names(years_cogs) == "X25.05.2011"] <- "2011"
names(years_cogs)[names(years_cogs) == "X25.05.2012"] <- "2012"
names(years_cogs)[names(years_cogs) == "X25.05.2013"] <- "2013"
names(years_cogs)[names(years_cogs) == "X25.05.2014"] <- "2014"
names(years_cogs)[names(years_cogs) == "X25.05.2015"] <- "2015"
names(years_cogs)[names(years_cogs) == "X25.05.2016"] <- "2016"
names(years_cogs)[names(years_cogs) == "X25.05.2017"] <- "2017"
names(years_cogs)[names(years_cogs) == "X25.05.2018"] <- "2018"
names(years_cogs)[names(years_cogs) == "X25.05.2019"] <- "2019"
names(years_cogs)[names(years_cogs) == "X25.05.2020"] <- "2020"

# wide to long
years_cogs <- years_cogs[,!(names(years_cogs) %in% c("varcode","Code"))]

panel_cogs <- melt(setDT(years_cogs), measure.vars = as.character(2008:2020), variable.name = "year")

panel_cogs$value <- as.numeric(as.character(panel_cogs$value))
names(panel_cogs)[names(panel_cogs) == "value"] <- "cogs"

panel_cogs <- subset(panel_cogs, select = c(ISIN, year, cogs))
panel_all <- panel_cogs

curr_rat <- read.csv("eikon_csvs/WC08106.csv")
all <- curr_rat

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)

years_curr_rat <- subset(all, all$varcode == "WC08106")
# Change column names 
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2008"] <- "2008"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2009"] <- "2009"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2010"] <- "2010"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2011"] <- "2011"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2012"] <- "2012"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2013"] <- "2013"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2014"] <- "2014"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2015"] <- "2015"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2016"] <- "2016"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2017"] <- "2017"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2018"] <- "2018"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2019"] <- "2019"
names(years_curr_rat)[names(years_curr_rat) == "X25.05.2020"] <- "2020"

# wide to long
years_curr_rat <- years_curr_rat[,!(names(years_curr_rat) %in% c("varcode","Code"))]

panel_curr_rat <- melt(setDT(years_curr_rat), measure.vars = as.character(2008:2020), variable.name = "year")

panel_curr_rat$value <- as.numeric(as.character(panel_curr_rat$value))
names(panel_curr_rat)[names(panel_curr_rat) == "value"] <- "curr_rat"

panel_curr_rat <- subset(panel_curr_rat, select = c(ISIN, year, curr_rat))

panel_all <- merge(panel_all, panel_curr_rat, by = c("ISIN", "year"), all.x = TRUE)
###############################################################################

stlt_lia <- read.csv("eikon_csvs/Longterm_shortterm_debt.csv")
all <- stlt_lia

all$ISIN <- substr(all$Code, start = 1, stop = 12)
all$varcode <- substr(all$Code, start = 14, stop = 20)
all$varcode <- gsub(")","",all$varcode)

years_stlt_lia <- all

# Change column names 
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2008"] <- "2008"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2009"] <- "2009"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2010"] <- "2010"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2011"] <- "2011"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2012"] <- "2012"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2013"] <- "2013"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2014"] <- "2014"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2015"] <- "2015"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2016"] <- "2016"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2017"] <- "2017"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2018"] <- "2018"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2019"] <- "2019"
names(years_stlt_lia)[names(years_stlt_lia) == "X25.05.2020"] <- "2020"

# wide to long
years_stlt_lia <- years_stlt_lia[,!(names(years_stlt_lia) %in% c("varcode","Code"))]

panel_stlt_lia <- melt(setDT(years_stlt_lia), measure.vars = as.character(2008:2020), variable.name = "year")

panel_stlt_lia$value <- as.numeric(as.character(panel_stlt_lia$value))
names(panel_stlt_lia)[names(panel_stlt_lia) == "value"] <- "stlt_lia"

panel_stlt_lia <- subset(panel_stlt_lia, select = c(ISIN, year, stlt_lia))
panel_all <- merge(panel_all, panel_stlt_lia, by = c("ISIN", "year"), all.x = TRUE)
write.csv(panel_all, 'All_ctrl_roa.csv', row.names = FALSE)

###############################################################################
###############################################################################
env_controversies <- read.csv('eikon_csvs/env_controversies.csv')
env_controversies1 <-env_controversies[(env_controversies$X2008 != '$$ER: 9903,NO DATA AVAILABLE' & 
                                          env_controversies$X2008 !='$$ER: 0904,NO DATA AVAILABLE' &
                                          env_controversies$X2008 !='N   ' &
                                          env_controversies$X2008 != '$$ER: E100,INVALID CODE OR EXPRESSION ENTERED'),]
env_controversies <- env_controversies1[(complete.cases(env_controversies1)),]

all <- env_controversies

all$ISIN <- substr(all$ï..Code, start = 1, stop = 12)
all$varï..Code <- substr(all$ï..Code, start = 14, stop = 20)
all$varï..Code <- gsub(")","",all$varï..Code)

years_env_controversies <- all

# Change column names 
names(years_env_controversies)[names(years_env_controversies) == "X2008"] <- "2008"
names(years_env_controversies)[names(years_env_controversies) == "X2009"] <- "2009"
names(years_env_controversies)[names(years_env_controversies) == "X2010"] <- "2010"
names(years_env_controversies)[names(years_env_controversies) == "X2011"] <- "2011"
names(years_env_controversies)[names(years_env_controversies) == "X2012"] <- "2012"
names(years_env_controversies)[names(years_env_controversies) == "X2013"] <- "2013"
names(years_env_controversies)[names(years_env_controversies) == "X2014"] <- "2014"
names(years_env_controversies)[names(years_env_controversies) == "X2015"] <- "2015"
names(years_env_controversies)[names(years_env_controversies) == "X2016"] <- "2016"
names(years_env_controversies)[names(years_env_controversies) == "X2017"] <- "2017"
names(years_env_controversies)[names(years_env_controversies) == "X2018"] <- "2018"
names(years_env_controversies)[names(years_env_controversies) == "X2019"] <- "2019"
names(years_env_controversies)[names(years_env_controversies) == "X2020"] <- "2020"

# wide to long
years_env_controversies <- years_env_controversies[,!(names(years_env_controversies) %in% c("varï..Code","ï..Code"))]

panel_env_controversies <- melt(setDT(years_env_controversies), measure.vars = as.character(2008:2020), variable.name = "year")

panel_env_controversies$value <- as.numeric(as.character(panel_env_controversies$value))
names(panel_env_controversies)[names(panel_env_controversies) == "value"] <- "env_controversies"

panel_env_controversies <- subset(panel_env_controversies, select = c(ISIN, year, env_controversies))

write.csv(panel_env_controversies, 'panel_env_controversies.csv', row.names = FALSE)

