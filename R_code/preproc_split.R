rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(corrplot)
#library(tidyr)
library(rsample)



wd <- setwd("C:/Users/kylia/OneDrive/Msc BAM/Thesis/Data/")

df1 <- read.csv("final.csv")
df1 <- df1 %>% drop_na(scope_total)
df1 <- df1 %>% drop_na(year)

# create new variables before split
df1$Age <- df1$PPEG/df1$Dprc
df1$CapInt <- df1$PPEG/df1$Rev

df1$INTAN_TA <- df1$INTAN/df1$TA
df1$Capex_NPPE <- df1$Capex/df1$NPPE
df1$Rev_Emp <- df1$Rev/df1$Emp


unique(df1$CO2_imp)
# uninformative variable so can be deleted

unique(df$Income.group)
count(subset(df, Income.group == "Upper middle income"))
# too few observation of Non-high income

str(df1)
df <- subset(df1, select = c(ISIN, year, scope_total, #Rev, Emp, TA, 
                             NPPE, 
                             #INTAN, 
                             Debt_TotCap, GMAR, gsector, #Capex, 
                             INTAN_TA, Capex_NPPE, Rev_Emp, FuelInten, 
                             Age, CapInt))

stargazer(df, type = 'text')

df$FuelInten <- as.numeric(as.character(df$FuelInten))

df$gsector <- as.factor(df$gsector)

# No ISIN year duplications
df$concat <- paste(df$ISIN, df$year)
sum(duplicated(df$concat))

df<-subset(df, duplicated(df$concat) == FALSE)
df <- subset(df, select = -concat)
df<-na.omit(df)


##############
IS <- df %>% group_by(ISIN)
ISIN_split <- group_split(IS)

ISIN_keys <- group_keys(IS)

sum(duplicated(ISIN_keys$ISIN))
#sum(df_train$ISIN %in% df_test$ISIN)

# make the split
set.seed(912340)
df_split <- initial_split(data = ISIN_keys, prop = 0.65)
df_train <- training(df_split)
df_test <- testing(df_split)
# make train test
train <- df[df$ISIN %in% df_train$ISIN, ]
test <- df[df$ISIN %in% df_test$ISIN, ]
# checking
sum(train$ISIN %in% test$ISIN)
# it worked

# now winsorize 
# Winsorization at 1% and 99% quantile
# Define how many entries should be winsorized
k = round(0.01*nrow(train))

stargazer(train, summary = TRUE, type = "text")
# scope_total: log transform
# NPPE: logtransform
# debt_totcap: winsorize
# gmar: investigate distribution
# gesector: fine
# INTAN_TA: fine
# capex_NPPE: investigate, maybe winsorize
# Rev_emp: investigate distribution, maybe winsorize
# fuelinten: fine
# age: investigat dist, maybe winsorize
# capint: invest dist, maybe winsorize

# ALL TRANSFORMATIONS MADE ON THE TRAIN SET ARE ALSO MADE ON THE TEST SET
# THE TRANSFORMATION DECISIONS ARE PURELY BASED ON THE TRAINSET

# investigate distributions as mentioned above
# scope total
ggplot(train, aes(scope_total)) +
  geom_histogram()
ggsave("dist_scope_total.png")
# Debt_TotCap
ggplot(train, aes(Debt_TotCap)) +
  geom_histogram()
ggsave("dist_Debt_TotCap.png")

a <- quantile(train$GMAR, c(0.01), names = FALSE)
b <- quantile(train$GMAR, c(0.99), names = FALSE)
train$Debt_TotCap[which(train$Debt_TotCap<a)]<-a
train$Debt_TotCap[which(train$Debt_TotCap>b)]<-b
test$Debt_TotCap[which(test$Debt_TotCap<a)]<-a
test$Debt_TotCap[which(test$Debt_TotCap>b)]<-b

# GMAR
ggplot(train, aes(GMAR)) +
  geom_histogram()
ggsave("dist_GMAR.png")

# winsorize lower end
a <- quantile(train$GMAR, c(0.01), names = FALSE)
train$GMAR[which(train$GMAR<a)]<-a
test$GMAR[which(test$GMAR<a)]<-a

# Capex_NPPE
ggplot(train, aes(Capex_NPPE)) +
  geom_histogram()
ggsave("dist_Capex_NPPE.png")

# Winsorize higher end
a <- quantile(train$Capex_NPPE, c(0.99), names = FALSE)
train$Capex_NPPE[which(train$Capex_NPPE>a)]<-a
test$Capex_NPPE[which(test$Capex_NPPE>a)]<-a

#Rev_emp
ggplot(train, aes(Rev_Emp)) +
  geom_histogram()
ggsave("dist_Rev_Emp.png")

ggplot(train, aes(log(Rev_Emp))) +
  geom_histogram()
# skewed distribution, the log is more normally distributed
# delete Rev_Emp<1
train$Rev_Emp[which(train$Rev_Emp<1)]<-1
test$Rev_Emp[which(test$Rev_Emp<1)]<-1

# AGE
ggplot(train, aes(Age)) +
  geom_histogram()
ggsave("dist_Age.png")

# winsorize higher end
a <- quantile(train$Age, c(0.99), names = FALSE)
train$Age[which(train$Age>a)]<-a
test$Age[which(test$Age>a)]<-a


ggplot(train, aes(CapInt)) +
  geom_histogram()
ggsave("dist_CapInt.png")

# winsorize higher end
a <- quantile(train$CapInt, c(0.99), names = FALSE)
train$CapInt[which(train$CapInt>a)]<-a
test$CapInt[which(test$CapInt>a)]<-a

# others
ggplot(train, aes(INTAN_TA)) +
  geom_histogram()
ggsave("dist_ITNAN_TA.png")

ggplot(train, aes(FuelInten)) +
  geom_histogram()
ggsave("dist_FuelInten.png")

ggplot(train, aes(gsector)) +
  geom_bar()
ggsave("dist_gsector.png")


# now check weird values
stargazer(train, summary = TRUE, summary.logical = TRUE, 
          header = FALSE,column.sep.width = "-1pt",font.size = "small")#, type = "text")

stargazer(test, summary = TRUE, summary.logical = TRUE, 
          header = FALSE,
          sumstat = c("n", "mean", "sd", "min", "median", "max"))#, type = "text")
str(train)
str(test)

write.csv(train, "train_2405.csv", row.names = FALSE)
write.csv(test, "test_2405.csv", row.names = FALSE)


# test collinearity
df1$FuelInten<-as.numeric(df1$FuelInten)
# everything together
model1 <- lm(scope_total ~ Rev+ Emp+ TA+ NPPE+ INTAN+ 
               Debt_TotCap+ GMAR+ gsector+ Capex+ 
               INTAN_TA+ Capex_NPPE+ Rev_Emp+ FuelInten+ 
               Age+ CapInt + year, data = df1)
stargazer(model1, type = "text")

vif1<- vif(model1)

stargazer(vif1, type = "text")

# as in nguyen
model2 <- lm(scope_total ~ Rev+ Emp+ TA+ NPPE+ INTAN+ 
               Debt_TotCap+ GMAR+ gsector+ Capex+ 
               FuelInten+ 
               Age+ CapInt + year, data = df1)
stargazer(model2, type = "text")

vif2<- vif(model2)

stargazer(vif2, type = "text")

# Ratios and factors only 
model3 <- lm(scope_total ~  
               Debt_TotCap+ GMAR+ gsector+  
               INTAN_TA+ Capex_NPPE+ Rev_Emp+ FuelInten+ 
               Age+ CapInt + year, data = df1)
stargazer(model3, type = "text")

vif3<- vif(model3)

stargazer(vif3, type = "text")

# try with one more scale var
model4 <- lm(scope_total ~  NPPE +
               Debt_TotCap+ GMAR+ gsector+  
               INTAN_TA+ Capex_NPPE+ Rev_Emp+ FuelInten+ 
               Age+ CapInt + year, data = df1)
stargazer(model4, type = "text")

vif4<- vif(model4)

stargazer(vif4, type = "text")

# compare results
stargazer(model1, model2, model3, model4,column.sep.width = "-1pt")#, type = "text")
stargazer(vif1,vif2,vif3,vif4, column.sep.width = "-1pt",type = "text")

table(vif1)
str(df1)

summary(log(df1$NPPE))

df_o <- subset(df1, select = c(scope_total, NPPE ,
                               Debt_TotCap, GMAR, gsector,  
                               INTAN_TA, Capex_NPPE, Rev_Emp, FuelInten, 
                               Age, CapInt , year))
df_o <- na.omit(df_o)

jpeg(file="corrplot_predictors_train.jpeg")
train %>% select(scope_total, NPPE ,
                 Debt_TotCap, GMAR,  
                 INTAN_TA, Capex_NPPE, Rev_Emp, FuelInten, 
                 Age, CapInt) %>% 
  cor() %>% corrplot()
dev.off()

str(train)

train$year <- as.numeric(train$year)
train$gsector <- as.numeric(train$gesector)

