
# Panel Data Models in R 
rm(list = ls())

library(plm)
library(tidyverse)
library(car)
library(lmtest)
library(multiwayvcov)
library(broom)
library(magrittr)

############## Running data 1 ##############

df <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Data/Thesis sheets - Main data (1).csv",
                       col_types = cols(.default = col_guess()
                       )) %>% slice(1:920)



#df <- read_csv("C:/Users/elmar/Desktop/thesis/thesis/Thesis sheets - Main data (1).csv",
#              col_types = cols(.default = col_guess()
#               )) %>% slice(1:920)

# rename for data manipulation and select variabels
colnames(df)[1] <- "year"; colnames(df)[2] <- "name"; colnames(df)[3] <- "country"; colnames(df)[4] <- "region"; colnames(df)[5] <- "industry"; colnames(df)[8] <- "tei"; colnames(df)[9] <- "tec"; colnames(df)[19] <- "esgcomb"; colnames(df)[20] <- "env"; colnames(df)[21] <- "soc"; colnames(df)[24] <- "gdpgrowth"; colnames(df)[25] <- "gdppercap"; colnames(df)[26] <- "profits"; colnames(df)[27] <- "assets"; colnames(df)[28] <- "revenue"; colnames(df)[29] <- "rnd"; colnames(df)[30] <- "liabilities"; colnames(df)[31] <- "se"; colnames(df)[32] <- "leverage"; colnames(df)[33] <- "debtratio";colnames(df)[22] <- "gov"

df <- df %>% select(year, name, country, region, industry, tei, tec, esgcomb, env,soc,gov,gdpgrowth,gdppercap,profits,assets, revenue, rnd,liabilities,se,leverage,debtratio)

df <- df[c(1,2,3,5,4,7,6,8:13,20,21,17,14,15,16,18,19)]

colnames(df)[1] <- "Year";colnames(df)[2] <- "Company";colnames(df)[3] <- "Country";colnames(df)[4] <- "Industry";colnames(df)[5] <- "Region";colnames(df)[7] <- "TEI";colnames(df)[6] <- "TEC";colnames(df)[12] <- "GDP growth";colnames(df)[13] <- "GDP";colnames(df)[14] <- "Leverage";colnames(df)[15] <- "Debt ratio";colnames(df)[17] <- "Profit";colnames(df)[18] <- "Assets";colnames(df)[19] <- "Revenue";colnames(df)[16] <- "RnD";colnames(df)[20] <- "Liabilities";colnames(df)[21] <- "SE";colnames(df)[8] <- "ESG";colnames(df)[9] <- "Env.";colnames(df)[11] <- "Gov.";colnames(df)[10] <- "Soc."


df <- df %>% mutate(assets_log = log(Assets))
df <- df %>% mutate(SE_log = log(SE))
df <- df %>% mutate(ROA = Profit/Assets)
df <- df %>% mutate(GDP_log = log(GDP))
df <- df %>% mutate(RnD_log = log(RnD))
df <- df %>% mutate(Revenue_log = log(Revenue))
df <- df %>% mutate(Liabilities_log = log(Liabilities))
df <- df %>% mutate(TEC_ml = (TEC/1000000))

############ Loading data 2 #########

df <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Data/thesis/Thesis sheets - Main data (1).csv",
               col_types = cols(.default = col_guess()
               )) %>% slice(1:920)

# rename for data manipulation and select variabels
colnames(df)[1] <- "year"; colnames(df)[2] <- "name"; colnames(df)[3] <- "country"; colnames(df)[4] <- "region"; colnames(df)[5] <- "industry"; colnames(df)[8] <- "tei"; colnames(df)[9] <- "tec"; colnames(df)[19] <- "esgcomb"; colnames(df)[20] <- "env"; colnames(df)[21] <- "soc"; colnames(df)[24] <- "gdpgrowth"; colnames(df)[25] <- "gdppercap"; colnames(df)[26] <- "profits"; colnames(df)[27] <- "assets"; colnames(df)[28] <- "revenue"; colnames(df)[29] <- "rnd"; colnames(df)[30] <- "liabilities"; colnames(df)[31] <- "se"; colnames(df)[32] <- "leverage"; colnames(df)[33] <- "debtratio";colnames(df)[22] <- "gov"

df <- df %>% select(year, name, country, region, industry, tei, tec, esgcomb, env,soc,gov,gdpgrowth,gdppercap,profits,assets, revenue, rnd,liabilities,se,leverage,debtratio)

df <- df[c(1,2,3,5,4,7,6,8:13,20,21,17,14,15,16,18,19)]

colnames(df)[1] <- "Year";colnames(df)[2] <- "Company";colnames(df)[3] <- "Country";colnames(df)[4] <- "Industry";colnames(df)[5] <- "Region";colnames(df)[7] <- "TEI";colnames(df)[6] <- "TEC";colnames(df)[12] <- "GDP growth";colnames(df)[13] <- "GDP";colnames(df)[14] <- "Leverage";colnames(df)[15] <- "Debt ratio";colnames(df)[17] <- "Profit";colnames(df)[18] <- "Assets";colnames(df)[19] <- "Revenue";colnames(df)[16] <- "RnD";colnames(df)[20] <- "Liabilities";colnames(df)[21] <- "SE";colnames(df)[8] <- "ESG";colnames(df)[9] <- "Env.";colnames(df)[11] <- "Gov.";colnames(df)[10] <- "Soc."

attach(df)
df <- df %>% arrange(Company, Year)
df <- df %>% mutate(Size = log(Assets))
df <- df %>% mutate(SE_log = log(SE))
df <- df %>% mutate(Profitability = Profit/Assets)
df <- df %>% mutate(GDP_log = log(GDP))
df <- df %>% mutate(RnD_int = RnD/Assets)
df <- df %>% mutate(Revenue_log = log(Revenue))
df <- df %>% mutate(Liabilities_log = log(Liabilities))

df10 <- df %>% filter(!Country == "United States") %>% filter(!Country == "United Kingdom")

write.csv(df,"C:\\Users\\frede\\Desktop\\countryfe.csv", na = "") #row.names = FALSE

############# Start here ###############

attach(df)

Y <- cbind(as.numeric(TEI))

X <- df[c(5,12,15,22,24:25)]
X3 <- X[c(1,2,3,4,6)]
X <- as.matrix(X)
X3 <- as.matrix(X3)

X2 <- df[c(5,12,13,15)]
X2 <- as.matrix(X2)
# Set data as panel data
pdata <- pdata.frame(df, index=c("Company", "Year"))

pdwtest(pdata$ESG ~ X, data = pdata, model = "random")

# Pooled OLS estimator
pooling <- plm(pdata$TEI ~ 
                 pdata$Region+pdata$GDP.growth+
                 pdata$GDP_log+
                 pdata$Profitability+pdata$Size+
                 pdata$Debt.ratio, 
               data = pdata, model = "pooling")

summary(pooling)

# Test to use twoways effect
plmtest(pooling, effect="twoways", type="ghm")

# Pooled OLS estimator with time and individual effects
pooling2 <- plm(pdata$TEI ~ 
                 pdata$Region+pdata$GDP.growth+
                 pdata$GDP_log+
                 pdata$Profitability+pdata$Size+
                 pdata$Debt.ratio + factor(Year), 
               data = pdata, model = "pooling")

coeftest(pooling2, vcov=vcovHC(pooling2, type = "HC2", cluster = "group"))

pooling3 <- plm(pdata$ESG ~ 
                  pdata$Region+pdata$GDP.growth+
                  pdata$GDP_log+
                  pdata$Profitability+pdata$Size+
                  pdata$Debt.ratio + factor(Year), 
                data = pdata, model = "pooling")
coeftest(pooling2, vcov=vcovHC(pooling2, type = "HC2", cluster = "group"))

# Random effects estimator
random <- plm(Y ~ X + factor(Year), data = pdata, model = "random")
summary(random)

random2 <- plm(Y ~ X2, data = pdata, model = "random")
summary(random2)


#LSDV
attach(pdata)
fd <- lm(TEI ~ Region+`GDP growth`+GDP_log+ROA+assets_log+factor(Company)-1)

summary(fd)

vif(pooling)


?coef_test
vcov_firm <- cluster.vcov(pooling2, df$Company)
#coeftest(m1, vcov_firm)


# BP test for random effects versus OLS
plmtest(pooling, type = "bp")
?plmtest

# bptest for heteroskedasticity
# The null hypothesis for the Breusch-Pagan test is homoskedasticity

bptest(ESG ~ X, data = pdata, studentize = F)

?bptest

plmtest(pooling, type=c("bp"))

# Descriptive statistics
summary(Y)
summary(X)


coeftest(fixed)
coeftest(random, vcovHC(random, type = "HC3"))

pooling3 <- vcovHC(pooling, type = "HC3")

?vcovHC

# Fixed effects or within estimator
fixed <- plm(Y ~ X, data=pdata, model = "within")
summary(fixed)

# LM test for fixed effects versus OLS
pFtest(random, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)

############ CSV file for USA  and EEA ########

df99 <- df[!grepl("Canada", df$Country),]
df99 <- df99[!grepl("Russia", df99$Country),]
df99 <- df99[!grepl("United Kingdom", df99$Country),]

table(df99$Region)

write.csv(df99,"C:\\Users\\frede\\Desktop\\usaeeadata.csv", na = "")

