
# Merging document

library(tidyverse)
library(ggpubr)
library(rstatix)
library(kableExtra)
library(table1)
library(stargazer)

# Only for clearing the list
rm(list = ls())

################# Getting the datasets ready ###################

  # Load the two sets

main2 <- read_csv2("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/thesisnew csv.csv",
                  col_types = cols(.default = col_guess()
                  ))
main2 <- main2 %>% slice(1:729)

main <- read_csv2("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Main merge csv.csv",
                    col_types = cols(.default = col_guess()
                    ))

orbis <- read_csv2("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Orbis 1 csv.csv",
                  col_types = cols(.default = col_guess()
                  ))

  # Sync the names of main data set two enable merging at the end
colnames(main)[1] <- "year"; colnames(main)[2] <- "name"

  # Renaming all profit/loss and 'name'
colnames(orbis)[2] <- "name";colnames(orbis)[12] <- "pl18";colnames(orbis)[13] <- "pl17";colnames(orbis)[14] <- "pl16";colnames(orbis)[15] <- "pl15";colnames(orbis)[16] <- "pl14";colnames(orbis)[17] <- "pl13";colnames(orbis)[18] <- "pl12";colnames(orbis)[19] <- "pl11"

  # Renaming all assets
colnames(orbis)[28] <- "as18";colnames(orbis)[29] <- "as17";colnames(orbis)[30] <- "as16";colnames(orbis)[31] <- "as15";colnames(orbis)[32] <- "as14";colnames(orbis)[33] <- "as13";colnames(orbis)[34] <- "as12";colnames(orbis)[35] <- "as11"

# In order to merge Orbis data with HBS data, I will take one year of one variable at a time (2011 is the earliest available data from Orbis)



################# First Merging attempt with P/L 2018 FORGET THIS ONE ###################################

# Select only chosen variables, name and the other variable
orbispl18 <- orbis %>% select(name, pl18)

# Now converge all pl's to 'pl', without the year
colnames(orbispl18)[2] <- "pl"

# Make a list of the chosen year
year <- c(2018)

# Implement the list
orbispl18$year <- year  

# Order the df according to main data
orbispl18 <- orbispl18[,c(3,1,2)] 

# I'll create copies to try and merge them 

maincopy <- main
orbispl18copy <- orbispl18

# Selecting only 2018 from main with a filter
maincopy <- maincopy %>% filter(year == '2018')

# Removing year from orbiscopy and grouping by name for both
orbispl18copy <- orbispl18copy %>% select(name, pl) %>% group_by(name)

maincopy <- maincopy %>% group_by(name)

# Merging attempt
merged18 <- merge(x = orbispl18copy, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged18 <- merged18[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


################# Second Merging attempt with P/L 2017 USE THIS ONE ################

  # Select only chosen variables, name and the other variable
orbispl17 <- orbis %>% select(name, pl17, as17) %>% group_by(name)

  # Now converge pl1X to 'pl', without the year
colnames(orbispl17)[2] <- "pl";colnames(orbispl17)[3] <- "as"

  # Create new main copy and filter year
maincopy <- main %>% filter(year == '2017') %>% group_by(name)

  # Merging attempt
merged17 <- merge(x = orbispl17, y = maincopy, by = "name", all.x = TRUE)

  # Regrouping merged 
merged17 <- merged17[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 

# Select only chosen variables, name and the other variable
orbispl16 <- orbis %>% select(name, pl16) %>% group_by(name)

# Now converge pl1X to 'pl', without the year
colnames(orbispl16)[2] <- "pl"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2016') %>% group_by(name)

# Merging attempt
merged16 <- merge(x = orbispl16, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged16 <- merged16[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 



# Select only chosen variables, name and the other variable
orbispl15 <- orbis %>% select(name, pl15) %>% group_by(name)

# Now converge pl1X to 'pl', without the year
colnames(orbispl15)[2] <- "pl"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2015') %>% group_by(name)

# Merging attempt
merged15 <- merge(x = orbispl15, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged15 <- merged15[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


# Select only chosen variables, name and the other variable
orbispl14 <- orbis %>% select(name, pl14) %>% group_by(name)

# Now converge pl1X to 'pl', without the year
colnames(orbispl14)[2] <- "pl"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2014') %>% group_by(name)

# Merging attempt
merged14 <- merge(x = orbispl14, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged14 <- merged14[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


# Select only chosen variables, name and the other variable
orbispl13 <- orbis %>% select(name, pl13) %>% group_by(name)

# Now converge pl1X to 'pl', without the year
colnames(orbispl13)[2] <- "pl"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2013') %>% group_by(name)

# Merging attempt
merged13 <- merge(x = orbispl13, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged13 <- merged13[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


# Select only chosen variables, name and the other variable
orbispl12 <- orbis %>% select(name, pl12) %>% group_by(name)

# Now converge pl1X to 'pl', without the year
colnames(orbispl12)[2] <- "pl"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2012') %>% group_by(name)

# Merging attempt
merged12 <- merge(x = orbispl12, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged12 <- merged12[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]


# Select only chosen variables, name and the other variable
orbispl11 <- orbis %>% select(name, pl11) %>% group_by(name)

# Now converge pl1X to 'pl', without the year
colnames(orbispl11)[2] <- "pl"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2011') %>% group_by(name)

# Merging attempt
merged11 <- merge(x = orbispl11, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged11 <- merged11[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


# Create new main copy and filter year
maincopy <- main %>% filter(year == '2010') %>% group_by(name)

orbispl10 <- orbispl18 

orbispl10['pl'] <- NA; orbispl10['year'] <- 2010

orbispl10 <- orbispl10 %>% select(name, pl)

# Merging attempt
merged10 <- merge(x = orbispl10, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged10 <- merged10[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


################# Merging all P/L ################
  # Easy 
pl <- rbind(merged10, merged11, merged12, merged13, merged14, merged15, merged16, merged17, merged18)
  # Selecting only the new data to keep the two sheets separate

pl <- pl %>% select(year, name, pl)






################# Merging individual total assets ##############

### 2011

# Select only chosen variables, name and the other variable
assets11 <- orbis %>% select(name, as11) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets11)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2011') %>% group_by(name)

# Merging attempt
merged11 <- merge(x = assets11, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged11 <- merged11[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2012

# Select only chosen variables, name and the other variable
assets12 <- orbis %>% select(name, as12) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets12)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2012') %>% group_by(name)

# Merging attempt
merged12 <- merge(x = assets12, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged12 <- merged12[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2013

# Select only chosen variables, name and the other variable
assets13 <- orbis %>% select(name, as13) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets13)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2013') %>% group_by(name)

# Merging attempt
merged13 <- merge(x = assets13, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged13 <- merged13[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2014

# Select only chosen variables, name and the other variable
assets14 <- orbis %>% select(name, as14) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets14)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2014') %>% group_by(name)

# Merging attempt
merged14 <- merge(x = assets14, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged14 <- merged14[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2015

# Select only chosen variables, name and the other variable
assets15 <- orbis %>% select(name, as15) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets15)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2015') %>% group_by(name)

# Merging attempt
merged15 <- merge(x = assets15, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged15 <- merged15[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2016

# Select only chosen variables, name and the other variable
assets16 <- orbis %>% select(name, as16) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets16)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2016') %>% group_by(name)

# Merging attempt
merged16 <- merge(x = assets16, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged16 <- merged16[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2017

# Select only chosen variables, name and the other variable
assets17 <- orbis %>% select(name, as17) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets17)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2017') %>% group_by(name)

# Merging attempt
merged17 <- merge(x = assets17, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged17 <- merged17[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2018

# Select only chosen variables, name and the other variable
assets18 <- orbis %>% select(name, as18) %>% group_by(name)

# Now converge as1X to 'as', without the year
colnames(assets18)[2] <- "as"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2018') %>% group_by(name)

# Merging attempt
merged18 <- merge(x = assets18, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged18 <- merged18[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]




################# Adding 2010 to AS ################

# Create new asset df
assets10 <- assets18

# Create/rename missing columns
assets10['as'] <- NA; assets10['year'] <- 2010

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2010') %>% group_by(name)

assets10 <- assets10 %>% select(name, as)

# Merging attempt
merged10 <- merge(x = assets10, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged10 <- merged10[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 


################# Merging all AS ################
# Easy 
assets <- rbind(merged10, merged11, merged12, merged13, merged14, merged15, merged16, merged17, merged18)
# Selecting only the new data to keep the two sheets separate

as <- assets %>% select(year, name, as)

################# Getting AS ready ################

as <- as %>% mutate(as = na_if(as, "n.a."))

sapply(merged11, class)


assets <- as

assets <- assets %>%
  mutate(as = str_replace_all(as, "[.]", "")) 

assets$as = as.numeric(as.character(assets$as))

rm(as)

################# Getting PL ready ################

proloss <- pl %>% mutate(pl = na_if(pl, "n.a."))



sapply(proloss, class)


proloss <- proloss %>%
  mutate(pl = str_replace_all(pl, "[.]", "")) 

proloss$pl = as.numeric(as.character(proloss$pl))

rm(pl)




















################# Main merge attempt ######

main <- main %>% slice(1:711) %>% group_by(name)

assetspl <- left_join(assets, proloss, by.x = "name", by.y = "name")

main2 <- left_join(main, assetspl, by.x = "name", by.y = "name")


################# Visualization attempt ######

 # Renaming columns

colnames(main2)[9] <- "tec"; colnames(main2)[5] <- "industry"

main2$industry[main2$industry =="Petroleum Refinery"] <- "Petroleum refinery"
main2$industry[main2$industry =="Extraction of crude petroleum and services related to crude oil extraction, excluding surveying"] <- "Petroleum"
main2$industry[main2$industry =="Production of electricity by petroleum and other oil derivatives"] <- "Electricity"
main2$industry[main2$industry =="Extraction of natural gas and services related to natural gas extraction, excluding surveying"] <- "Natural gas"


p <- ggplot(data = main2,
       mapping = aes(x = tec, y = pl, color = industry)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Total Environmental Cost versus Profits")  + 
  geom_smooth(method = lm, se = TRUE, color = "black") +
  scale_x_continuous(name = "Total Environmental Cost", ) +
  scale_y_continuous(name = "Profit/Loss")
 

p+stat_cor(method="pearson", color = "black")

?geom_smooth
?ggplot

write.csv(assetspl,"C:\\Users\\frede\\Desktop\\assetspl.csv", row.names = FALSE)


################# Getting revenue ready ############

# Renaming all revenue
colnames(orbis)[68] <- "rev18";colnames(orbis)[69] <- "rev17";colnames(orbis)[70] <- "rev16";colnames(orbis)[71] <- "rev15";colnames(orbis)[72] <- "rev14";colnames(orbis)[73] <- "rev13";colnames(orbis)[74] <- "rev12";colnames(orbis)[75] <- "rev11"


rev <- orbis %>% select(name, rev11, rev12, rev13, rev14, rev15, rev16, rev17, rev18)

rev <- rev %>%
  mutate(rev11 = str_replace_all(rev11, "[.]", "")) %>%
  mutate(rev12 = str_replace_all(rev12, "[.]", "")) %>%
  mutate(rev13 = str_replace_all(rev13, "[.]", "")) %>%
  mutate(rev14 = str_replace_all(rev14, "[.]", "")) %>%
  mutate(rev15 = str_replace_all(rev15, "[.]", "")) %>%
  mutate(rev16 = str_replace_all(rev16, "[.]", "")) %>%
  mutate(rev17 = str_replace_all(rev17, "[.]", "")) %>%
  mutate(rev18 = str_replace_all(rev18, "[.]", "")) 

rev <- rev %>% 
  mutate(rev11 = na_if(rev11, "na")) %>% 
  mutate(rev12 = na_if(rev12, "na")) %>% 
  mutate(rev13 = na_if(rev13, "na")) %>% 
  mutate(rev14 = na_if(rev14, "na")) %>% 
  mutate(rev15 = na_if(rev15, "na")) %>% 
  mutate(rev16 = na_if(rev16, "na")) %>% 
  mutate(rev17 = na_if(rev17, "na")) %>% 
  mutate(rev18 = na_if(rev18, "na"))


rev$rev11 = as.numeric(as.character(rev$rev11))
rev$rev12 = as.numeric(as.character(rev$rev12))
rev$rev13 = as.numeric(as.character(rev$rev13))
rev$rev14 = as.numeric(as.character(rev$rev14))
rev$rev15 = as.numeric(as.character(rev$rev15))
rev$rev16 = as.numeric(as.character(rev$rev16))
rev$rev17 = as.numeric(as.character(rev$rev17))
rev$rev18 = as.numeric(as.character(rev$rev18))









################# Merging revenue ######

### 2011

# Select only chosen variables, name and the other variable
rev11 <- rev %>% select(name, rev11) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev11)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2011') %>% group_by(name)

# Merging attempt
merged11 <- merge(x = rev11, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged11 <- merged11[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2012

# Select only chosen variables, name and the other variable
rev12 <- rev %>% select(name, rev12) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev12)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2012') %>% group_by(name)

# Merging attempt
merged12 <- merge(x = rev12, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged12 <- merged12[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2013

# Select only chosen variables, name and the other variable
rev13 <- rev %>% select(name, rev13) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev13)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2013') %>% group_by(name)

# Merging attempt
merged13 <- merge(x = rev13, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged13 <- merged13[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2014

# Select only chosen variables, name and the other variable
rev14 <- rev %>% select(name, rev14) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev14)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2014') %>% group_by(name)

# Merging attempt
merged14 <- merge(x = rev14, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged14 <- merged14[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2015

# Select only chosen variables, name and the other variable
rev15 <- rev %>% select(name, rev15) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev15)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2015') %>% group_by(name)

# Merging attempt
merged15 <- merge(x = rev15, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged15 <- merged15[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2016

# Select only chosen variables, name and the other variable
rev16 <- rev %>% select(name, rev16) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev16)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2016') %>% group_by(name)

# Merging attempt
merged16 <- merge(x = rev16, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged16 <- merged16[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2017

# Select only chosen variables, name and the other variable
rev17 <- rev %>% select(name, rev17) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev17)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2017') %>% group_by(name)

# Merging attempt
merged17 <- merge(x = rev17, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged17 <- merged17[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]

### 2018

# Select only chosen variables, name and the other variable
rev18 <- rev %>% select(name, rev18) %>% group_by(name)

# Now converge as1X to 'rev', without the year
colnames(rev18)[2] <- "rev"

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2018') %>% group_by(name)

# Merging attempt
merged18 <- merge(x = rev18, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged18 <- merged18[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)]





################# Adding 2010 to revenue ######

# Create new asset df
rev10 <- rev18

# Create/rename missing columns
rev10['rev'] <- NA; rev10['year'] <- 2010

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2010') %>% group_by(name)

rev10 <- rev10 %>% select(name, rev)

# Merging attempt
merged10 <- merge(x = rev10, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged10 <- merged10[,c(3,1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,2)] 







################# Merging all revenues ######

# Easy 
revenue <- rbind(merged10, merged11, merged12, merged13, merged14, merged15, merged16, merged17, merged18)
# Selecting only the new data to keep the two sheets separate

revenue <- revenue %>% select(year, name, rev)

write.csv(revenue,"C:\\Users\\frede\\Desktop\\revenue.csv", row.names = FALSE)





################# NEW(ish) DATA ###############

# This is actually Orbis3
main3 <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Orbis2 csv.csv",
                   col_types = cols(.default = col_guess()
                   ))

main4 <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/newest csv.csv",
                   col_types = cols(.default = col_guess()
                   ))
main4 <- main4 %>% slice(1:729)
colnames(main4)[2] <- "name";colnames(main4)[1] <- "year"


# Sync the names of main data set two enable merging at the end

colnames(main3)[2] <- "name"; colnames(main3)[18] <- "rev18"; colnames(main3)[19] <- "rev17"; colnames(main3)[20] <- "rev16"; colnames(main3)[21] <- "rev15"; colnames(main3)[22] <- "rev14"; colnames(main3)[23] <- "rev13"; colnames(main3)[24] <- "rev12"; colnames(main3)[25] <- "rev11"
colnames(main3)[52] <- "as18"; colnames(main3)[53] <- "as17"; colnames(main3)[54] <- "as16"; colnames(main3)[55] <- "as15"; colnames(main3)[56] <- "as14"; colnames(main3)[57] <- "as13"; colnames(main3)[58] <- "as12"; colnames(main3)[59] <- "as11"
colnames(main3)[86] <- "pl18"; colnames(main3)[87] <- "pl17"; colnames(main3)[88] <- "pl16"; colnames(main3)[89] <- "pl15"; colnames(main3)[90] <- "pl14"; colnames(main3)[91] <- "pl13"; colnames(main3)[92] <- "pl12"; colnames(main3)[93] <- "pl11"
colnames(main3)[110] <- "rnd18"; colnames(main3)[111] <- "rnd17"; colnames(main3)[112] <- "rnd16"; colnames(main3)[113] <- "rnd15"; colnames(main3)[114] <- "rnd14"; colnames(main3)[115] <- "rnd13"; colnames(main3)[116] <- "rnd12"; colnames(main3)[117] <- "rnd11"
colnames(main3)[178] <- "lnd18"; colnames(main3)[179] <- "lnd17"; colnames(main3)[180] <- "lnd16"; colnames(main3)[181] <- "lnd15"; colnames(main3)[182] <- "lnd14"; colnames(main3)[183] <- "lnd13"; colnames(main3)[184] <- "lnd12"; colnames(main3)[185] <- "lnd11"
colnames(main3)[200] <- "se18"; colnames(main3)[201] <- "se17"; colnames(main3)[202] <- "se16"; colnames(main3)[203] <- "se15"; colnames(main3)[204] <- "se14"; colnames(main3)[205] <- "se13"; colnames(main3)[206] <- "se12"; colnames(main3)[207] <- "se11"

### 2018

# Select only chosen variables, name and the other variable
orbis18 <- main3 %>% select(name, pl18, as18, rev18, rnd18, lnd18, se18) %>% group_by(name) %>% arrange(name)

# Now converge without the year
colnames(orbis18)[2] <- "pl";colnames(orbis18)[3] <- "as";colnames(orbis18)[4] <- "rev";colnames(orbis18)[5] <- "rnd";colnames(orbis18)[6] <- "lnd";colnames(orbis18)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2018') %>% group_by(name)

# Merging attempt
merged18 <- merge(x = orbis18, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged18 <- merged18[,c(8,1,2,3,4,5,6,7)] 


### 2017

# Select only chosen variables, name and the other variable
orbis17 <- main3 %>% select(name, pl17, as17, rev17, rnd17, lnd17, se17) %>% group_by(name) %>% arrange(name)

# Now converge without the year
colnames(orbis17)[2] <- "pl";colnames(orbis17)[3] <- "as";colnames(orbis17)[4] <- "rev";colnames(orbis17)[5] <- "rnd";colnames(orbis17)[6] <- "lnd";colnames(orbis17)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2017') %>% group_by(name)

# Merging attempt
merged17 <- merge(x = orbis17, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged17 <- merged17[,c(8,1,2,3,4,5,6,7)] 

### 2016

# Select only chosen variables, name and the other variable
orbis16 <- main3 %>% select(name, pl16, as16, rev16, rnd16, lnd16, se16) %>% group_by(name)%>% arrange(name)

# Now converge without the year
colnames(orbis16)[2] <- "pl";colnames(orbis16)[3] <- "as";colnames(orbis16)[4] <- "rev";colnames(orbis16)[5] <- "rnd";colnames(orbis16)[6] <- "lnd";colnames(orbis16)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2016') %>% group_by(name)

# Merging attempt
merged16 <- merge(x = orbis16, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged16 <- merged16[,c(8,1,2,3,4,5,6,7)] 

### 2015

# Select only chosen variables, name and the other variable
orbis15 <- main3 %>% select(name, pl15, as15, rev15, rnd15, lnd15, se15) %>% group_by(name)%>% arrange(name)

# Now converge without the year
colnames(orbis15)[2] <- "pl";colnames(orbis15)[3] <- "as";colnames(orbis15)[4] <- "rev";colnames(orbis15)[5] <- "rnd";colnames(orbis15)[6] <- "lnd";colnames(orbis15)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2015') %>% group_by(name)

# Merging attempt
merged15 <- merge(x = orbis15, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged15 <- merged15[,c(8,1,2,3,4,5,6,7)] 

### 2014

# Select only chosen variables, name and the other variable
orbis14 <- main3 %>% select(name, pl14, as14, rev14, rnd14, lnd14, se14) %>% group_by(name)%>% arrange(name)

# Now converge without the year
colnames(orbis14)[2] <- "pl";colnames(orbis14)[3] <- "as";colnames(orbis14)[4] <- "rev";colnames(orbis14)[5] <- "rnd";colnames(orbis14)[6] <- "lnd";colnames(orbis14)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2014') %>% group_by(name)

# Merging attempt
merged14 <- merge(x = orbis14, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged14 <- merged14[,c(8,1,2,3,4,5,6,7)] 

### 2013

# Select only chosen variables, name and the other variable
orbis13 <- main3 %>% select(name, pl13, as13, rev13, rnd13, lnd13, se13) %>% group_by(name)%>% arrange(name)

# Now converge without the year
colnames(orbis13)[2] <- "pl";colnames(orbis13)[3] <- "as";colnames(orbis13)[4] <- "rev";colnames(orbis13)[5] <- "rnd";colnames(orbis13)[6] <- "lnd";colnames(orbis13)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2013') %>% group_by(name)

# Merging attempt
merged13 <- merge(x = orbis13, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged13 <- merged13[,c(8,1,2,3,4,5,6,7)] 

### 2012

# Select only chosen variables, name and the other variable
orbis12 <- main3 %>% select(name, pl12, as12, rev12, rnd12, lnd12, se12) %>% group_by(name)%>% arrange(name)

# Now converge without the year
colnames(orbis12)[2] <- "pl";colnames(orbis12)[3] <- "as";colnames(orbis12)[4] <- "rev";colnames(orbis12)[5] <- "rnd";colnames(orbis12)[6] <- "lnd";colnames(orbis12)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2012') %>% group_by(name)

# Merging attempt
merged12 <- merge(x = orbis12, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged12 <- merged12[,c(8,1,2,3,4,5,6,7)] 

### 2011

# Select only chosen variables, name and the other variable
orbis11 <- main3 %>% select(name, pl11, as11, rev11, rnd11, lnd11, se11) %>% group_by(name) %>% arrange(name)

# Now converge without the year
colnames(orbis11)[2] <- "pl";colnames(orbis11)[3] <- "as";colnames(orbis11)[4] <- "rev";colnames(orbis11)[5] <- "rnd";colnames(orbis11)[6] <- "lnd";colnames(orbis11)[7] <- "se"

# Create new main copy and filter year
maincopy <- main4 %>% filter(year == '2011') %>% group_by(name) %>% arrange(name)

# Merging attempt
merged11 <- merge(x = orbis11, y = maincopy, by = "name", all.x = TRUE)

# Regrouping merged 
merged11 <- merged11[,c(8,1,2,3,4,5,6,7)] 

### 2010

# Create new asset df
merged10 <- merged11

# Create/rename missing columns
merged10['rev'] <- NA; merged10['year'] <- 2010; merged10['as'] <- NA; merged10['pl'] <- NA; merged10['rnd'] <- NA; merged10['lnd'] <- NA; merged10['se'] <- NA

# Create new main copy and filter year
maincopy <- main %>% filter(year == '2010') %>% group_by(name)


### Merging all of them

# Easy 
lastmerge <- rbind(merged10, merged11, merged12, merged13, merged14, merged15, merged16, merged17, merged18)
# Selecting only the new data to keep the two sheets separate

write.csv(lastmerge,"C:\\Users\\frede\\Desktop\\lastmerge.csv", row.names = FALSE)





################# cormat and sum stat ###############

# Load the two sets

vis <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Test/second test csv.csv",
                   col_types = cols(.default = col_guess()
                   ))
visfull <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Test/second test csv.csv",
                    col_types = cols(.default = col_guess()
                    ))


vis2 <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Test/second test csv.csv",
                    col_types = cols(.default = col_guess()
                    ))

# rename for data manipulation and select variabels
colnames(vis)[1] <- "year"; colnames(vis)[2] <- "name"; colnames(vis)[3] <- "country"; colnames(vis)[4] <- "region"; colnames(vis)[5] <- "industry"; colnames(vis)[8] <- "tei"; colnames(vis)[12] <- "tec"; colnames(vis)[28] <- "esgcomb"; colnames(vis)[30] <- "env"; colnames(vis)[31] <- "soc"; colnames(vis)[37] <- "gdpgrowth"; colnames(vis)[38] <- "gdppercap"; colnames(vis)[39] <- "profits"; colnames(vis)[40] <- "assets"; colnames(vis)[41] <- "revenue"; colnames(vis)[42] <- "rnd"; colnames(vis)[43] <- "liabilities"; colnames(vis)[44] <- "se"; colnames(vis)[45] <- "leverage"; colnames(vis)[46] <- "debtratio"
colnames(vis)[32] <- "gov"
vis <- vis %>% select(year, name, country, region, industry, tei, tec, esgcomb, env,soc,gov,gdpgrowth,gdppercap,profits,assets, revenue, rnd,liabilities,se,leverage,debtratio)



# Creating correlation matrix
cormat <- cor_mat(vis[,6:20]) %>% pull_lower_triangle(diagonal = FALSE)

visfull <- visfull[c(8,12,28,30,31,37:46)]

full_names <- colnames(visfull[,1:14])
short_names <- colnames(cormat[,2:15])

kbl(cormat[,-16], booktabs = T, linesep = "",
    align = c("l", rep("c",15)),
    col.names = c("",var_names),
    row.names = F) %>%
  kable_styling(font_size = 10) %>%
  landscape() %>%
  kable_styling()

sumtab <- tableby(data = visfull) 

summary(sumtab, title = "All data")
?table1

visfull <- visfull[c(1,2,3,4,5,8,12,28,30:32,37:46)]

# Creating mat_cor

colnames(visfull)[6] <- "TEI";colnames(visfull)[7] <- "TEC";colnames(visfull)[12] <- "Annual GDP Growth";colnames(visfull)[13] <- "GDP per capita";colnames(visfull)[20] <- "Financial Leverage";colnames(visfull)[21] <- "Debt ratio";

cormat <- cor_mat(visfull[,6:20]) %>% pull_lower_triangle(diagonal = FALSE)

var_names <- colnames(cormat[,2:15])

kbl(cormat[,-16], booktabs = T, linesep = "",
    align = c("l", rep("c",15)),
    col.names = c("",var_names),
    row.names = F) %>%
  kable_styling(font_size = 10) %>%
  landscape() %>%
  kable_styling()


library(stargazer)

stargazer(as.data.frame(visfull[6:21]), digits.extra = 0, summary.stat = c("n", "median", "mean", "sd", "min", "max"))




################# Timeseries plot ###########

# Setting standard theme for ggplot
theme_set(theme_classic() +
            theme(legend.position = "bottom",
                  legend.box.spacing = unit(-0.4, "cm"),
                  legend.margin = margin(6, 6, 6, 6),
                  legend.title = element_blank(),
            ))



# Summarizing the tec data
time <- vis %>% select(year, region, tec) %>% group_by(year, region) %>% 
  summarise(tec_mean = round(mean(tec, na.rm = TRUE), 2))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                           grepl("1", region, ignore.case = TRUE) ~"North America"))


# Plotting TEC

tecplot <- ggplot(time, mapping = aes(x = year, y = tec_mean/100000*1)) +
  geom_line(aes(color = region), lwd = 1.5) +
  ylim(-3095, -65000) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "TEC / 100,000", title = "Yearly average TEC for Europe and North America", subtitle = "Lower TEC is worse. Note that the y-axis is flipped.")

tecplot


# Summarizing the tei data
timetei <- vis %>% select(year, region, tei) %>% group_by(year, region) %>% 
  summarise(tei_mean = round(mean(tei, na.rm = TRUE), 2))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                            grepl("1", region, ignore.case = TRUE) ~"North America"))

# Plotting TEI

p <- ggplot(timetei, mapping = aes(x = year, y = tei_mean)) +
  geom_line(aes(color = region), lwd = 2) + 
  ylim(-0.0147, -0.31) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "TEI", title = "Yearly average TEI for Europe and North America", subtitle = "Lower TEI is worse. Note that the y-axis is flipped.") 
p



# Summarizing theasset data
assetmean <- vis %>% select(year, region, assets) %>% group_by(year, region) %>% filter(!year == "2010") %>% 
  summarise(assets_mean = round(mean(assets, na.rm = TRUE)))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                            grepl("1", region, ignore.case = TRUE) ~"North America"))

# Plotting assets_mean

ggplot(assetmean, mapping = aes(x = year, y = assets_mean/1000)) +
  geom_line(aes(color = region), lwd = 2) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Assets in $million", title = "Average assets for European and North American firms", subtitle = "") 

# Plotting ESG

esg <- vis %>% select(year, region, esgcomb) %>% group_by(year, region) %>% 
  summarise(esg_mean = round(mean(esgcomb, na.rm = TRUE), 2))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                            grepl("1", region, ignore.case = TRUE) ~"North America"))

esg <- ggplot(esg, mapping = aes(x = year, y = esg_mean)) +
  geom_line(aes(color = region), lwd = 2) + 
  ylim(0.38, 8) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Combined ESG", title = "Average Combined ESG", subtitle = "Incl. ESG Controversies score") 


esgenv <- vis %>% select(year, region, env) %>% group_by(year, region) %>% 
  summarise(env_mean = round(mean(env, na.rm = TRUE), 2))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                            grepl("1", region, ignore.case = TRUE) ~"North America"))

esgenv <- ggplot(esgenv, mapping = aes(x = year, y = env_mean)) +
  geom_line(aes(color = region), lwd = 2) + 
  ylim(0.38, 8) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Environmental Score", title = "Average Environmental", subtitle = "") 

esgsoc <- vis %>% select(year, region, soc) %>% group_by(year, region) %>% 
  summarise(soc_mean = round(mean(soc, na.rm = TRUE), 2))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                            grepl("1", region, ignore.case = TRUE) ~"North America"))

esgsoc <- ggplot(esgsoc, mapping = aes(x = year, y = soc_mean)) +
  geom_line(aes(color = region), lwd = 2) + 
  ylim(0.38, 9) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Social Score", title = "Average Social", subtitle = "") 


esggov <- vis %>% select(year, region, gov) %>% group_by(year, region) %>% 
  summarise(gov_mean = round(mean(gov, na.rm = TRUE), 2))  %>%  
  mutate(region = case_when(grepl("0", region) ~ "Europe",
                            grepl("1", region, ignore.case = TRUE) ~"North America"))

esggov <- ggplot(esggov, mapping = aes(x = year, y = gov_mean)) +
  geom_line(aes(color = region), lwd = 2) + 
  ylim(0.38, 9) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Governance Score", title = "Average Governance", subtitle = "") 

### Plotting ESG in one

ggarrange(ggarrange(esg, esgenv, ncol = 2, labels = c("A", "B")),                                                 
          ggarrange(esgsoc, esggov, ncol = 2, labels = c("C", "D")), 
          nrow = 2                                       
  )



################# Cormat #############

visfull <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Thesis/Data/Test/second test csv.csv",
                    col_types = cols(.default = col_guess()
                    ))

visfull <- visfull[c(1,2,3,4,5,8,12,28,30:32,37:46)]

colnames(visfull)[6] <- "TEI";colnames(visfull)[7] <- "TEC";colnames(visfull)[12] <- "GDP growth";colnames(visfull)[13] <- "GDP";colnames(visfull)[20] <- "Leverage";colnames(visfull)[21] <- "Debt ratio";colnames(visfull)[14] <- "Profit";colnames(visfull)[15] <- "Assets";colnames(visfull)[16] <- "Revenue";colnames(visfull)[17] <- "RnD";colnames(visfull)[18] <- "Liabilities";colnames(visfull)[19] <- "SE";colnames(visfull)[8] <- "ESG";colnames(visfull)[9] <- "Env.";colnames(visfull)[11] <- "Gov.";colnames(visfull)[10] <- "Soc.";colnames(visfull)[4] <- "Region";

visfull <- visfull[,c(1:3,5,4,7,6,8:13,20,21,17,14,15,16,18,19)]

cormat2 <- cor_mat(visfull[,5:21], method = "pearson") %>% pull_lower_triangle(diagonal = FALSE)

var_names2 <- colnames(cormat2[,2:17])

cormat2 <- cormat2[-1,-18]

kbl(cormat2[,-18], booktabs = T, linesep = "",
    align = c("l", rep("c",15)),
    col.names = c("",var_names2),
    row.names = F) %>%
  kable_styling(font_size = 8) %>%
  landscape() %>%
  kable_styling()



################# NEWest DATA START HERE ###################

df <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Data/New data - v1 csv.csv",
                 col_types = cols(.default = col_guess()
                 )) %>% slice(1:920)

# rename for data manipulation and select variabels
colnames(df)[1] <- "year"; colnames(df)[2] <- "name"; colnames(df)[3] <- "country"; colnames(df)[4] <- "region"; colnames(df)[5] <- "industry"; colnames(df)[8] <- "tei"; colnames(df)[9] <- "tec"; colnames(df)[19] <- "esgcomb"; colnames(df)[20] <- "env"; colnames(df)[21] <- "soc"; colnames(df)[24] <- "gdpgrowth"; colnames(df)[25] <- "gdppercap"; colnames(df)[26] <- "profits"; colnames(df)[27] <- "assets"; colnames(df)[28] <- "revenue"; colnames(df)[29] <- "rnd"; colnames(df)[30] <- "liabilities"; colnames(df)[31] <- "se"; colnames(df)[32] <- "leverage"; colnames(df)[33] <- "debtratio";colnames(df)[22] <- "gov"

df <- df %>% select(year, name, country, region, industry, tei, tec, esgcomb, env,soc,gov,gdpgrowth,gdppercap,profits,assets, revenue, rnd,liabilities,se,leverage,debtratio)

df <- df[c(1,2,3,5,4,7,6,8:13,20,21,17,14,15,16,18,19)]

colnames(df)[1] <- "Year";colnames(df)[2] <- "Company name";colnames(df)[3] <- "Country";colnames(df)[4] <- "Industry";colnames(df)[5] <- "Region";colnames(df)[7] <- "TEI";colnames(df)[6] <- "TEC";colnames(df)[12] <- "GDP growth";colnames(df)[13] <- "GDP";colnames(df)[14] <- "Leverage";colnames(df)[15] <- "Debt ratio";colnames(df)[17] <- "Profit";colnames(df)[18] <- "Assets";colnames(df)[19] <- "Revenue";colnames(df)[16] <- "RnD";colnames(df)[20] <- "Liabilities";colnames(df)[21] <- "SE";colnames(df)[8] <- "ESG";colnames(df)[9] <- "Env.";colnames(df)[11] <- "Gov.";colnames(df)[10] <- "Soc."

# Create cormat
cormat <- cor_mat(df[,5:21], method = "pearson") %>% pull_lower_triangle(diagonal = FALSE)

var_names <- colnames(cormat[,2:17])

cormat <- cormat[-1,-18]

attributes(cormat)

kbl(cormat[,-18], booktabs = T, linesep = "",
    align = c("l", rep("c",15)),
    col.names = c("",var_names),
    row.names = F) %>%
  kable_styling(font_size = 8) %>%
  landscape() %>%
  kable_styling()

### Sum stat
library(stargazer)

stargazer(as.data.frame(df[5:21]), digits.extra = 0, summary.stat = c("n", "median", "mean", "sd", "min", "max"))

### New graphs

dfplot <- df %>% select(Year, "Company name", Country, Region, Industry, TEI, TEC, ESG, Env.,Soc.,Gov.,"GDP growth",GDP,Profit,Assets, Revenue, RnD,Liabilities,SE,Leverage,"Debt ratio")

df <- df[df$`Company name` !="ENAGAS SA",]


theme_set(theme_classic() +
            theme(legend.position = "bottom",
                  legend.box.spacing = unit(-0.4, "cm"),
                  legend.margin = margin(6, 6, 6, 6),
                  legend.title = element_blank(),
            ))


# Summarizing the tec data
tecplot <- df %>% select(Year, Region, TEC) %>% group_by(Year, Region) %>% 
  summarise(tec_mean = round(mean(TEC, na.rm = TRUE), 2))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region, ignore.case = TRUE) ~"North America"))


# Plotting TEC

p <- ggplot(tecplot, mapping = aes(x = Year, y = tec_mean/100000*1)) +
  geom_line(aes(color = Region), lwd = 1.5) +
  ylim(-3800, -80000) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  labs(x = "", y = "TEC / 100,000", title = "Yearly average TEC for Europe and North America", subtitle = "Lower TEC is worse. Note that the y-axis is flipped.")

p

# Summarizing the tei data
teiplot <- df %>% select(Year, Region, TEI) %>% group_by(Year, Region) %>% 
  summarise(tei_mean = round(mean(TEI, na.rm = TRUE), 2))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region, ignore.case = TRUE) ~"North America"))

# Plotting TEI

p <- ggplot(teiplot, mapping = aes(x = Year, y = tei_mean)) +
  geom_line(aes(color = Region), lwd = 2) + 
  ylim(-0.0152, -0.32) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018,2019)) +
  labs(x = "", y = "TEI", title = "Yearly average TEI for Europe and North America", subtitle = "Lower TEI is worse. Note that the y-axis is flipped.") 
p


# Summarizing revenue data
revmean <- df %>% select(Year, Region, Revenue) %>% group_by(Year, Region) %>% 
  summarise(rev_mean = round(mean(Revenue, na.rm = TRUE)))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region) ~ "North America"))

# Plotting revenue

ggplot(revmean, mapping = aes(x = Year, y = rev_mean/1000)) +
  geom_line(aes(color = Region), lwd = 2) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Revenue in $million", title = "Average revenue for European and North American firms", subtitle = "Revenue is in millions of US Dollars") +
  scale_y_continuous(limits = c(2850, 50000), breaks = c(0,10000, 20000, 30000, 40000, 50000)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2019))

# Plotting ESG

esg <- df %>% select(Year, Region, ESG) %>% group_by(Year, Region) %>% 
  summarise(esg_mean = round(mean(ESG, na.rm = TRUE), 2))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region) ~"North America"))

esg <- ggplot(esg, mapping = aes(x = Year, y = esg_mean)) +
  geom_line(aes(color = Region), lwd = 1.5) + 
  ylim(0.38, 8) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Combined ESG", title = "Average Combined ESG", subtitle = "Incl. ESG Controversies score") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2019))

esg

esgenv <- df %>% select(Year, Region, Env.) %>% group_by(Year, Region) %>% 
  summarise(env_mean = round(mean(Env., na.rm = TRUE), 2))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region) ~"North America"))

esgenv <- ggplot(esgenv, mapping = aes(x = Year, y = env_mean)) +
  geom_line(aes(color = Region), lwd = 1.5) + 
  ylim(0.38, 8) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Environmental Score", title = "Average Environmental", subtitle = "") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2019))
esgenv

esgsoc <- df %>% select(Year, Region, Soc.) %>% group_by(Year, Region) %>% 
  summarise(soc_mean = round(mean(Soc., na.rm = TRUE), 2))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region) ~"North America"))

esgsoc <- ggplot(esgsoc, mapping = aes(x = Year, y = soc_mean)) +
  geom_line(aes(color = Region), lwd = 1.5) + 
  ylim(0.42, 9) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Social Score", title = "Average Social", subtitle = "") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2019))
esgsoc


esggov <- df %>% select(Year, Region, Gov.) %>% group_by(Year, Region) %>% 
  summarise(gov_mean = round(mean(Gov., na.rm = TRUE), 2))  %>%  
  mutate(Region = case_when(grepl("0", Region) ~ "Europe",
                            grepl("1", Region) ~"North America"))

esggov <- ggplot(esggov, mapping = aes(x = Year, y = gov_mean)) +
  geom_line(aes(color = Region), lwd = 1.5) + 
  ylim(0.42, 9) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  labs(x = "", y = "Governance Score", title = "Average Governance", subtitle = "") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2019))
esggov


### Plotting ESG in one

ggarrange(ggarrange(esg, esgenv, ncol = 2, labels = c("A", "B")),                                                 
          ggarrange(esgsoc, esggov, ncol = 2, labels = c("C", "D")), 
          nrow = 2                                       
)


########## Ignore (logistic) ########

df2 <- df %>% drop_na(TEI)

model = glm(Region ~ TEI, data = df2, family = binomial)
summary(model)

range <- data.frame(TEI = seq(from=min(df2$TEI), to=max(df2$TEI), by=.01))

range$Region <- predict(model, newdata=range, type = "response")



ggplot(range, aes(x = TEI, y = Region)) +
  geom_line()

model2 <- lm(TEI ~ Region, data = df2)
summary(model2)

ggplot(model2, aes(x = Region, y = )) +
  geom_line()

############ Very newest data! ###########
df <- read_csv("C:/Users/frede/Desktop/Ting/CBS/Data/thesis/Thesis sheets - Main data (1).csv",
               col_types = cols(.default = col_guess()
               )) %>% slice(1:920)

# rename for data manipulation and select variabels
colnames(df)[1] <- "year"; colnames(df)[2] <- "name"; colnames(df)[3] <- "country"; colnames(df)[4] <- "region"; colnames(df)[5] <- "industry"; colnames(df)[8] <- "tei"; colnames(df)[9] <- "tec"; colnames(df)[19] <- "esgcomb"; colnames(df)[20] <- "env"; colnames(df)[21] <- "soc"; colnames(df)[24] <- "gdpgrowth"; colnames(df)[25] <- "gdppercap"; colnames(df)[26] <- "profits"; colnames(df)[27] <- "assets"; colnames(df)[28] <- "revenue"; colnames(df)[29] <- "rnd"; colnames(df)[30] <- "liabilities"; colnames(df)[31] <- "se"; colnames(df)[32] <- "leverage"; colnames(df)[33] <- "debtratio";colnames(df)[22] <- "gov"

df <- df %>% select(year, name, country, region, industry, tei, tec, esgcomb, env,soc,gov,gdpgrowth,gdppercap,profits,assets, revenue, rnd,liabilities,se,leverage,debtratio)

df <- df[c(1,2,3,5,4,7,6,8:13,20,21,17,14,15,16,18,19)]

colnames(df)[1] <- "Year";colnames(df)[2] <- "Company";colnames(df)[3] <- "Country";colnames(df)[4] <- "Industry";colnames(df)[5] <- "Region";colnames(df)[7] <- "TEI";colnames(df)[6] <- "TEC";colnames(df)[12] <- "GDP growth";colnames(df)[13] <- "GDP";colnames(df)[14] <- "Leverage";colnames(df)[15] <- "Debt ratio";colnames(df)[17] <- "Profit";colnames(df)[18] <- "Assets";colnames(df)[19] <- "Revenue";colnames(df)[16] <- "RnD";colnames(df)[20] <- "Liabilities";colnames(df)[21] <- "SE";colnames(df)[8] <- "ESG";colnames(df)[9] <- "Env.";colnames(df)[11] <- "Gov.";colnames(df)[10] <- "Soc."

attach(df)

df <- df %>% mutate(Size = log(Assets))
df <- df %>% mutate(SE_log = log(SE))
df <- df %>% mutate(Profitability = Profit/Assets)
df <- df %>% mutate(GDP_log = log(GDP))
df <- df %>% mutate(RnD_int = RnD/Assets)
df <- df %>% mutate(Revenue_log = log(Revenue))
df <- df %>% mutate(Liabilities_log = log(Liabilities))

################ Newest sum.stat and cor.mat ################

# Sumstat
library(stargazer)

df11 <- df[c(5,7:12,15,22,24:26)]

colnames(df11)[8] <- "Leverage"; colnames(df11)[11] <- "GDP";colnames(df11)[12] <- "RnD"

df11 <- df11[c(1:6,9,10,8,12,11,7)]

stargazer(as.data.frame(df11[c(2:12)]), digits.extra = 0, summary.stat = c("n", "median", "mean", "sd", "min", "max"))


# Cormat
cormat1 <- cor_mat(df11[,1:12], method = "pearson") %>% pull_lower_triangle(diagonal = FALSE)

var_names2 <- colnames(cormat1[,2:12])

cormat1 <- cormat1[-1,-13]

kbl(cormat1[,-13], booktabs = T, linesep = "",
    align = c("l", rep("c",15)),
    col.names = c("",var_names2),
    row.names = F) %>%
  kable_styling(font_size = 8) %>%
  landscape() %>%
  kable_styling()









