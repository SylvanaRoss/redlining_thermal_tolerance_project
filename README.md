# redlining_thermal_tolerance_project
A study looking at the impact of redlining on the morphology and physiology of the ant species Tapinoma sessile. Comparisons of thermal tolerance and body size with ant populations in historically redlined neighborhoods of Baltimore, Maryland and Philadelphia, PA. The thermal tolerance and body sizes will be compared across different redlined neighborhoods to identify neighborhood specific and city specific patterns. 



#Packages 
install.packages("multcomp")
#Master Library 
library(tidyverse)
library(readxl)
library(multcomp)


#check data sheet
#library(readxl)
raw_ant_data <- read_excel("data/thermal_meta_raw.xlsx")
head(raw_ant_data)

#Convert 'city' and 'holc' to factors instead of charecters 
raw_ant_data$city <- as.factor(raw_ant_data$city)
raw_ant_data$holc <- as.factor(raw_ant_data$holc)

#check 'city' and 'holc' are factors 
head(raw_ant_data) 



##1. DATA SUMMARY: Collection of the total number of samples per holc grade and per city

#Total number of Tapinoma sessile colonies collected per city 
raw_ant_data %>% 
  filter(genus == "Tapinoma") %>% 
  count(city)

#Total number of Tainoma collected per holc grade for both cities combined 
raw_ant_data %>%
  count(holc)

#Total number of Tapinoma collected per each holc grade for each city 
raw_ant_data %>%
  group_by(city, holc) %>%
  summarise(n = n())



###2. LINEAR REGRESSION ANALYSIS ###

##A. LINEAR REGRESSION ANALYSIS FOR BOTH CITIES COMBINED ##

#1. Identify and eliminate outliers from meta dataset 
#calculate the IQR: removing rowns in 'avg_ct' that are more than 1.5 x IQR above the third quartile or below the first quartile. 
iqr <- IQR(raw_ant_data$avg_ct, na.rm = TRUE)
q1 <- quantile(raw_ant_data$avg_ct, 0.25, na.rm = TRUE)
q3 <- quantile(raw_ant_data$avg_ct, 0.75, na.rm = TRUE)

# Define lower and upper bounds
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Filter out outliers
clean_ant_data <- raw_ant_data[raw_ant_data$avg_ct >= lower_bound & raw_ant_data$avg_ct <= upper_bound, ]

#There are no outliers. We will continue with the clean_ant_data spreadsheet though.


# 2. Running linear regression on holc to average CTmax value for both cities combined compared to the reference level holc "A"
meta_thermal_model <- lm(avg_ct ~ holc, data = clean_ant_data)

#view summary of meta model
summary(meta_thermal_model)

#Running linear regression on holc to average CTmax value for both cities combined compared to the reference level holc "N" (natural). The coefficients show the difference in avg_ct for each HOLC grade compared to N (natural sites)
clean_ant_data$holc <- relevel(clean_ant_data$holc, ref = "N")
meta_thermal_model <- lm(avg_ct ~ holc, data = clean_ant_data)
summary(meta_thermal_model)


#3. Run ANOVA and Turkey Post Hoc Test on city combined data 

#ANOVA 
meta_thermal_anova <- anova(meta_thermal_model)
print(meta_thermal_anova)

#the ANOVA shows a significant effect of 'holc' on 'avg_ct' (p= 6.73e-15). There is a difference somewhere among the HOLC grades, but we don;t know which pairs of HOLC grades differ

#Tukey's HSD
#library(multcomp)
meta_thermal_tukey <- glht(meta_thermal_model, linfct = mcp(holc = "Tukey"))
summary(meta_thermal_tukey)
confint(meta_thermal_tukey)
#Tukey compared all pairs of holc grades against each other and also against the reference "N".
#KEY RESULTS##
#A-N, B-N (marginal), C-B, and D-C are not significantly different because their confidence intervals include 0 or p > .05 



=======
>>>>>>> a9697ae5c07e3638edeec4b4940c5a0d8013949a
