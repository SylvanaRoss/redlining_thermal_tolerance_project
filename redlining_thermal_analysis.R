##redlining_thermal_tolerance_project 

#Packages 
install.packages("multcomp")

#Master Library 
library(tidyverse)
library(readxl)
library(multcomp)
library(lme4)
library(emmeans)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)

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
#Differences against the baseline "N", show that C and D are significantly higher than N, B is borderline, and A is not significantly different than N. 

##4. Linear Mixed Model for both cities combined 
#library(lme4)
meta_lmm_model <- lmer(avg_ct ~ holc + (1 | city), data = clean_ant_data)
summary(meta_lmm_model)
#used the random intercept for each city in which the variance is very small (.0072). Which means the average thermal tolerance differs very little between cities, once holc is accountanted for. 
#the residual variance = .295 which means most of the variation is within cities, between samples, so city doesn't contribute to much extra variability. 
# This also shows the pattern that thermal tolerance increases with worse HOLC grades. Which means holc has a strong significant effect on avg_ct. Also, the city contributes almost no additional variance, which means most of the variation is explained by holc, not which city the samples come from. 







##B. LINEAR REGRESSION FOR BALTIMORE ONLY 
#1.Running linear regression on Baltimore only samples  
#Running linear regression on holc to average CTmax value for BALTIMORE compared to the reference level holc "N" (natural). The coefficients show the difference in avg_ct for each HOLC grade compared to N (natural sites) with only samples from Baltimore 
balt_thermal_model <- lm(avg_ct ~ holc, data = clean_ant_data[clean_ant_data$city == "Baltimore", ])
summary(balt_thermal_model)

#2. ANOVA and Turkey Post Hoc Test on BALTIMORE samples 
balt_thermal_anova <- anova(balt_thermal_model)
print(balt_thermal_anova)
# the anova shows a highly significant effect (p = 2.55e-07). 

#Tukey's HSD
balt_thermal_tukey <- glht(balt_thermal_model, linfct = mcp(holc = "Tukey"))
summary(balt_thermal_tukey)
confint(balt_thermal_tukey)
#within BALTIMORE, C-N, D-N, B-A, C-A, D-A are all significantly different in avg_ct from each other (first letter is higher than the second letter). 
#A -N, B-N, C-B, D-B, D-C are not significantly different than each other 
#A is significantly lower than B, C, and D but not from N 
# B is significantly different than A (barley) but not significantly different than N, C, or D. 
# C and D are not significantly different from each other but they are from A and N 



##C. LINEAR REGRESSION FOR PHILADELPHIA ONLY 
#1.Running linear regression on PHILADELPHIA only samples  
#Running linear regression on holc to average CTmax value for PHILADELPHIA compared to the reference level holc "N" (natural). The coefficients show the difference in avg_ct for each HOLC grade compared to N (natural sites) with only samples from philadelphia 
phily_thermal_model <- lm(avg_ct ~ holc, data = clean_ant_data[clean_ant_data$city == "Philadelphia", ])
summary(phily_thermal_model)

#2. ANOVA and Turkey Post Hoc Test on BALTIMORE samples 
phily_thermal_anova <- anova(phily_thermal_model)
print(phily_thermal_anova)
#anova shows significant effect of holc on avg_ct (p=3.69e-08). 

#Tukey's HSD
phily_thermal_tukey <- glht(phily_thermal_model, linfct = mcp(holc = "Tukey"))
summary(phily_thermal_tukey)
confint(phily_thermal_tukey)
#within BALTIMORE, C-N, D-N, B-A, C-A, D-A are all significantly different in avg_ct from each other (first letter is higher than the second letter). 
#A -N, B-N, C-B, D-B, D-C are not significantly different than each other 
#A is significantly lower than B, C, and D but not from N 
# B is significantly different than A (barley) but not significantly different than N, C, or D. 
# C and D are not significantly different from each other but they are from A and N 






###3. VISUALIZATION: BOX PLOTS AND HEAT MAPS - based on linear regression analysis ###

#3A. BOX PLOTS 

##A. Box plot for BOTH cities combined
#library(ggplot2)
ggplot(clean_ant_data, aes(x = `holc`, y =`avg_ct`, fill = `holc`)) +
  geom_boxplot() +
  scale_fill_manual(values = c("A" = "forestgreen", 
                               "B" = "steelblue", 
                               "C" = "gold", 
                               "D" = "firebrick", 
                               "N" = "plum")) +
  labs(x = "HOLC Neighborhood Grade", y = "Average Thermal Tolerance per Colony (°C)",
       title = "Thermal Tolerance Averages of Tapinoma sessile Colonies by HOLC Neighborhood Grade ") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )



##B. Box plot for BALTIMORE
ggplot(clean_ant_data %>% filter(city == "Baltimore"),
       aes(x = holc, y = avg_ct, fill = holc)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "A" = "forestgreen",
    "B" = "steelblue",
    "C" = "gold",
    "D" = "firebrick",
    "N" = "plum"
  )) +
  labs(
    x = "HOLC Neighborhood Grade",
    y = "Average Thermal Tolerance per Colony (°C)",
    title = "Thermal Tolerance Averages of Tapinoma sessile Colonies in Baltimore by HOLC Neighborhood Grade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )


##C. Box plot for PHILADELPHIA 
ggplot(clean_ant_data %>% filter(city == "Philadelphia"),
       aes(x = holc, y = avg_ct, fill = holc)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "A" = "forestgreen",
    "B" = "steelblue",
    "C" = "gold",
    "D" = "firebrick",
    "N" = "plum"
  )) +
  labs(
    x = "HOLC Neighborhood Grade",
    y = "Average Thermal Tolerance per Colony (°C)",
    title = "Thermal Tolerance Averages of Tapinoma sessile Colonies in Philadelphia by HOLC Neighborhood Grade"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )





##3B. HEAT MAPS ##

##A. Heat map of p-values from Tukey's, showing only the upper Triangle
#library(emmeans)
#library(dplyr)
#library(tidyr)
#library(reshape2)
#library(ggplot2)
#library(scales)


#Heat maps for BOTH cities combined##

## Pairwise comparisons
meta_pairs <- pairs(emmeans(meta_thermal_model, ~ `holc`))
meta_pairs_summary <- as.data.frame(summary(meta_pairs))


## Clean and separate the contrasts (e.g., "A - B" → "A", "B") into group1 and group2
#libary(tidyr)
meta_pvals_df <- meta_pairs_summary %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(p.value = as.numeric(p.value))

## Create list of all grades
holc_grades <- sort(unique(c(meta_pvals_df$group1, meta_pvals_df$group2)))

## Initialize symmetric matrix
meta_matrix <- matrix(1, nrow = length(holc_grades), ncol = length(holc_grades),
                     dimnames = list(holc_grades, holc_grades))


## Fill heat map with p-values
for (i in seq_len(nrow(meta_pvals_df))) {
  g1 <- meta_pvals_df$group1[i]
  g2 <- meta_pvals_df$group2[i]
  p <- meta_pvals_df$p.value[i]
  meta_matrix[g1, g2] <- p
  meta_matrix[g2, g1] <- p
}
diag(meta_matrix) <- NA

## Convert to long format
meta_melted <- melt(meta_matrix, na.rm = TRUE) 

#library(ggplot2)
#library(scales)

# Assume meta_melted has Var1, Var2, value (p-values) and includes all grades
#holc_grades <- c("A", "B", "C", "D", "N")
meta_melted$Var1 <- factor(meta_melted$Var1, levels = holc_grades)
meta_melted$Var2 <- factor(meta_melted$Var2, levels = holc_grades)


# Filter upper triangle only
meta_melted_upper <- meta_melted %>%
  filter(as.numeric(Var1) < as.numeric(Var2))


##use a transformed scale like -log10(p-value) for the heat map to spread out the small p-values and make marginal differences more visually distinct 
ggplot(meta_melted_upper, aes(x = Var1, y = Var2, fill = -log10(value))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(value < 0.0001, "<.0001",
                               sprintf("%.3f", value))),
            color = "black", size = 4) +
  scale_fill_viridis_c(option = "cividis", direction = -1, 
                       name = expression(-log[10](p))) +
  labs(
    title = "Pairwise Tukey Post Hoc p-values (CTmax) Between HOLC Grades",
    x = "HOLC Grade",
    y = "HOLC Grade"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )




##Heat map for BALTIMORE##
## Pairwise comparisons
balt_pairs <- pairs(emmeans(balt_thermal_model, ~ `holc`))
balt_pairs_summary <- as.data.frame(summary(balt_pairs))

## Clean and separate the contrasts (e.g., "A - B" → "A", "B") into group1 and group2
#libary(tidyr)
balt_pvals_df <- balt_pairs_summary %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(p.value = as.numeric(p.value))

## Create list of all grades. I kept holc_grades from the combined data
#holc_grades <- sort(unique(c(meta_pvals_df$group1, meta_pvals_df$group2)))

## Initialize symmetric matrix
balt_matrix <- matrix(1, nrow = length(holc_grades), ncol = length(holc_grades),
                      dimnames = list(holc_grades, holc_grades))

## Fill heat map with p-values
for (i in seq_len(nrow(balt_pvals_df))) {
  g1 <- balt_pvals_df$group1[i]
  g2 <- balt_pvals_df$group2[i]
  p <- balt_pvals_df$p.value[i]
  balt_matrix[g1, g2] <- p
  balt_matrix[g2, g1] <- p
}
diag(balt_matrix) <- NA

## Convert to long format
balt_melted <- melt(balt_matrix, na.rm = TRUE) 

#library(ggplot2)
#library(scales)

# Assume meta_melted has Var1, Var2, value (p-values) and includes all grades
#holc_grades <- c("A", "B", "C", "D", "N")
balt_melted$Var1 <- factor(balt_melted$Var1, levels = holc_grades)
balt_melted$Var2 <- factor(balt_melted$Var2, levels = holc_grades)


# Filter upper triangle only
balt_melted_upper <- balt_melted %>%
  filter(as.numeric(Var1) < as.numeric(Var2))

##use a transformed scale like -log10(p-value) for the heat map to spread out the small p-values and make marginal differences more visually distinct 
ggplot(balt_melted_upper, aes(x = Var1, y = Var2, fill = -log10(value))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(value < 0.0001, "<.0001",
                               sprintf("%.3f", value))),
            color = "black", size = 4) +
  scale_fill_viridis_c(option = "cividis", direction = -1, 
                       name = expression(-log[10](p))) +
  labs(
    title = "Pairwise Tukey Post Hoc p-values (CTmax) Between HOLC Grades in Baltimore",
    x = "HOLC Grade",
    y = "HOLC Grade"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )



##Heat map for PHILADELPHIA##
## Pairwise comparisons
phily_pairs <- pairs(emmeans(phily_thermal_model, ~ `holc`))
phily_pairs_summary <- as.data.frame(summary(phily_pairs))

## Clean and separate the contrasts (e.g., "A - B" → "A", "B") into group1 and group2
#libary(tidyr)
phily_pvals_df <- phily_pairs_summary %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(p.value = as.numeric(p.value))

## Create list of all grades. I kept holc_grades from the combined data
#holc_grades <- sort(unique(c(meta_pvals_df$group1, meta_pvals_df$group2)))

## Initialize symmetric matrix
phily_matrix <- matrix(1, nrow = length(holc_grades), ncol = length(holc_grades),
                      dimnames = list(holc_grades, holc_grades))

## Fill heat map with p-values
for (i in seq_len(nrow(phily_pvals_df))) {
  g1 <- phily_pvals_df$group1[i]
  g2 <- phily_pvals_df$group2[i]
  p <- phily_pvals_df$p.value[i]
  phily_matrix[g1, g2] <- p
  phily_matrix[g2, g1] <- p
}
diag(phily_matrix) <- NA

## Convert to long format
phily_melted <- melt(phily_matrix, na.rm = TRUE) 

#library(ggplot2)
#library(scales)

# Assume meta_melted has Var1, Var2, value (p-values) and includes all grades
#holc_grades <- c("A", "B", "C", "D", "N")
phily_melted$Var1 <- factor(phily_melted$Var1, levels = holc_grades)
phily_melted$Var2 <- factor(phily_melted$Var2, levels = holc_grades)


# Filter upper triangle only
phily_melted_upper <- phily_melted %>%
  filter(as.numeric(Var1) < as.numeric(Var2))

##use a transformed scale like -log10(p-value) for the heat map to spread out the small p-values and make marginal differences more visually distinct 
ggplot(phily_melted_upper, aes(x = Var1, y = Var2, fill = -log10(value))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(value < 0.0001, "<.0001",
                               sprintf("%.3f", value))),
            color = "black", size = 4) +
  scale_fill_viridis_c(option = "cividis", direction = -1, 
                       name = expression(-log[10](p))) +
  labs(
    title = "Pairwise Tukey Post Hoc p-values (CTmax) Between HOLC Grades in Philadelphia",
    x = "HOLC Grade",
    y = "HOLC Grade"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )


