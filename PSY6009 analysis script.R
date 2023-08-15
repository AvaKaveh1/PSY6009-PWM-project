# ---------Descriptives of Time 1 data set 
# Import T1 data named "Prototype_Willingness_model_and_Reuse_July_22_2023_05_04"
#change data set name to df
df <- Prototype_Willingness_model_and_Reuse_July_22_2023_05_04


# Number of female and male participants
# Rename the column "Q3" to "gender" in the dataset
colnames(df)[colnames(df) == "Q3"] <- "gender"
Gender <- table(df$gender)
head(Gender)

# Mean age
# Rename the column "Q4" to "age" in the dataset
colnames(df)[colnames(df) == "Q4"] <- "age"

# Calculate the mean of the 'age' column
# Convert "age" column to numeric
df$age <- as.numeric(df$age)

# Calculate the mean age
mean_value <- mean(df$age, na.rm = TRUE)

# Print the mean value
print(mean_value)

# Calculate age range
# Calculate the minimum and maximum ages
min_age <- min(df$age, na.rm = TRUE)
max_age <- max(df$age, na.rm = TRUE)
print(min_age)
print(max_age)

# Rename the column Q5 to ethnicity and calculate mean
colnames(df)[colnames(df) == "Q5"] <- "ethnicity"
ethnicity_counts <- table(df$ethnicity, useNA = "always")

# Print the counts
print(ethnicity_counts)

#------ Data wrangaling----


# Convert items to numeric variable
df$Q7 <- as.numeric(df$Q7)
df$Q37 <- as.numeric(df$Q37)
df$Q9 <- as.numeric(df$Q9)
df$Q38 <- as.numeric(df$Q38)
df$Q11 <- as.numeric(df$Q11)
df$Q39 <- as.numeric(df$Q39)
df$Q13 <- as.numeric(df$Q13)
df$Q40 <- as.numeric(df$Q40)
df$Q15 <- as.numeric(df$Q15)
df$Q41 <- as.numeric(df$Q41)
df$Q17 <- as.numeric (df$Q17)
df$Q42 <- as.numeric(df$Q42)
df$Q19 <- as.numeric (df$Q19)
df$Q43 <- as.numeric(df$Q43)
df$Q21 <- as.numeric (df$Q21)
df$Q44 <- as.numeric(df$Q44)
df$Q23 <- as.numeric (df$Q23)
df$Q45 <- as.numeric(df$Q45)
df$Q25 <- as.numeric (df$Q25)
df$Q46 <- as.numeric(df$Q46)
df$Q29 <- as.numeric (df$Q29)
df$Q47 <- as.numeric(df$Q47)
df$Q48 <- as.numeric (df$Q48)
df$Q49 <- as.numeric(df$Q49)
df$Q50 <- as.numeric(df$Q50)

#reverse code attitude items 
reverse_cols = c("Q9", "Q38","Q11", "Q39")
df [, reverse_cols] = 8 - df[, reverse_cols]

#Import T2 data set 
# Rename data set to dft2
dft2 <- Time_2_Prototype_Willingness_Model_July_27_2023_08_37

#Make variables T2 data set variables numeric 
dft2$Q1 <- as.numeric(dft2$Q1)
dft2$Q2 <- as.numeric (dft2$Q2)
dft2$Q3 <- as.numeric(dft2$Q3)
dft2$Q4 <- as.numeric(dft2$Q4)


#Name the T2 variables of behaviour and opportunity 

colnames(dft2)[colnames(dft2) == "Q1"] <- "oppsell"
colnames(dft2)[colnames(dft2) == "Q2"] <- "oppbuy"
colnames(dft2)[colnames(dft2) == "Q3"] <- "behav1"
colnames(dft2)[colnames(dft2) == "Q4"] <- "behav2"

# Merge Time 1 data set with Time 2 based off email addresses (Q36 and Q7)
# Assuming df and df2 have the columns "Q36" and "Q7" respectively
merged_data <- merge(x = df, y = dft2, by.x = "Q36", by.y = "Q7")

# Make sure attitude items are still reversed coded in the new merged df
reverse_cols <- c("Q9", "Q38", "Q11", "Q39")
merged_data[, reverse_cols] <- 8 - merged_data[, reverse_cols]

# Make time 2 variables numeric
merged_data$oppsell <- as.numeric(merged_data$oppsell)
merged_data$oppbuy <- as.numeric(merged_data$oppbuy)
merged_data$behav1 <- as.numeric(merged_data$behav1)
merged_data$behav2 <- as.numeric(merged_data$behav2)

#Rename all Q columns to reflect the item measures and create a new data set with all required variables
install.packages("dplyr")
library(dplyr)
finalpwmdf <- merged_data %>%
  rename(att1 = Q7,
         att2 = Q37,
         att3 = Q9,
         att4 = Q38,
         att5 = Q11,
         att6 = Q39,
         norm1 = Q13,
         norm2 = Q40,
         norm3 = Q15,
         norm4 = Q41,
         pbc1 = Q17,
         pbc2 = Q42,
         pastb1 = Q19,
         pastb2 = Q43,
         int1 = Q21,
         int2 = Q44,
         will1 = Q23,
         will2 = Q45,
         sim1 = Q46,
         sim2 = Q25,
         fav1 = Q29,
         fav2 = Q47,
         fav3 = Q48,
         fav4 = Q49,
         fav5 = Q50) %>%
  select(att1, att2, att3, att4, att5, att6, norm1, norm2, norm3, norm4,
         pbc1, pbc2, pastb1, pastb2, int1, int2, will1, will2, sim1, sim2,
         fav1, fav2, fav3, fav4, fav5, oppbuy, oppsell, behav1, behav2)
finalpwmdf <- finalpwmdf[complete.cases(finalpwmdf), ]


# SEM analysis 
# Install and load the Lavaan package
install.packages("lavaan")
library(lavaan)

# Measurement model- CFA
# Specify the modified CFA model
model <- '
  # Latent constructs
  attitudes =~ att1 + att2 + att3 + att4 + att5 + att6
  norms =~ norm1 + norm2 + norm3 + norm4
  pbc =~ pbc1 + pbc2
  pastbehaviour =~ pastb1 + pastb2
  intention =~ int1 + int2
  willingness =~ will1 + will2
  prototype_similarity =~ sim1 + sim2
  prototype_favourability =~ fav1 + fav2 + fav3 + fav4 + fav5
  opportunity_to_sell =~ oppsell
  opportunity_to_buy =~ oppbuy
  reuse_behaviour =~ behav1 + behav2
'
fit <- cfa(model, data=finalpwmdf)

# Summarize the CFA results
summary(fit, fit.measures= TRUE)
mod_indices <- modificationIndices(fit)

# Print the modification indices
print(mod_indices)
head(mod_indices[order(mod_indices$mi, decreasing = TRUE), ], 10)

loadings <- as.data.frame(inspect(fit, "std")$loadings)
factor_scores <- as.data.frame(fitted(fit))

# Specify the structural paths in the model
structural_model <- '  # Structural paths
  intention ~ attitudes + norms + pbc + pastbehaviour
  willingness ~ prototype_favourability + prototype_similarity
  reuse_behaviour ~ intention + willingness + opportunity_to_buy + opportunity_to_sell
'

# Combine the measurement and structural models
full_model <- paste0(model, structural_model)

# Fit the full model (CFA + Structural)
fit_full <- sem(full_model, data = finalpwmdf)

# Summarize the full model results
summary(fit_full, fit.measures = TRUE)
inspect(fit_full, 'r2')

# % of variance accounted for intention, willingness and reuse behaviour
inspect(fit_full, 'r2')

# New measurement model with the consideration of the modification indices output 
modelupdate <- '
# Latent constructs
attitudes =~ att1 + att2 + att3 + att4 + att5 + att6
norms =~ norm1 + norm2 + norm3 + norm4
pbc =~ pbc1 + pbc2
pastbehaviour =~ pastb1 + pastb2
intention =~ int1 + int2
willingness =~ will1 + will2
prototype_similarity =~ sim1 + sim2
prototype_favourability =~ fav1 + fav2 + fav3 + fav4 + fav5
opportunity_to_sell =~ oppsell
opportunity_to_buy =~ oppbuy
reuse_behaviour =~ behav1 + behav2
norm3 ~~norm4
norm1 ~~ norm2
att1 ~~ att2
att3 ~~ att4
'
fit <- cfa(modelupdate, data=finalpwmdf)
summary(fit, fit.measures= TRUE)


