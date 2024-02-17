daF3547 <- read.csv2("~/Biostatistics/daF3547.csv", header=TRUE)
names(daF3547)
class(daF3547)
head(daF3547) # Display the first few rows of the data
str(daF3547) # Display the structure of the data frame
View(daF3547)
summary(daF3547)
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(car) # for regression analysis

# Subset the data for selected variables
selected_data <- daF3547 %>% dplyr::select(q27, q28, q29, q30, q31, q32, q33, q39, q40,
                                           q42, survey)

# Summary statistics for selected variables
summary(selected_data)

# Frequency table for demographic variables
print("Frequency Table for Restriction (q27):")
print(table(daF3547$q27))
print("Frequency Table for Restriction (q28):")
print(table(daF3547$q28))
print("Frequency Table for Restriction (q29):")
print(table(daF3547$q29))
print("Frequency Table for Restriction (q30):")
print(table(daF3547$q30))
print("Frequency Table for Restriction (q31):")
print(table(daF3547$q31))
print("Frequency Table for Restriction (q32):")
print(table(daF3547$q32))
print("Frequency Table for Restriction (q33):")
print(table(daF3547$q33))
print("Frequency Table for Age Group (q39):")
print(table(daF3547$q39))
print("Frequency Table for Gender (q40):")
print(table(daF3547$q40))
print("Frequency Table for Survey Response:")
print(table(daF3547$survey))

#Distribution Plots and Shapiro-Wilk Tests:
hist(selected_data$q27)
qqnorm(selected_data$q27)
qqline(selected_data$q27)
boxplot(selected_data$q27)
shapiro.test(selected_data$q27)
hist(selected_data$q28)
qqnorm(selected_data$q28)
qqline(selected_data$q28)
boxplot(selected_data$q28)
shapiro.test(selected_data$q28)
hist(selected_data$q29)
qqnorm(selected_data$q29)
qqline(selected_data$q29)
boxplot(selected_data$q29)
shapiro.test(selected_data$q29)
hist(selected_data$q30)
qqnorm(selected_data$q30)
qqline(selected_data$q30)
boxplot(selected_data$q30)
shapiro.test(selected_data$q30)
hist(selected_data$q31)
qqnorm(selected_data$q31)
qqline(selected_data$q31)
boxplot(selected_data$q31)
shapiro.test(selected_data$q31)
hist(selected_data$q32)
qqnorm(selected_data$q32)
qqline(selected_data$q32)
boxplot(selected_data$q32)
shapiro.test(selected_data$q32)
hist(selected_data$q33)
qqnorm(selected_data$q33)
qqline(selected_data$q33)
boxplot(selected_data$q33)
shapiro.test(selected_data$q33)
hist(selected_data$q39)
qqnorm(selected_data$q39)
qqline(selected_data$q39)
boxplot(selected_data$q39)
shapiro.test(selected_data$q39)
hist(selected_data$q40)
qqnorm(selected_data$q40)
qqline(selected_data$q40)
boxplot(selected_data$q40)
shapiro.test(selected_data$q40)
hist(selected_data$q42)
qqnorm(selected_data$q42)
qqline(selected_data$q42)
boxplot(selected_data$q42)
shapiro.test(selected_data$q42)
hist(selected_data$survey)
qqnorm(selected_data$survey)
qqline(selected_data$survey)
boxplot(selected_data$survey)
shapiro.test(selected_data$survey)

#Data Analysis:
# Perform chi-square test for categorical variables (age, gender, restrictions, survey)
chi_square_age <- chisq.test(selected_data$q39, selected_data$q42)
chi_square_gender <- chisq.test(selected_data$q40, selected_data$q42)
chi_square_restriction_1 <- chisq.test(selected_data$q27, selected_data$q42)
chi_square_restriction_2 <- chisq.test(selected_data$q28, selected_data$q42)
chi_square_restriction_3 <- chisq.test(selected_data$q29, selected_data$q42)
chi_square_restriction_4 <- chisq.test(selected_data$q30, selected_data$q42)
chi_square_restriction_5 <- chisq.test(selected_data$q31, selected_data$q42)
chi_square_restriction_6 <- chisq.test(selected_data$q32, selected_data$q42)
chi_square_restriction_7 <- chisq.test(selected_data$q33, selected_data$q42)
chi_square_survey <- chisq.test(selected_data$survey, selected_data$q42)
chi_square_results <- data.frame(
  Variable = c("Age", "Gender", "Restriction_1", "Restriction_2", "Restriction_3",
               "Restriction_4", "Restriction_5", "Restriction_6", "Restriction_7", "Survey"),
  P_Value = c(chi_square_age$p.value, chi_square_gender$p.value,
              chi_square_restriction_1$p.value, chi_square_restriction_2$p.value,
              chi_square_restriction_3$p.value, chi_square_restriction_4$p.value,
              chi_square_restriction_5$p.value, chi_square_restriction_6$p.value,
              chi_square_restriction_7$p.value, chi_square_survey$p.value)
)
library(knitr)

# Print the Chi-Square test results as Table-1
kable(chi_square_results,
      col.names = c("Demographic Variable", "P-Value"),
      caption = "Table-1: Chi-Square Test Result for Demographic Variables with Gaming
Behavior",
      align = "c")

# Perform regression analysis
regression_model <- lm(q42 ~ q27 + q28 + q29 + q30 + q31 + q32 + q33 + q39 + q40 +
                         survey, data = selected_data)

# Store regression summary results in a data frame
regression_results <- summary(regression_model)$coefficients

# Extract coefficients, standard errors, t-values, and p-values
coefficients <- regression_results[, 1]
standard_errors <- regression_results[, 2]
t_values <- regression_results[, 3]
p_values <- regression_results[, 4]


# Create a data frame for regression analysis results
regression_table <- data.frame(
  Variable = c("Intercept","Restriction_1", "Restriction_2", "Restriction_3", "Restriction_4",
               "Restriction_5", "Restriction_6", "Restriction_7",
               "Age", "Gender", "Survey"),
  Coefficient = coefficients,
  SE = standard_errors,
  T_Value = t_values,
  P_Value = p_values
)

# Print the regression analysis results as Table-2
kable(regression_table,
      col.names = c("Variable", "Coefficient", "SE", "T-Value", "P-Value"),
      caption = "Table-2: Regression Analysis Results for Gaming Behavior",
      align = "c")

# Calculate correlation matrix between q42 and all other variables
correlation_matrix <- cor(selected_data[, c('q42')], selected_data[, c('q27', 'q28', 'q29', 'q30',
                                                                       'q31', 'q32', 'q33', 'q39', 'q40', 'survey')], use="complete.obs", method = "spearman")
rownames(correlation_matrix)[1] <- "q42"
print(correlation_matrix)


# Create scatter plots for q42 against all other numeric variables
numeric_columns <- sapply(selected_data, is.numeric) & !(names(selected_data) %in%
                                                           c("q42"))
# Get the names of numeric columns (excluding q42)
numeric_column_names <- names(selected_data)[numeric_columns]
# Create scatter plots for q42 against all other numeric variables
par(mfrow=c(3, 3)) # Set the layout for the plots (adjust rows and columns as needed)
for (col in numeric_column_names) {
  plot(selected_data$q42, selected_data[[col]], main=paste("q42 vs.", col),
       xlab="q42", ylab=col, pch=16, col="blue")
}