#Abarna Sivaprakasam
#Final 

library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(stats)
library(moments)
library(psych)

#loading the data
data<- read.csv('medicalmalpractice.csv')
head(data)

#summary of the data
str(data)
summary(data)

#checking for missing values
sum(is.na(data))

#finding the unique values
sapply(data, function(x) length(unique(x)))


#outliers
boxplot(data$Amount)
boxplot(data$Age)
boxplot(data$Severity, main='Box Plot for Severity', ylab='Severity')

ggplot(data, aes(y = Amount)) + geom_boxplot(fill = "lightgreen", color = "black") + labs(title = "Amount Distribution - Boxplot")


# Plotting histogram with mean, median, and mode lines
ggplot(data, aes(x=Amount)) +
  geom_histogram(binwidth=100000, fill="lightblue", color="black") +
  geom_vline(xintercept=mean(data$Amount), color="red", linetype="dashed", size=1, label="Mean") +
  geom_vline(xintercept=median(data$Amount), color="green", linetype="dashed", size=1, label="Median") +
  geom_vline(xintercept=mode, color="blue", linetype="dashed", size=1, label="Mode") +
  scale_x_continuous(labels=scales::comma_format()) +  # Format x-axis labels as regular numbers
  labs(title="Amount Distribution", x="Amount") +
  theme_minimal() +
  theme(legend.position="topright")


#Descriptive statistics
describe(data)


#tables 
print(table(data$Specialty))
print(table(data$Insurance))
print(table(data$Gender))
print(table(data$Marital.Status))
print(table(data$Severity))
print(table(data$Private.Attorney))


# Cross-tabulation between Marital Status and Private Attorney
cross_tab <- table(data$`Marital.Status`, data$`Private.Attorney`)
print(cross_tab)

# Cross-tabulation between Marital Status and gender
cross_tab2 <- table(data$Marital.Status, data$Gender)
# View the cross-tabulation
print(cross_tab2)

# Cross-tabulation between Marital Status and severity
cross_tab3 <- table(data$Marital.Status, data$Severity)
# View the cross-tabulation
print(cross_tab3)

# Cross-tabulation between Specialty and severity
cross_tab4 <- table(data$Specialty, data$Severity)
# View the cross-tabulation
print(cross_tab4)

# Histogram for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

#subset data - categorical variable
subset_data <- subset(data, Severity >= 7) 
# Histogram for subset data
ggplot(subset_data, aes(x = Age)) + geom_histogram(binwidth = 5, fill = "orange", color = "black") + labs(title = "Age Distribution for High Severity Cases")
summary(subset_data$Age)

#subset data - discrete variable
discrete_subset <- data$Age

discrete_subset_df <- data.frame(Age = discrete_subset)
# Plot the histogram
ggplot(discrete_subset_df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

#subset data - continuous variable
continuous_subset <- data[, c("Amount")]
ggplot(data, aes(y = Amount, x = 1)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Claim Amount Distribution",
       x = "",
       y = "Amount") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# Count of claims by severity level
severity_counts <- data %>%
  group_by(Severity) %>%
  summarise(count = n())
# Visualize claim counts by severity
ggplot(severity_counts, aes(x = Severity, y = count)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Claim Counts by Severity Level", x = "Severity Level", y = "Claim Count")


# Count of claims by gender
gender_counts <- data %>%
  group_by(Gender) %>%
  summarise(count = n())
# Visualize claim counts by gender
ggplot(gender_counts, aes(x = Gender, y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Claim Counts by Gender", x = "Gender", y = "Claim Count")



# Count of claims by insurance type
insurance_counts <- data %>%
  group_by(Insurance) %>%
  summarise(count = n())
# Visualize claim counts by insurance type
ggplot(insurance_counts, aes(x = Insurance, y = count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Claim Counts by Insurance Type", x = "Insurance Type", y = "Claim Count")


# Scatter plot of Amount vs. Severity
ggplot(data, aes(x = Severity, y = Amount)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Claim Amount vs. Severity", x = "Severity", y = "Claim Amount")
summary(data$Amount)
summary(data$Severity)


# Create a table of counts for Private Attorney and Insurance
attorney_insurance_counts <- table(data$Private.Attorney, data$Insurance)
# Convert the table to a data frame
attorney_insurance_df <- as.data.frame(attorney_insurance_counts)
names(attorney_insurance_df) <- c("Private.Attorney", "Insurance", "Count")
# Create a bar plot
ggplot(attorney_insurance_df, aes(x = Insurance, y = Count, fill = Private.Attorney)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Claims Count by Private Attorney and Insurance Type",
       x = "Insurance Type", y = "Claim Count",
       fill = "Private Attorney") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Count the number of claims for each Marital Status
marital_counts <- table(data$`Marital.Status`)
# Create a bar plot for Marital Status
ggplot(data.frame(Marital.Status = names(marital_counts), Count = as.numeric(marital_counts)), 
       aes(x = reorder(Marital.Status, -Count), y = Count, fill = Marital.Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Count by Marital Status", x = "Marital Status", y = "Claim Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Count the number of claims for each Specialty and Marital Status
specialty_counts <- table(data$Specialty, data$`Marital.Status`)
# Create a stacked bar plot for Specialty and Marital Status
ggplot(data = as.data.frame(specialty_counts), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Count by Specialty and Marital Status", x = "Specialty", y = "Claim Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Question 1: Is there sufficient evidence to conclude that the average claim amounts for cases represented by private attorneys are different from the overall average claim amount?

# Null Hypothesis (H0): The mean claim amount for cases with private attorneys is equal to the overall mean claim amount.
# Alternative Hypothesis (H1): The mean claim amount for cases with private attorneys is different from the overall mean claim amount.
one_sample_t_test_result <- t.test(data$Amount, mu = mean(data$Amount))

# Display null and alternative hypotheses
cat("\nNull Hypothesis: The mean claim amount for cases with private attorneys is equal to the overall mean claim amount.\n")
cat("Alternative Hypothesis: The mean claim amount for cases with private attorneys is different from the overall mean claim amount.\n")
# Display t-test results
cat("\nOne-Sample t-test Results for Claim Amounts:\n")
print(one_sample_t_test_result)
# Interpret the results
cat("\nInterpretation:\n")
if (one_sample_t_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is sufficient evidence that the average claim amounts for cases with private attorneys are different from the overall average claim amount.\n")
} else {
  cat("Fail to reject the null hypothesis. No sufficient evidence of a difference in average claim amounts.\n")
}

# Question 2: Does the distribution of claim severity differ significantly among physician specialties?

# Null Hypothesis (H0): There is no significant difference in claim severity distribution across physician specialties.
# Alternative Hypothesis (H1): Claim severity distribution varies significantly among different physician specialties.

# Perform Kruskal-Wallis test 
kruskal_result <- kruskal.test(Severity ~ Specialty, data = data)

# Display null and alternative hypotheses
cat("\nNull Hypothesis: There is no significant difference in claim severity distribution across physician specialties.\n")
cat("Alternative Hypothesis: Claim severity distribution varies significantly among different physician specialties.\n")

# Display Kruskal-Wallis test results
cat("\nKruskal-Wallis Test Results for Claim Severity by Physician Specialty:\n")
print(kruskal_result)

# Interpret the results
cat("\nInterpretation:\n")
if (kruskal_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is sufficient evidence that claim severity distribution differs significantly among physician specialties.\n")
} else {
  cat("Fail to reject the null hypothesis. No sufficient evidence of a difference in claim severity distribution.\n")
}


#Question 3: Is there a significant difference in claim severity between male and female claimants?
# Null Hypothesis (H0): The mean claim severity for male claimants is equal to the mean claim severity for female claimants.
# Alternative Hypothesis (H1): The mean claim severity for male claimants is different from the mean claim severity for female claimants.

# Perform Two-sample t-test 
t_test_gender_result <- t.test(data$Severity ~ data$Gender)

# Display null and alternative hypotheses
cat("\nNull Hypothesis: The mean claim severity for male claimants is equal to the mean claim severity for female claimants.\n")
cat("Alternative Hypothesis: The mean claim severity for male claimants is different from the mean claim severity for female claimants.\n")

# Display Two-sample t-test results
cat("\nTwo-sample T-test Results for Claim Severity between Male and Female Claimants:\n")
print(t_test_gender_result)

# Interpret the results
cat("\nInterpretation:\n")
if (t_test_gender_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is sufficient evidence that the mean claim severity differs between male and female claimants.\n")
} else {
  cat("Fail to reject the null hypothesis. No sufficient evidence of a difference in mean claim severity between male and female claimants.\n")
}


# Question 4: Is there a statistically significant correlation between the age of the claimant and the severity of the claim?

# Null Hypothesis (H0): There is no correlation between the age of the claimant and claim severity.
# Alternative Hypothesis (H1): There is a significant correlation between age and claim severity.

# Perform Pearson correlation test
correlation_result <- cor.test(data$Age, data$Severity)

# Display null and alternative hypotheses
cat("\nNull Hypothesis: There is no correlation between the age of the claimant and claim severity.\n")
cat("Alternative Hypothesis: There is a significant correlation between age and claim severity.\n")

# Display Pearson correlation test results
cat("\nPearson Correlation Test Results for Age and Claim Severity:\n")
print(correlation_result)

# Interpret the results
cat("\nInterpretation:\n")
if (correlation_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is sufficient evidence of a statistically significant correlation between the age of the claimant and the severity of the claim.\n")
} else {
  cat("Fail to reject the null hypothesis. No sufficient evidence of a significant correlation between age and claim severity.\n")
}

#Correlation

numeric_data <- data[, sapply(data, is.numeric)]
# Create a correlation matrix
correlation_matrix <- cor(numeric_data)
# Print the correlation matrix table
print(correlation_matrix)
# Plot the correlation matrix heatmap
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(50), addCoef.col = "black", tl.col = "black", tl.srt = 45)
# Add a title
title("Correlation Matrix Heatmap")


# Question 1: Relationship between claim payment amount and the age of the claimant
# Dependent Variable: Amount of the claim payment
# Independent Variable: Age of the claimant
# Convert Amount to millions for better readability
sampled_subset$Amount_million <- sampled_subset$Amount / 1e6

# Identify and remove outliers using IQR
Q1 <- quantile(sampled_subset$Amount_million, 0.25)
Q3 <- quantile(sampled_subset$Amount_million, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
sampled_subset <- sampled_subset[sampled_subset$Amount_million >= lower_bound & sampled_subset$Amount_million <= upper_bound, ]

# If there are cases after removing outliers, perform linear regression and generate a scatterplot
if (nrow(sampled_subset) > 0) {
# Perform linear regression on the sampled subset
model_another <- lm(Amount_million ~ Age, data = sampled_subset)
# print the summary of the model
print(summary(model_another))
# Scatterplot
  ggplot(sampled_subset, aes(x = Age, y = Amount_million)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Scatterplot of Amount vs. Age",
         x = "Age of the Claimant",
         y = "Amount of Claim Payment (Millions)")
# Extract the p-value
  p_value <- summary(model_another)$coefficients[2, 4]
# Display the p-value
  cat("P-value for the Age coefficient:", p_value, "\n")
# Check for significance (common threshold is 0.05)
  if (p_value < 0.05) {
    cat("The relationship between age and amount is statistically significant.\n")
  } else {
    cat("There is no statistically significant relationship between age and amount.\n")
  }
} else {
  cat("No cases found for the chosen subset after removing outliers.")
}


#Question 2: Is there a significant relationship between the claim amount (dependent variable) and the age of the claimant (continuous numerical independent variable) based on their insurance type (categorical independent variable)?
#Dependent Variable:Amount of the claim payment
#Independent Variable:age of the claimant based on their insurance type
quantile_threshold_q4 <- quantile(data$Amount, c(0.01, 0.99))
data_subset_q4 <- subset(data, Amount >= quantile_threshold_q4[1] & Amount <= quantile_threshold_q4[2])
ggplot(data_subset_q4_sampled, aes(x = Age, y = Amount, color = Insurance)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Insurance), show.legend = TRUE) +
  ggtitle("Scatterplot: Age vs. Amount by Insurance Type") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Private" = "red", "No Insurance" = "blue", "Unknown" = "green", "Workers Compensation" = "yellow", "Medicare/Medicaid" = "purple"))
# Linear Regression Model
model4_q4 <- lm(Amount ~ Age * Insurance, data = data_subset_q4)
# Hypothesis Testing - Display summary output
summary(model4_q4)
# Extract p-values
p_values <- coef(summary(model4_q4))[, "Pr(>|t|)"]
# Display p-values
cat("P-values for each coefficient:\n")
print(p_values)
# Check for significance 
significant_terms <- names(p_values[p_values < 0.05])
if (length(significant_terms) > 0) {
  cat("Significant terms:", paste(significant_terms, collapse = ", "), "\n")
} else {
  cat("No statistically significant terms found.\n")
}



#Question 3: Is there a significant relationship between the claim amount (dependent variable) and the age of the claimant (continuous numerical independent variable) based on their marital status (categorical independent variable)?
# Dependent Variable: Amount of the claim payment
# Independent Variable: Age of the claimant based on marital status
# Set seed for reproducibility
set.seed(123)
# Define the quantile threshold for outliers
quantile_threshold_q3 <- quantile(data$Amount, c(0.01, 0.99))
# Create a subset without outliers
data_subset_q3 <- subset(data, Amount >= quantile_threshold_q3[1] & Amount <= quantile_threshold_q3[2])
# Create a random sample for the scatterplot
sample_indices_q3 <- sample(seq_len(nrow(data_subset_q3)), size = 1000)  # Adjust the sample size as needed
data_subset_q3_sampled <- data_subset_q3[sample_indices_q3, ]
# Scatterplot with Regression Lines and Legend
ggplot(data_subset_q3_sampled, aes(x = Age, y = Amount, color = factor(Marital.Status))) +
  geom_point(aes(shape = factor(Marital.Status)), size = 3) +  # Use different shapes for each point based on Marital Status
  geom_smooth(method = "lm", se = FALSE, aes(group = factor(Marital.Status)), show.legend = TRUE) +  # Add regression lines for each Marital Status
  ggtitle("Scatterplot: Age vs. Amount by Marital Status") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "orange", "4" = "purple")) +
  scale_shape_manual(values = c("0" = 16, "1" = 17, "2" = 18, "3" = 19, "4" = 20))  # Use different shapes for each Marital Status
# Linear Regression
model3_q3_sampled <- lm(Amount ~ Age * factor(Marital.Status), data = data_subset_q3_sampled)
# Hypothesis Testing
summary(model3_q3_sampled)
# Extract p-values for each coefficient
p_values <- coef(summary(model3_q3_sampled))[, "Pr(>|t|)"]
# Display p-values
cat("P-values for each coefficient:\n")
print(p_values)
# Specify the null and alternative hypotheses
cat("\nNull Hypothesis: There is no significant relationship between claim amount and the age of the claimant based on marital status.\n")
cat("Alternative Hypothesis: There is a significant relationship between claim amount and the age of the claimant based on marital status.\n")
# Check for significance (common threshold is 0.05)
significant_terms <- names(p_values[p_values < 0.05])
if (length(significant_terms) > 0) {
  cat("\nSignificant terms:", paste(significant_terms, collapse = ", "), "\n")
} else {
  cat("\nNo statistically significant terms found.\n")
}

