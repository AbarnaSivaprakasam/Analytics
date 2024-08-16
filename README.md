# Medical Malpractice Analysis

## Overview
This project provides a comprehensive analysis of a US medical malpractice dataset using inferential statistics and exploratory data analysis (EDA). The analysis includes data cleaning, cross-tabulations, hypothesis testing, and regression analysis to uncover patterns and insights into medical malpractice claims.

## Dataset
Title: Medical Malpractice
Source: Kaggle
Link: Kaggle Dataset
Description: The dataset contains 79,210 rows and 8 columns, including information on claimant age, claim payment amount, severity rating, attorney representation, marital status, insurance type, and physician specialty.

## Analysis
Exploratory Data Analysis (EDA)
Data Cleaning: No missing values; outliers identified in claim amounts.
Descriptive Statistics: Average claim amount is $157,484; severity ratings range from 1 to 9 with an average of 4.8. Private attorneys are involved in 66% of cases.
Cross Tabulations: Explored relationships between marital status and private attorney involvement, gender, and severity.
Subset Analysis: Analyzed severity levels, age, and claim amounts.
Visualizations: Included claim count by severity level, gender, insurance type, and specialty.

Inferential Statistics and Hypothesis Testing
Claim Amount Comparison:
Hypothesis: Compare claim amounts between private attorney cases and all instances.
Result: No significant difference in claim amounts between private attorney cases and the overall dataset.

Severity Distribution by Specialty:
Hypothesis: Assess differences in claim severity across physician specialties.
Result: Significant differences found in claim severity among different specialties.

Gender and Severity:
Hypothesis: Test if there is a difference in severity between males and females.
Result: Significant difference found, with females having lower average claim severity.

Age and Severity Correlation:
Hypothesis: Explore correlation between claimant age and claim severity.
Result: Weak negative correlation between age and severity.

Regression Testing
Claim Amount and Age:
Hypothesis: Investigate the relationship between claim amount and claimant age.
Result: Statistically significant relationship found, though age explains only a small portion of the variation in claim amounts.

Claim Amount by Age and Insurance Type:
Hypothesis: Examine the impact of insurance type on the relationship between age and claim amount.
Result: Private insurance leads to higher claim amounts, while other insurance types show varied effects.

Claim Amount by Age and Marital Status:
Hypothesis: Analyze the effect of marital status on the relationship between age and claim amount.
Result: Some significant interaction effects observed.

Conclusion
This analysis reveals key insights into medical malpractice claims, including significant differences in claim amounts based on private attorney representation, variations in claim severity across specialties, and gender-based differences in claim severity. The relationship between claim amount and age, as well as the effects of insurance type and marital status, were also explored.
