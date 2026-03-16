# NSSO Household Consumption Expenditure Analysis (India)

This project analyzes India's household consumption patterns using NSSO (National Sample Survey Office) data. The analysis focuses on differences in spending behavior between rural and urban households, regional disparities across states, and inequality in consumption levels.

The entire analysis was conducted using **R programming**, including data cleaning, statistical summaries, and visualizations.

---

# Project Objectives

The main goals of this project are:

- Analyze **Monthly Per Capita Consumption Expenditure (MPCE)**
- Compare **rural and urban consumption patterns**
- Identify **state-wise consumption disparities**
- Measure **inequality across income groups**
- Visualize **regional expenditure patterns using maps**

---

# Dataset

Source: NSSO Household Consumption Expenditure Survey  
Portal: https://microdata.gov.in

The dataset includes household-level information across **37 states and union territories of India**.

Key variables used:

- Food expenditure
- Non-food expenditure
- Household size
- Sector (Rural / Urban)
- State
--
**Note:** Due to data security and usage restrictions from the NSSO microdata portal, the dataset is **not included in this repository**. Users can download the dataset directly from the official portal and run the analysis scripts provided in this project.
---

# Data Processing

The following preprocessing steps were performed in R:

- Cleaned dataset and converted sector codes into **Rural and Urban**
- Handled missing values in household size
- Created new variables:
  - **Total Expenditure**
  - **Food Share (Food Expenditure / Total Expenditure)**
- Computed **state and sector level averages**

---

# Key Analysis

## 1. MPCE Distribution (Rural vs Urban)

A density plot was created to compare expenditure distributions.

Findings:

- Urban households have **higher average MPCE**
- Urban distribution is **more dispersed**
- Rural expenditure is **more concentrated around the mean**

---

## 2. State-wise MPCE Analysis

Boxplots were created to examine expenditure distribution across states.

Insights:

- Metropolitan states like **Delhi and Maharashtra show higher spending**
- Rural states like **Bihar and Uttar Pradesh show lower MPCE**
- Urban areas show **greater spending variability**

---

## 3. Geographic Visualization

Using **sf package in R**, state-wise maps were created to visualize:

- Average MPCE
- Top decile spending (90th percentile)

Findings:

- Southern states (Kerala, Tamil Nadu, Karnataka) show **higher consumption levels**
- Northern states show **lower average spending**

---

## 4. Decile Analysis

Households were divided into **10 expenditure groups (Deciles)**.

Findings:

- Spending increases steadily from **Decile 1 (poorest)** to **Decile 10 (richest)**
- Urban households spend **more at every decile level**

---

## 5. Inequality Measurement (P90/P10 Ratio)

The **P90/P10 ratio** was calculated to measure inequality.

Key result:

- Most states have a similar ratio (~1.49)
- Indicates inequality within states is relatively stable
- The larger gap exists between **rural and urban sectors**

---

# Key Insights

- Urban households spend **almost twice as much as rural households**
- Urban consumption shows **greater inequality**
- Regional differences exist across Indian states
- Southern states show **higher consumption levels**
- Rural households spend a **larger share of income on food**

---

# Tools & Libraries Used

- R Programming
- ggplot2
- dplyr
- sf
- tidyverse

---
## Feedback & Contact

Feel free to share your feedback, suggestions, or reviews.  
You can also connect with me on **LinkedIn**:

https://www.linkedin.com/in/vijay-chander-16182433b/

---
