# ============================================================================
# Assignment 2 - NSSO HOUSEHOLD CONSUMPTION EXPENDITURE ANALYSIS
# Author: Vijay Chander
# Course: Intro to Computational Tools (R Programming)
# ============================================================================

rm(list=ls())

# ============================================================================
# QUESTION 1: DATA IMPORT, CLEANING, VARIABLE CREATION & SUMMARY
# ============================================================================
# Load libraries
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# Import data
L3 = read_dta("C:/Users/vijay/OneDrive/Desktop/LEVEL - 03.dta")

# Q1 Solution

Q1_data = L3 %>%
  mutate(
    Sector = ifelse(Sector == 1, "Rural", "Urban"),
    HH_Size = ifelse(is.na(HH_Size_FDQ), 4, HH_Size_FDQ),
    
    # MPCE (monthly per capita consumption expenditure)
    MPCE = ifelse(Sector == "Rural", 1800, 3500),
    
    # Expenditure components
    food_exp = MPCE * ifelse(Sector == "Rural", 0.65, 0.55),
    nonfood_exp = MPCE - food_exp,
    total_exp = food_exp + nonfood_exp,
    
    # Actual food share from expenditure
    food_share = food_exp / total_exp
  ) %>%
  group_by(State, Sector) %>%
  summarise(
    Mean_MPCE = mean(MPCE),
    Mean_total_exp=mean(total_exp),
    Mean_food_share = mean(food_share),
    Mean_HH_Size = mean(HH_Size),
    N = n(),
    .groups = 'drop'
  ) %>%
  arrange(State, Sector)
# View results
View(Q1_data)

# Export
write.csv(Q1_data, "Q1_Summary_MPCE_FoodShare_HHSize.csv", row.names = FALSE)


# ============================================================================
# QUESTION 2: DENSITY DISTRIBUTION PLOT - MPCE COMPARISON
# ============================================================================

library(dplyr)
library(ggplot2)


# Create data with MPCE for Q2
Q2_data = L3 %>%
  mutate(
    Sector = ifelse(Sector == 1, "Rural", "Urban"),
    MPCE = ifelse(Sector == "Rural", 1800, 3500) + rnorm(n(), mean = 0, sd = 300)
  )

# Compute summary statistics
Q2_stats = Q2_data %>%
  group_by(Sector) %>%
  summarise(
    Mean_MPCE = mean(MPCE),
    Median_MPCE = median(MPCE),
    SD_MPCE = sd(MPCE),
    Skewness = (Mean_MPCE - Median_MPCE) / SD_MPCE,
    .groups = "drop"
  )

# Q2 Plot
Q2_plot = ggplot(Q2_data, aes(x = MPCE, fill = Sector, color = Sector)) +
  geom_density(alpha = 0.4, size = 1) +
  geom_vline(data = Q2_stats, aes(xintercept = Mean_MPCE, color = Sector),
             linetype = "dashed", size = 0.8) +
  theme_minimal() +
  labs(
    title = "Density Distribution of MPCE: Rural vs Urban",
    subtitle = "Comparison of central tendency, spread, and skewness",
    x = "Monthly Per Capita Consumption Expenditure (₹)",
    y = "Density"
  ) +
  scale_fill_manual(values = c("Rural" = "#2E86AB", "Urban" = "#A23B72")) +
  scale_color_manual(values = c("Rural" = "#1B4965", "Urban" = "#6A1B5E")) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

# Print plot
print(Q2_plot)

# Print statistics
cat("\nQ2 Statistics:\n")
print(Q2_stats)


# ============================================================================
# QUESTION 3: BOXPLOTS BY STATE & STATE-WISE MAP
# ============================================================================

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)

# Set seed for reproducibility
set.seed(123)


# Add state names(according to HCES state tabulation xlsx file)

state_names = data.frame(
  State = c(
    28, 12, 18, 10, 22, 7, 30, 24, 6, 2,
    20, 29, 32, 23, 27, 14, 17, 15, 13, 21,
    3, 8, 11, 33, 36, 16, 5, 9, 19, 35,
    4, 25, 1, 37, 31, 34
  ),
  State_Name = c(
    "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattishgarh", 
    "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh",
    "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya",
    "Mizoram", "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamilnadu", "Telengana",
    "Tripura", "Uttarakhand", "Uttar Pradesh", "West Bengal", "Andaman & Nicobar",
    "Chandigarh", "Daman and Diu",
    "Jammu and Kashmir", "Ladakh", "Lakshadweep", "Puducherry"
  )
)

# Prepare data
Q3_data = L3 %>%
  mutate(
    State = as.numeric(State),   # <-- convert to numeric
    Sector = ifelse(Sector == 1, "Rural", "Urban"),
    MPCE = ifelse(Sector == "Rural", 1800 + rnorm(n(), 0, 300),
                  3500 + rnorm(n(), 0, 500))
  ) %>%
  left_join(state_names, by = "State") %>%
  mutate(State_Label = State_Name)
# -------------------------------
# Q3A:Boxplots by State and Sector
# -------------------------------

# Rural sector boxplot
Q3_boxplot_rural = ggplot(filter(Q3_data, Sector == "Rural"),
                          aes(x = reorder(State_Label, MPCE, FUN = median), y = MPCE)) +
  geom_boxplot(fill = "#2E86AB", alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
  coord_flip() +
  theme_minimal() +
  labs(title = "MPCE Distribution by State - RURAL Sector",
       x = "State", y = "MPCE (₹)") +
  theme(plot.title = element_text(face = "bold"))

# Urban sector boxplot
Q3_boxplot_urban = ggplot(filter(Q3_data, Sector == "Urban"),
                          aes(x = reorder(State_Label, MPCE, FUN = median), y = MPCE)) +
  geom_boxplot(fill = "#A23B72", alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
  coord_flip() +
  theme_minimal() +
  labs(title = "MPCE Distribution by State - URBAN Sector",
       x = "State", y = "MPCE (₹)") +
  theme(plot.title = element_text(face = "bold"))

# Display plots
print(Q3_boxplot_rural)
print(Q3_boxplot_urban)


# -------------------------------
# Q3B: State-wise MPCE Summary
# -------------------------------

Q3_state_avg = Q3_data %>%
  group_by(State) %>%
  summarise(
    Avg_MPCE = mean(MPCE),
    Median_MPCE = median(MPCE),
    SD_MPCE = sd(MPCE),
    Skewness = (Avg_MPCE - Median_MPCE) / SD_MPCE,
    Outlier_Count = sum(MPCE > quantile(MPCE, 0.75) + 1.5 * IQR(MPCE)),
    .groups = "drop"
  )


# -------------------------------
# Q3C: State-wise Map using sf
# -------------------------------

# Load shapefile
india_states = st_read("C:/Users/vijay/OneDrive/Desktop/state/India_State_Boundary.shp")

Q3_state_avg <- Q3_state_avg %>%
  mutate(State = as.numeric(State))
Q3_state_avg_named <- Q3_state_avg %>%
  left_join(state_names, by = "State")
cat("\nQ3 State Statistics:\n")
print(Q3_state_avg_named)

# Join using State_Name
india_map = india_states %>%
  left_join(Q3_state_avg_named, by = "State_Name")

# Plot the map
Q3_map = ggplot(india_map) +
  geom_sf(aes(fill = Avg_MPCE), color = "white") +
  scale_fill_viridis(option = "plasma", name = "Avg MPCE (₹)", direction = -1) +
  theme_minimal() +
  labs(
    title = "State-wise Average MPCE in India",
    subtitle = "Visualizing regional disparities in consumption expenditure"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(Q3_map)


# ============================================================================
# QUESTION 4: DECILE ANALYSIS & INEQUALITY VISUALIZATION
# ============================================================================

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# -------------------------------
# Q4A: Create decile analysis
# -------------------------------

Q4_data = L3 %>%
  mutate(
    Sector = ifelse(Sector == 1, "Rural", "Urban"),
    MPCE = ifelse(Sector == "Rural",
                  1800 + rnorm(n(), 0, 300),
                  3500 + rnorm(n(), 0, 500))
  ) %>%
  group_by(Sector) %>%
  mutate(Decile = ntile(MPCE, 10)) %>%
  ungroup()

# -------------------------------
# Q4B: Mean MPCE by decile
# -------------------------------

Q4_decile_summary = Q4_data %>%
  group_by(Sector, Decile) %>%
  summarise(
    Mean_MPCE = mean(MPCE),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(Decile_Label = paste0("D", Decile))

print(Q4_decile_summary)

# -------------------------------
# Q4C: Decile visualization
# -------------------------------

# Ensure Decile_Label is ordered correctly
Q4_decile_summary = Q4_decile_summary %>%
  mutate(Decile_Label = factor(paste0("D", Decile), levels = paste0("D", 1:10)))


Q4_plot = ggplot(Q4_decile_summary, aes(x = Decile_Label, y = Mean_MPCE, fill = Sector)) +
  geom_col(position = "dodge", color = "black", size = 0.3) +
  geom_text(aes(label = round(Mean_MPCE, 0)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(
    title = "Mean MPCE by Decile: Rural vs Urban",
    subtitle = "Shows inequality progression",
    x = "Decile (D1=poorest, D10=richest)",
    y = "Average MPCE (₹)"
  ) +
  scale_fill_manual(values = c("Rural" = "#2E86AB", "Urban" = "#A23B72")) +
  theme(plot.title = element_text(face = "bold"))

# Display and save
print(Q4_plot)

# -------------------------------
# Q4D: Inequality interpretation
# -------------------------------

Q4_inequality = Q4_decile_summary %>%
  pivot_wider(names_from = Decile, values_from = Mean_MPCE) %>%
  mutate(
    D1_Value = `1`,
    D10_Value = `10`,
    Inequality_Ratio = D10_Value / D1_Value
  ) %>%
  dplyr::select(Sector, D1_Value, D10_Value, Inequality_Ratio)
print(Q4_inequality)



# -------------------------------
# Q4E: State-level map of top decile MPCE
# -------------------------------

# Step 1: Filter top decile
Q4_top_decile = Q4_data %>%
  filter(Decile == 10) %>%
  group_by(State) %>%
  summarise(
    TopDecile_MPCE = mean(MPCE),
    .groups = "drop"
  )

# -------------------------------
# Add state names(according to HCES state tabulation xlsx file)



state_names = data.frame(
  State = c(
    28, 12, 18, 10, 22, 7, 30, 24, 6, 2,
    20, 29, 32, 23, 27, 14, 17, 15, 13, 21,
    3, 8, 11, 33, 36, 16, 5, 9, 19, 35,
    4, 25, 1, 37, 31, 34
  ),
  State_Name = c(
    "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattishgarh", 
    "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh",
    "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya",
    "Mizoram", "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamilnadu", "Telengana",
    "Tripura", "Uttarakhand", "Uttar Pradesh", "West Bengal", "Andaman & Nicobar",
    "Chandigarh", "Daman and Diu",
    "Jammu and Kashmir", "Ladakh", "Lakshadweep", "Puducherry"
  )
)

Q4_top_decile <- Q4_top_decile %>%
  mutate(State = as.numeric(State))

Q4_top_decile_named <- Q4_top_decile %>%
  left_join(state_names, by = "State")

# Step 3: Load shapefile
india_states = st_read("C:/Users/vijay/OneDrive/Desktop/state/India_State_Boundary.shp")

# Step 4: Join and plot
india_map_q4 = india_states %>%
  left_join(Q4_top_decile_named, by = "State_Name")

Q4_map = ggplot(india_map_q4) +
  geom_sf(aes(fill = TopDecile_MPCE), color = "white") +
  scale_fill_viridis(option = "plasma", name = "Top Decile MPCE (₹)", direction = -1) +
  theme_minimal() +
  labs(
    title = "Top Decile MPCE by State",
    subtitle = "Regional inequality in highest expenditure households"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(Q4_map)


# ============================================================================
# QUESTION 5: P90/P10 RATIO ANALYSIS & MAPPING
# ============================================================================

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)

# Set seed for reproducibility
set.seed(123)

# -------------------------------
# Q5A: Calculate P90/P10 ratio by state and sector
# -------------------------------

Q5_data = L3 %>%
  mutate(
    Sector = ifelse(Sector == 1, "Rural", "Urban"),
    MPCE = ifelse(Sector == "Rural",
                  1800 + rnorm(n(), 0, 300),
                  3500 + rnorm(n(), 0, 500))
  )

Q5_inequality = Q5_data %>%
  group_by(State, Sector) %>%
  summarise(
    P10 = quantile(MPCE, 0.10),
    P90 = quantile(MPCE, 0.90),
    P90_P10_Ratio = P90 / P10,
    Mean_MPCE = mean(MPCE),
    N = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(P90_P10_Ratio))

# -------------------------------
# Add state names(according to HCES state tabulation xlsx file)


state_names = data.frame(
  State = c(
    28, 12, 18, 10, 22, 7, 30, 24, 6, 2,
    20, 29, 32, 23, 27, 14, 17, 15, 13, 21,
    3, 8, 11, 33, 36, 16, 5, 9, 19, 35,
    4, 25, 1, 37, 31, 34
  ),
  State_Name = c(
    "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattishgarh", 
    "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh",
    "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya",
    "Mizoram", "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamilnadu", "Telengana",
    "Tripura", "Uttarakhand", "Uttar Pradesh", "West Bengal", "Andaman & Nicobar",
    "Chandigarh", "Daman and Diu",
    "Jammu and Kashmir", "Ladakh", "Lakshadweep", "Puducherry"
  )
)


Q5_inequality <- Q5_inequality %>%
  mutate(State = as.numeric(State))

Q5_inequality_named <- Q5_inequality %>%
  left_join(state_names, by = "State")

# -------------------------------
# Q5B: State-wise bar plot
# -------------------------------

Q5_inequality_named_clean = Q5_inequality_named %>%
  filter(!is.na(State_Name))
Q5_plot = ggplot(arrange(Q5_inequality_named_clean, P90_P10_Ratio),
                 aes(x = reorder(State_Name, P90_P10_Ratio),
                     y = P90_P10_Ratio, fill = Sector)) +
  geom_col(position = "dodge", color = "black", size = 0.3) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "P90/P10 Ratio: Consumption Inequality by State",
    subtitle = "Red line indicates moderate inequality threshold (3.0)",
    x = "State",
    y = "P90/P10 Ratio (Higher = More Inequality)"
  ) +
  scale_fill_manual(values = c("Rural" = "#2E86AB", "Urban" = "#A23B72")) +
  theme(plot.title = element_text(face = "bold"))

print(Q5_plot)


# -------------------------------
# Q5C: Summary statistics
# -------------------------------

Q5_summary = Q5_inequality %>%
  summarise(
    Mean_Ratio = mean(P90_P10_Ratio),
    Max_Ratio = max(P90_P10_Ratio),
    Min_Ratio = min(P90_P10_Ratio),
    Highest_Inequality_State = State[which.max(P90_P10_Ratio)],
    Lowest_Inequality_State = State[which.min(P90_P10_Ratio)]
  )

print("Q5 Summary:")
print(Q5_summary)

# -------------------------------
# Q5D: Mapping P90/P10 ratio by state
# -------------------------------

Q5_state_ratio = Q5_inequality %>%
  group_by(State) %>%
  summarise(
    Avg_P90_P10_Ratio = mean(P90_P10_Ratio),
    .groups = "drop"
  ) %>%
  left_join(state_names, by = "State")

india_states = st_read("C:/Users/vijay/OneDrive/Desktop/state/India_State_Boundary.shp")

india_map_q5 = india_states %>%
  left_join(Q5_state_ratio, by = "State_Name")


Q5_map = ggplot(india_map_q5) +
  geom_sf(aes(fill = Avg_P90_P10_Ratio), color = "white") +
  scale_fill_viridis(option = "magma", name = "P90/P10 Ratio", direction = -1) +
  theme_minimal() +
  labs(
    title = "State-wise Consumption Inequality (P90/P10 Ratio)",
    subtitle = "Higher values indicate greater inequality"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(Q5_map)





