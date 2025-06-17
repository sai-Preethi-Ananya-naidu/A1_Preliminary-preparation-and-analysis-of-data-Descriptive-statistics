# 0. Set Working Directory
setwd("C:\\Users\\user\\Desktop\\VCU\\BOOT CAMP\\SCMA-632-C51 - STATISTICL ANALYSIS & MODELING\\VCU_christ")
getwd()


# 1. Installing and Importing Necessary libraries ####

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "tidyr", "ggplot2", "BSDA") # Vector of required packages
lapply(libraries, install_and_load)

# 2. Reading the dataset into R ####

data <- read.csv("NSSO68.csv")

# 3. Filtering data for Nagaland (State Code 13) ####
state_name <- "NAG"
state_data <- data %>%
  filter(state_1 == state_name)  # Filter Nagaland
write.csv(state_data, 'Nagaland_filtered_data.csv', row.names = FALSE)


unique(data$state_1)
unique(state_data$state_1)


# 4. Display dataset information ####
cat("Dataset Information:\n")
print(names(state_data))
print(head(state_data))
print(dim(state_data))
sum(is.na(state_data))

# 5. Check for missing values ####
missing_info <- colSums(is.na(state_data))
cat("Missing Values Information:\n")
print(missing_info)

# 6. Select relevant columns for analysis ####
state_subset <- state_data %>%
  select(state_1, District, Region, Sector, State_Region,
         Meals_At_Home, ricetotal_v, wheattotal_v, Milktotal_v,
         pulsestot_v, nonvegtotal_v, fruitstt_v, No_of_Meals_per_day)

# 7. Impute missing values with mean ####
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

missing_info <- colSums(is.na(state_subset))
cat("Missing Values Before Imputation:\n")
print(missing_info)

state_subset$Meals_At_Home <- impute_with_mean(state_subset$Meals_At_Home)

missing_info <- colSums(is.na(state_subset))
cat("Missing Values After Imputation:\n")
print(missing_info)

# 8. Remove outliers from specific columns ####
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - 1.5 * IQR
  upper_threshold <- Q3 + 1.5 * IQR
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c('Meals_At_Home', 'ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                     'pulsestot_v','nonvegtotal_v', 'fruitstt_v', 'No_of_Meals_per_day')

for (col in outlier_columns) {
  state_subset <- remove_outliers(state_subset, col)
}

# 9. Create total consumption variable ####
state_subset$total_consumption <- rowSums(state_subset[, c('ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                                                           'pulsestot_v','nonvegtotal_v', 'fruitstt_v')], na.rm = TRUE)

# 10. Summarize consumption by district and region ####
summarize_consumption <- function(group_col) {
  summary <- state_subset %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")



cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Bottom Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

# 11. Rename district and sector codes (based on NSS codes) ####
district_mapping <- c(
  "1" = "Mon", "2" = "Tuensang", "3" = "Mokokchung", "4" = "Zunheboto",
  "5" = "Wokha", "6" = "Dimapur", "7" = "Kohima", "8" = "Phek",
  "9" = "Kiphire", "10" = "Longleng", "11" = "Peren"
)

sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

state_subset$District <- as.character(state_subset$District)
state_subset$Sector <- as.character(state_subset$Sector)

state_subset$District <- ifelse(state_subset$District %in% names(district_mapping),
                                district_mapping[state_subset$District],
                                state_subset$District)

state_subset$Sector <- ifelse(state_subset$Sector %in% names(sector_mapping),
                              sector_mapping[state_subset$Sector],
                              state_subset$Sector)

# Update summaries again after mapping
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")


cat("Updated Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Updated Bottom Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Updated Region Consumption Summary:\n")
print(region_summary)
cat("Updated Sector Consumption Summary:\n")
print(sector_summary)

# 12. Test for mean difference between Urban and Rural consumption ####
rural <- state_subset %>% filter(Sector == "RURAL") %>% select(total_consumption)
urban <- state_subset %>% filter(Sector == "URBAN") %>% select(total_consumption)

z_test_result <- z.test(rural, urban, alternative = "two.sided",
                        mu = 0, sigma.x = 2.1, sigma.y = 2.3, conf.level = 0.95)

if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, "→ Reject H0: Urban and Rural means differ significantly.\n")
} else {
  cat("P value is >=", 0.05, "→ Fail to reject H0: No significant difference in Urban vs Rural means.\n")
}

# 13. Test for mean difference between Top and Bottom Districts ####
top_district <- head(district_summary$District, 1)
bottom_district <- tail(district_summary$District, 1)

top_data <- state_subset %>% filter(District == top_district) %>% select(total_consumption)
bottom_data <- state_subset %>% filter(District == bottom_district) %>% select(total_consumption)

z_test_result2 <- z.test(top_data, bottom_data, alternative = "two.sided",
                         mu = 0, sigma.x = 2.1, sigma.y = 2.3, conf.level = 0.95)

if (z_test_result2$p.value < 0.05) {
  cat("P value is <", 0.05, "→ Reject H0: Top and Bottom district means differ significantly.\n")
} else {
  cat("P value is >=", 0.05, "→ Fail to reject H0: No significant difference between top and bottom district consumption.\n")
}
