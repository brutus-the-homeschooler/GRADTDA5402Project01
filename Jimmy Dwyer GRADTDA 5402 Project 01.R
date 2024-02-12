#############################
##       Pre-work          ##
#############################

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load the files
team_data <- read.table("NBA_teamstats.txt", header = TRUE, sep = "\t")
team_record <- read.csv("Nba_team_records.csv")
current_season_data <- read_excel("current_season.xlsx")

# Join data frames
merged_data <- merge(team_data, team_record, by.x = c("Team", "Season"), by.y = c("Abbreviation", "Season"))

# Remove unnecessary columns
merged_data <- merged_data[, !(names(merged_data) %in% c("Team.y","SemiFinals","Champion","ConferenceFinals","Finals"))]

# Creation of new metrics, Points per possession/defensive points per possession/Win Percentage
merged_data$PointsPerPoss <- merged_data$PointsFor / merged_data$PossFor
merged_data$DefensivePointsPerPoss <- merged_data$PointsOpp / merged_data$PossOpp
merged_data$WinPercentage <- merged_data$W / (merged_data$W+merged_data$L)


# Create dataframe with no team or season identifers
no_team_name <- merged_data[, !names(merged_data) %in% c("Team","Season")]



####################################
## Create Histograms of Variables ##
####################################

# Create histograms for each variable
metrics <- names(no_team_name)[!names(no_team_name) %in% "Playoffs"]
for (metric in metrics) {
  p <- ggplot(no_team_name, aes(x = !!sym(metric), fill = factor(Playoffs))) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
    facet_wrap(~ Playoffs) +
    labs(title = paste("Histogram of", metric), x = metric, y = "Frequency") +
    theme_minimal()
  
  print(p)  # Print the histogram
}

#############################
## T Testing All Variables ##
#############################

# T Test results for each variable
# Create an empty dataframe to store the results
t_test_results <- data.frame(Column = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through each column in the dataframe except the 'Playoffs' column
for (col in names(no_team_name)[!names(no_team_name) %in% "Playoffs"]) {
  # Perform two-sample t-test
  t_test <- t.test(no_team_name[[col]] ~ no_team_name$Playoffs)
  
  # Extract p-value from the test result
  p_value <- t_test$p.value
  
  # Store the results in the dataframe
  t_test_results <- rbind(t_test_results, data.frame(Column = col, P_Value = p_value))
}

# Print the table of results
print(t_test_results)

################################
## T Testing Cumulative Score ##
################################


# Two sample T Test for cumulative Score based on ranking seven variables

# Define the metrics for which you want to rank the teams
metrics <- c("OffRtg", "DefRtg", "OverallRtg", "DRebRate", "PointsPerPoss", "DefensivePointsPerPoss", "WinPercentage")

# Define a function to rank teams for a given metric
rank_teams <- function(data, metric) {
  if (metric %in% c("OffRtg", "OverallRtg", "PointsPerPoss", "WinPercentage", "DRebRate")) {
    # Rank in descending order
    data <- data %>%
      group_by(Season) %>%
      mutate(!!paste0(metric, "_Rank") := dense_rank(desc(.data[[metric]]))) %>%
      ungroup()
  } else {
    # Rank in ascending order
    data <- data %>%
      group_by(Season) %>%
      mutate(!!paste0(metric, "_Rank") := dense_rank(.data[[metric]])) %>%
      ungroup()
  }
  return(data)
}

# Rank teams for each metric and season
ranked_data <- merged_data
for (metric in metrics) {
  ranked_data <- rank_teams(ranked_data, metric)
}

# Calculate cumulative score
ranked_data <- ranked_data %>%
  mutate(CumulativeScore = rowSums(select(., ends_with("_Rank"))))

# View the resulting dataframe
#print(ranked_data)

# Perform two-sample t-test
t_test_result_CumulativeScore <- t.test(ranked_data$CumulativeScore ~ ranked_data$Playoffs)
t_test_result_CumulativeScore

#############################
## Modeling weight factors ##
#############################

weights <- c(WinPercentage = 40, OverallRtg = 10, DefRtg = 20, DefensivePointsPerPoss = 10, 
             PointsPerPoss = 10, OffRtg = 5, DRebRate = 5)

# Apply the weights to the ranking columns and calculate the weighted cumulative score
ranked_data <- ranked_data %>%
  mutate(WeightedScore = rowSums(select(., ends_with("_Rank")) * weights))

model <- glm(Playoffs ~ WeightedScore, data = ranked_data, family = binomial)


predictions <- predict(model, type = "response")

# Evaluate the model performance
accuracy <- mean((predictions >= 0.5 & ranked_data$Playoffs == 1) |
                   (predictions < 0.5 & ranked_data$Playoffs == 0))
print(paste("Accuracy:", accuracy))


############################
## Current season Testing ##
############################

## testing on current season data
current_season_data <- current_season_data %>%
  mutate(WeightedScore = rowSums(select(., ends_with("_Rank")) * weights))
predictions <- predict(model, newdata = current_season_data, type = "response")

# Generate new columns
current_season_data$Predicted_Playoff <- ifelse(predictions >= 0.5, 1, 0)
current_season_data$Prediction_Value <- predictions

selected_data <- current_season_data %>%
  select(TEAM, Predicted_Playoff,Prediction_Value)%>%
  arrange(desc(Prediction_Value))

# Print the resulting dataframe with team, playoff (1/0) and Prediction Value
print(selected_data, n=30)
