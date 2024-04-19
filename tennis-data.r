library(dplyr)
library(ggplot2)
library(tidyr)

year <- "2022" ## SET YEAR
player1_name <- "Carlos Alcaraz" ## CHANGE PLAYER NAMES HERE (Can also be "Average", "Minimum", or "Maximum")
player2_name <- "Average"

data <- read.csv(paste("./atp_data/atp_matches_", year, ".csv", sep = ""))
filtered_data <- data

## WINNER
aggregate_data <- aggregate(winner_rank ~ winner_name, data = filtered_data, FUN = min)
merged_data <- merge(filtered_data, aggregate_data, by = c("winner_name", "winner_rank"), all.x = TRUE)
final_data <- subset(merged_data, select = c("winner_name", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_bpSaved", "w_bpFaced"))

final_data$firstWon_perc <- final_data$w_1stWon / final_data$w_1stIn
final_data$secondWon_perc <- final_data$w_2ndWon / (final_data$w_svpt - final_data$w_df - final_data$w_1stIn)
final_data$df_perc <- final_data$w_df / final_data$w_svpt
final_data$ace_perc <- final_data$w_ace / final_data$w_svpt
final_data$bpWon_perc <- final_data$w_bpSaved / final_data$w_bpFaced

final_data <- final_data[complete.cases(final_data), ]
final_data_selected <- final_data %>%
  select(winner_name, firstWon_perc, secondWon_perc, df_perc, ace_perc, bpWon_perc)


## LOSER
aggregate_data_loser <- aggregate(loser_rank ~ loser_name, data = filtered_data, FUN = min)
merged_data_loser <- merge(filtered_data, aggregate_data_loser, by = c("loser_name", "loser_rank"), all.x = TRUE)
final_data_loser <- subset(merged_data_loser, select = c("loser_name", "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_bpSaved", "l_bpFaced"))

final_data_loser$firstWon_perc <- final_data_loser$l_1stWon / final_data_loser$l_1stIn
final_data_loser$secondWon_perc <- final_data_loser$l_2ndWon / (final_data_loser$l_svpt - final_data_loser$l_df - final_data_loser$l_1stIn)
final_data_loser$df_perc <- final_data_loser$l_df / final_data_loser$l_svpt
final_data_loser$ace_perc <- final_data_loser$l_ace / final_data_loser$l_svpt
final_data_loser$bpWon_perc <- final_data_loser$l_bpSaved / final_data_loser$l_bpFaced

final_data_loser <- final_data_loser[complete.cases(final_data_loser), ]
final_data_loser_selected <- final_data_loser %>%
  select(loser_name, firstWon_perc, secondWon_perc, df_perc, ace_perc, bpWon_perc)


## COMBINE
final_data_loser_selected <- final_data_loser_selected %>%
  rename(
    winner_name = loser_name,
    firstWon_perc = firstWon_perc,
    secondWon_perc = secondWon_perc,
    df_perc = df_perc,
    ace_perc = ace_perc,
    bpWon_perc = bpWon_perc
  )
combined_data <- bind_rows(final_data_selected, final_data_loser_selected)
grouped_data <- combined_data %>%
  group_by(winner_name) %>%
  summarise_all(mean, na.rm = TRUE)

averages <- colMeans(grouped_data[, -1], na.rm = TRUE)
average_row <- data.frame(winner_name = "Average", t(averages))
grouped_data_with_average <- rbind(average_row, grouped_data)

min_row <- grouped_data_with_average %>%
  summarise_all(~ ifelse(. == "Average", NA, min(., na.rm = TRUE))) %>% slice(1)
max_row <- grouped_data_with_average %>%
  summarise_all(~ ifelse(. == "Average", NA, max(., na.rm = TRUE))) %>% slice(1)

min_row$winner_name <- "Minimum"
max_row$winner_name <- "Maximum"

final_data_with_extremes <- bind_rows(min_row, max_row, grouped_data_with_average) ## FINAL DATA SET


## BAR CHART
barchart_title <- paste(player1_name, " vs. ", player2_name, " Statistics (", year, ")", sep = "")

player1_row <- final_data_with_extremes[final_data_with_extremes$winner_name == player1_name, ]
player2_row <- final_data_with_extremes[final_data_with_extremes$winner_name == player2_name, ]

reshape_data <- function(data) {
  data %>%
    pivot_longer(cols = -winner_name, names_to = "Statistic") %>%
    mutate(Statistic = factor(Statistic, levels = c("firstWon_perc", "secondWon_perc", "df_perc", "ace_perc", "bpWon_perc")))
}

player1_data <- reshape_data(player1_row)
player2_data <- reshape_data(player2_row)

combined_plot_data <- rbind(player1_data, player2_data)
combined_plot_data$type <- rep(c(player1_name, player2_name), each = nrow(player1_data))

ggplot(combined_plot_data, aes(x = Statistic, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(value * 100, 2), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3, color = "black") +
  labs(title = barchart_title,
       x = "Statistic", y = "Percentage", fill = "") +
  scale_fill_manual(values = c("darkred", "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete(labels = c(
    "firstWon_perc" = "Win on First Serve",
    "secondWon_perc" = "Win on Second Serve",
    "df_perc" = "Double Fault",
    "ace_perc" = "Ace",
    "bpWon_perc" = "Break Point Save"
  ))