library(tidyverse)   # dplyr, tidyr, tibble, ggplot2
library(ggrepel)
library(knitr)
library(car)
library(randomForest)
# Function: Run regression and plot
plotmvpos <- function(df, season_label) {
  model <- lm(leaguepos ~ totalmarketvalm, data = df)
  print(paste("=== Season:", season_label, "==="))
  print(summary(model))
  p <- ggplot(df, aes(x = totalmarketvalm, y = leaguepos)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    geom_text(aes(label = Club), vjust = -0.5, size = 3, check_overlap = TRUE) +
    labs(
      title = paste("Premier League", season_label, ": Market Value vs League Position"),
      x = "Total Market Value (Million Euros)",
      y = "Final League Position (1 = Champion)",
      caption = "Red line: Linear regression"
    ) +
    theme_minimal() +
    scale_y_reverse(breaks = 1:20)
  return(p)
}

# 2024–25 Data
clubdata45 <- data.frame(Club = c("Manchester City", "Arsenal FC", "Liverpool FC", "Chelsea FC", 
                                  "Tottenham Hotspur", "Newcastle United", "Manchester United", "Aston Villa", 
                                  "Brighton & Hove Albion", "AFC Bournemouth", "Nottingham Forest", 
                                  "Crystal Palace", "Brentford FC", "Wolverhampton Wanderers", 
                                  "West Ham United", "Fulham FC", "Everton FC", "Ipswich Town", 
                                  "Southampton FC", "Leicester City"),
                         totalmarketvalm = c(1180, 1100, 942.5, 913.8, 766.1, 651.48, 643.1, 592, 555.7,
                                             470.1, 462.15, 458, 414.48, 413.9, 400.3, 351.35, 349.55, 261.5, 256.3, 242.4),
                         leaguepos = c(3, 2, 1, 4, 17, 5, 15, 6, 8, 10, 7, 12, 9, 16, 14, 11, 13, 19, 20, 18))

# 2023–24 Data
clubdata2324 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Chelsea FC", "Liverpool FC", 
           "Tottenham Hotspur", "Manchester United", "Aston Villa", "Newcastle United", 
           "Brighton & Hove Albion", "Nottingham Forest", "West Ham United", 
           "Crystal Palace", "Wolverhampton Wanderers", "Brentford FC", 
           "AFC Bournemouth", "Everton FC", "Fulham FC", "Burnley FC", 
           "Sheffield United", "Luton Town"),
  totalmarketvalm = c(1460, 1200, 1010, 955.65, 838.50, 808.35, 709.65, 652.25, 524.40, 
                      501.35, 487.50, 446.58, 442.73, 428.78, 401.75, 391.75, 384.35, 273.48, 
                      152.35, 140.90),
  leaguepos = c(1, 2, 6, 3, 5, 8, 4, 7, 11, 17, 9, 10, 15, 16, 12, 13, 14, 19, 20, 18)
)

# 2022–23 Full Data
clubdata2223 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Chelsea FC", "Manchester United", "Liverpool FC",
           "Tottenham Hotspur", "Newcastle United", "Brighton & Hove Albion", "Aston Villa",
           "Wolverhampton Wanderers", "Leicester City", "West Ham United", "Southampton FC",
           "Everton FC", "Nottingham Forest", "Brentford FC", "Leeds United",
           "Crystal Palace", "Fulham FC", "AFC Bournemouth"),
  totalmarketvalm = c(1150, 1000, 994.95, 847.75, 811.85, 649.10, 541.60, 529.80, 509.55, 497.65,
                      490.70, 465.60, 419.95, 413.15, 376.25, 371.20, 345.15, 323.05, 295.10, 287.20),
  leaguepos = c(1, 2, 12, 3, 5, 8, 4, 6, 7, 13, 18, 14, 20, 17, 16, 9, 19, 11, 10, 15)
)

# We Run all 4 plots & regressions
dir.create("plots", showWarnings = FALSE)
p_mv_24_25 <- plotmvpos(clubdata45, "2024–25")
ggsave(filename = "plots/market_vs_pos_2024-25.png", plot = p_mv_24_25, width = 8, height = 6, dpi = 300)
p_mv_23_24 <- plotmvpos(clubdata2324, "2023–24")
ggsave(filename = "plots/market_vs_pos_2023-24.png", plot = p_mv_23_24, width = 8, height = 6, dpi = 300)
p_mv_22_23 <- plotmvpos(clubdata2223, "2022–23")
ggsave(filename = "plots/market_vs_pos_2022-23.png", plot = p_mv_22_23, width = 8, height = 6, dpi = 300)
p_mv_22_23_noche <- plotmvpos(clubdata2223 %>% filter(Club != "Chelsea FC"), "2022–23 (No Chelsea)")
ggsave(filename = "plots/market_vs_pos_2022-23_no_chelsea.png", plot = p_mv_22_23_noche, width = 8, height = 6, dpi = 300)

# Data: 2022–23 
clubdata2223 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Chelsea FC", "Manchester United", "Liverpool FC",
           "Tottenham Hotspur", "Newcastle United", "Brighton & Hove Albion", "Aston Villa",
           "Wolverhampton Wanderers", "Leicester City", "West Ham United", "Southampton FC",
           "Everton FC", "Nottingham Forest", "Brentford FC", "Leeds United",
           "Crystal Palace", "Fulham FC", "AFC Bournemouth"),
  leaguepos = c(1, 2, 12, 3, 5, 8, 4, 6, 7, 13, 18, 14, 20, 17, 16, 9, 19, 11, 10, 15),
  Grosspygbp = c(199160000, 133016000, 226564000, 242190000, 170820000, 117000000, 92326000,
                 44876000, 110390000, 67782000, 84760000, 88920000, 62296000, 86372000,
                 76750000, 38376000, 61074000, 70460000, 62400000, 50804000))

#  Data: 2023–24 
clubdata2324 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Chelsea FC", "Liverpool FC", 
           "Tottenham Hotspur", "Manchester United", "Aston Villa", "Newcastle United", 
           "Brighton & Hove Albion", "Nottingham Forest", "West Ham United", 
           "Crystal Palace", "Wolverhampton Wanderers", "Brentford FC", 
           "AFC Bournemouth", "Everton FC", "Fulham FC", "Burnley FC", 
           "Sheffield United", "Luton Town"),
  leaguepos = c(1, 2, 6, 3, 5, 8, 4, 7, 11, 17, 9, 10, 15, 16, 12, 13, 14, 19, 20, 18),
  Grosspygbp = c(196872000, 171496000, 157170000, 141960000, 132210000, 198346000, 
                 115070000, 88140000, 65520000, 73740000, 104676000, 73580000, 
                 67782000, 46722000, 57330000, 81318000, 67990000, 40846000, 35360000, 26182000))

# Data: 2024–25
clubdata45 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Liverpool FC", "Chelsea FC", 
           "Tottenham Hotspur", "Newcastle United", "Manchester United", "Aston Villa", 
           "Brighton & Hove Albion", "AFC Bournemouth", "Nottingham Forest", 
           "Crystal Palace", "Brentford FC", "Wolverhampton Wanderers", 
           "West Ham United", "Fulham FC", "Everton FC", "Ipswich Town", 
           "Southampton FC", "Leicester City"),
  leaguepos = c(3, 2, 1, 4, 17, 5, 15, 6, 8, 10, 7, 12, 9, 16, 14, 11, 13, 19, 20, 18),
  Grosspygbp = c(224733600, 169936000, 148493800, 169925600, 116181000, 95175600, 
                 173823000, 130761800, 64259000, 53898000, 58006000, 73684000, 44343000, 
                 63440000, 101738000, 71526000, 73788000, 40768000, 50492000, 66690000))

# Regression Models 
reg_22_23 <- lm(leaguepos ~ Grosspygbp, data = clubdata2223)
reg_23_24 <- lm(leaguepos ~ Grosspygbp, data = clubdata2324)
reg_24_25 <- lm(leaguepos ~ Grosspygbp, data = clubdata45)

#  Regression Summaries 
summary(reg_22_23)
summary(reg_23_24)
summary(reg_24_25)

# Plot Function 
leaguewage <- function(data, year_label) {
  p <- ggplot(data, aes(x = Grosspygbp, y = leaguepos, label = Club)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "gray70") +
    geom_text_repel(size = 3, max.overlaps = 100) +
    scale_y_reverse(breaks = 1:20) +
    labs(
      title = paste0("Premier League ", year_label, ": Wage Bill vs League Position"),
      x = "Gross Annual Wage Bill (GBP)",
      y = "Final League Position (1 = Champion)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank()
    )
  return(p)
}

# Plots 
p_wage_22_23 <- leaguewage(clubdata2223, "2022–23")
ggsave(filename = "plots/wage_vs_pos_2022-23.png", plot = p_wage_22_23, width = 8, height = 6, dpi = 300)
p_wage_23_24 <- leaguewage(clubdata2324, "2023–24")
ggsave(filename = "plots/wage_vs_pos_2023-24.png", plot = p_wage_23_24, width = 8, height = 6, dpi = 300)
p_wage_24_25 <- leaguewage(clubdata45, "2024–25")
ggsave(filename = "plots/wage_vs_pos_2024-25.png", plot = p_wage_24_25, width = 8, height = 6, dpi = 300)

# Data: 2022–23 season
clubdata2223 <- data.frame(
  Club = c(
    "Manchester City", "Arsenal FC", "Chelsea FC", "Manchester United", "Liverpool FC",
    "Tottenham Hotspur", "Newcastle United", "Brighton & Hove Albion", "Aston Villa",
    "Wolverhampton Wanderers", "Leicester City", "West Ham United", "Southampton FC",
    "Everton FC", "Nottingham Forest", "Brentford FC", "Leeds United",
    "Crystal Palace", "Fulham FC", "AFC Bournemouth" ),
  leaguepos = c(1, 2, 12, 3, 5, 8, 4, 6, 7, 13, 18, 14, 20, 17, 16, 9, 19, 11, 10, 15),
  nettspendM = c(
    7.17, -162.60, -562.39, -219.63, -65.10, -141.15, -170.94, 82.70, -59.00,
    -116.64, 32.40, -175.05, -147.61, 20.53, -193.35, -45.65, -49.39, -41.15,
    -50.70, -83.10))

# Data 2023–24 season
clubdata2324 <- data.frame(
  Club = c(
    "Manchester City", "Arsenal FC", "Chelsea FC", "Liverpool FC", 
    "Tottenham Hotspur", "Manchester United", "Aston Villa", "Newcastle United", 
    "Brighton & Hove Albion", "Nottingham Forest", "West Ham United", 
    "Crystal Palace", "Wolverhampton Wanderers", "Brentford FC", 
    "AFC Bournemouth", "Everton FC", "Fulham FC", "Burnley FC", 
    "Sheffield United", "Luton Town"),
  leaguepos = c(1, 2, 6, 3, 5, 8, 4, 7, 11, 17, 9, 10, 15, 16, 12, 13, 14, 19, 20, 18 ),
  nettspendM = c(
    -120.80, -165.90, -181.90, -111.30, -151.40, -152.36, -78.34, -103.50, 
    82.15, -44.88, 28.30, -67.80, 75.20, -62.10, -125.79, 42.30, -19.97, 
    -107.25, -36.05, -25.60 ))

# Data: 2024–25 season
clubdata45 <- data.frame(
  Club = c(
    "Manchester City", "Arsenal FC", "Liverpool FC", "Chelsea FC", 
    "Tottenham Hotspur", "Newcastle United", "Manchester United", "Aston Villa", 
    "Brighton & Hove Albion", "AFC Bournemouth", "Nottingham Forest", 
    "Crystal Palace", "Brentford FC", "Wolverhampton Wanderers", 
    "West Ham United", "Fulham FC", "Everton FC", "Ipswich Town", 
    "Southampton FC", "Leicester City"
  ),
  leaguepos = c(3, 2, 1, 4, 17, 5, 15, 6, 8, 10, 7, 12, 9, 16, 14, 11, 13, 19, 20, 18 ),
  nettspendM = c(
    -102.30, -25.11, 5.00, -40.60, -120.05, 21.35, -143.30, 42.97,
    -231.46, -65.22, -18.80, 8.80, -23.90, -8.21, -98.25, -22.75, 
    33.95, -150.23, -80.56, -43.80 ))

# Function to plot
nettspendvpos <- function(data, season_label) {
  p <- ggplot(data, aes(x = nettspendM, y = leaguepos)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", color = "red", fill = "gray", alpha = 0.3) +
    geom_text_repel(aes(label = Club), size = 3.2) +
    scale_y_reverse(breaks = 1:20) +  # Top position is 1
    labs(
      title = paste("Premier League", season_label, ": Net Spend vs League Position"),
      x = "Net Spend (Million GBP)",
      y = "Final League Position (1 = Champion)"
    ) +
    theme_minimal()
  return(p)
}

# Run models and plot for all 3 seasons
model_22_23 <- lm(leaguepos ~ nettspendM, data = clubdata2223)
print(summary(model_22_23))
# plots and saves for net spend vs pos
p_nett_22_23 <- nettspendvpos(clubdata2223, "2022–23")
ggsave(filename = "plots/netspend_vs_pos_2022-23.png", plot = p_nett_22_23, width = 8, height = 6, dpi = 300)

model_23_24 <- lm(leaguepos ~ nettspendM, data = clubdata2324)
print(summary(model_23_24))
p_nett_23_24 <- nettspendvpos(clubdata2324, "2023–24")
ggsave(filename = "plots/netspend_vs_pos_2023-24.png", plot = p_nett_23_24, width = 8, height = 6, dpi = 300)

model_24_25 <- lm(leaguepos ~ nettspendM, data = clubdata45)
print(summary(model_24_25))
p_nett_24_25 <- nettspendvpos(clubdata45, "2024–25")
ggsave(filename = "plots/netspend_vs_pos_2024-25.png", plot = p_nett_24_25, width = 8, height = 6, dpi = 300)
# DATA 
# 2022–23
clubdata2223 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Chelsea FC", "Manchester United", "Liverpool FC",
           "Tottenham Hotspur", "Newcastle United", "Brighton & Hove Albion", "Aston Villa",
           "Wolverhampton Wanderers", "Leicester City", "West Ham United", "Southampton FC",
           "Everton FC", "Nottingham Forest", "Brentford FC", "Leeds United",
           "Crystal Palace", "Fulham FC", "AFC Bournemouth"),
  totalmarketvalm = c(1150, 1000, 994.95, 847.75, 811.85, 649.10, 541.60, 529.80, 509.55, 497.65,
                      490.70, 465.60, 419.95, 413.15, 376.25, 371.20, 345.15, 323.05, 295.10, 287.20),
  leaguepos = c(1, 2, 12, 3, 5, 8, 4, 6, 7, 13, 18, 14, 20, 17, 16, 9, 19, 11, 10, 15),
  Grosspygbp = c(199160000, 133016000, 226564000, 242190000, 170820000, 117000000, 92326000, 
                 44876000, 110390000, 67782000, 84760000, 88920000, 62296000, 86372000, 
                 76750000, 38376000, 61074000, 70460000, 62400000, 50804000),
  nettspendM = c(7.17, -162.60, -562.39, -219.63, -65.10, -141.15, -170.94, 82.70, -59.00,
                 -116.64, 32.40, -175.05, -147.61, 20.53, -193.35, -45.65, -49.39, -41.15,
                 -50.70, -83.10))

# 2023–24
clubdata2324 <- data.frame(
  Club = c("Manchester City", "Arsenal FC", "Chelsea FC", "Liverpool FC", 
           "Tottenham Hotspur", "Manchester United", "Aston Villa", "Newcastle United", 
           "Brighton & Hove Albion", "Nottingham Forest", "West Ham United", 
           "Crystal Palace", "Wolverhampton Wanderers", "Brentford FC", 
           "AFC Bournemouth", "Everton FC", "Fulham FC", "Burnley FC", 
           "Sheffield United", "Luton Town"),
  totalmarketvalm = c(1460, 1200, 1010, 955.65, 838.50, 808.35, 709.65, 652.25, 524.40, 
                      501.35, 487.50, 446.58, 442.73, 428.78, 401.75, 391.75, 384.35, 
                      273.48, 152.35, 140.90),
  leaguepos = c(1, 2, 6, 3, 5, 8, 4, 7, 11, 17, 9, 10, 15, 16, 12, 13, 14, 19, 20, 18),
  Grosspygbp = c(196872000, 171496000, 157170000, 141960000, 132210000, 198346000, 
                 115070000, 88140000, 65520000, 73740000, 104676000, 73580000, 
                 67782000, 46722000, 57330000, 81318000, 67990000, 40846000, 35360000, 26182000),
  nettspendM = c(-120.80, -165.90, -181.90, -111.30, -151.40, -152.36, -78.34, -103.50, 
                 82.15, -44.88, 28.30, -67.80, 75.20, -62.10, -125.79, 42.30, -19.97, 
                 -107.25, -36.05, -25.60))

# 2024–25
clubdata45 <- data.frame(
  Club = c(
    "Manchester City", "Arsenal FC", "Liverpool FC", "Chelsea FC", 
    "Tottenham Hotspur", "Newcastle United", "Manchester United", "Aston Villa", 
    "Brighton & Hove Albion", "AFC Bournemouth", "Nottingham Forest", 
    "Crystal Palace", "Brentford FC", "Wolverhampton Wanderers", 
    "West Ham United", "Fulham FC", "Everton FC", "Ipswich Town", 
    "Southampton FC", "Leicester City"
  ),
  totalmarketvalm = c(
    1180, 1100, 942.5, 913.8, 766.1, 651.48, 643.1, 592, 555.7,
    470.1, 462.15, 458, 414.48, 413.9, 400.3, 351.35, 349.55, 261.5, 256.3, 242.4
  ),
  leaguepos = c(
    3, 2, 1, 4, 17, 5, 15, 6, 8, 10, 7, 12, 9, 16, 14, 11, 13, 19, 20, 18
  ),
  Grosspygbp = c(
    224733600, 169936000, 148493800, 169925600, 116181000, 95175600, 
    173823000, 130761800, 64259000, 53898000, 58006000, 73684000, 44343000, 
    63440000, 101738000, 71526000, 73788000, 40768000, 50492000, 66690000
  ),
  nettspendM = c(
    -102.30, -25.11, 5.00, -40.60, -120.05, 21.35, -143.30, 42.97,
    -231.46, -65.22, -18.80, 8.80, -23.90, -8.21, -98.25, -22.75, 
    33.95, -150.23, -80.56, -43.80 ))

# combining the training set
clubdata2223$Season <- "2022_23"
clubdata2324$Season <- "2023_24"
training_data <- bind_rows(clubdata2223, clubdata2324)

# Linear Regression for All Predictors 
lm_model <- lm(leaguepos ~ totalmarketvalm + nettspendM + Grosspygbp, data = training_data)
summary(lm_model)

# checking for collinearity 
vif_values <- vif(lm_model)
print(vif_values)

# linear regression with just market value 
lm_model <- lm(leaguepos ~ totalmarketvalm, data = training_data)
summary(lm_model)

# linear regression - Constant RMSE for Probabilities
# Compute RMSE from training residuals
linear_residuals <- lm_model$residuals
linear_rmse <- sqrt(mean(linear_residuals^2))

# Compute probabilities for 2024–25 predictions using constant RMSE
clubdata45$Predicted_Position <- predict(lm_model, clubdata45)
clubdata45 <- clubdata45 %>%
  mutate(Top6_Prob = round(100 * pnorm(6.5, Predicted_Position, linear_rmse)), Top4_Prob = round(100 * pnorm(4.5, Predicted_Position, linear_rmse)),
         Relegation_Prob = round(100 * pnorm(17.5, Predicted_Position, linear_rmse, lower.tail = FALSE)))

# linear table + plots 
cat(" Linear regression predictions \n")
print(kable(clubdata45 %>%
              select(Club, leaguepos, Predicted_Position, Top6_Prob) %>%
              arrange(Predicted_Position),
            digits = 1,
            col.names = c("Club", "Actual", "Linear Pred", "Top6%")))

ggplot(clubdata45, aes(x = leaguepos, y = Predicted_Position, label = Club)) +
  geom_point(color = "blue", size = 3) +
  geom_text_repel(size = 3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + scale_y_reverse(breaks = 1:20) + scale_x_reverse(breaks = 1:20) +
  labs(title = "Linear Regression: Predicted vs Actual (2024–25)",x = "Actual Position", y = "Predicted Position") +theme_minimal()

# Running Random Forest
rf_model_full <- randomForest(leaguepos ~ totalmarketvalm + Grosspygbp + nettspendM, data = training_data, ntree = 500, importance = TRUE)

clubdata45$RF_Predicted_Position <- predict(rf_model_full, clubdata45)
prediction_errors <- rep(sqrt(rf_model_full$mse[length(rf_model_full$mse)]), nrow(clubdata45))
clubdata45 <- clubdata45 %>%
  mutate(RF_Top4_Prob = round(100 * pnorm(4.5, RF_Predicted_Position, prediction_errors)), RF_Top6_Prob = round(100 * pnorm(6.5, RF_Predicted_Position, prediction_errors)), RF_Relegation_Prob = round(100 * pnorm(17.5, RF_Predicted_Position, prediction_errors, lower.tail = FALSE)))

# RF variable importance 
importance_rf <- importance(rf_model_full, type = 1)
importance_rf_df <- as.data.frame(importance_rf) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`))
cat("\n Random Forest: % Increase in MSE \n")
print(kable(importance_rf_df, digits = 2))


# We reran Rf without net spend 
rf_model_nospend <- randomForest(
  leaguepos ~ totalmarketvalm + Grosspygbp,  data = training_data,  ntree = 500,importance = TRUE)

# Predict for 2024–25
clubdata45$RF_Predicted_NoSpend <- predict(rf_model_nospend, clubdata45)
rf_se_nospend <- rep(sqrt(rf_model_nospend$mse[length(rf_model_nospend$mse)]), nrow(clubdata45))

#  recomputing the probabilities
clubdata45 <- clubdata45 %>%
  mutate(RF_Top4_Prob_NoSpend = round(100 * pnorm(4.5, RF_Predicted_NoSpend, rf_se_nospend)), RF_Top6_Prob_NoSpend = round(100 * pnorm(6.5, RF_Predicted_NoSpend, rf_se_nospend)), RF_Relegation_Prob_NoSpend = round(100 * pnorm(17.5, RF_Predicted_NoSpend, rf_se_nospend, lower.tail = FALSE)))
cat("\n Random Foresting (No Net Spend): Prediction \n")
print(kable(
  clubdata45 %>%
    select(Club, leaguepos, RF_Predicted_NoSpend, RF_Top6_Prob_NoSpend) %>%
    rename(Actual = leaguepos,`RF Pred` = RF_Predicted_NoSpend, `Top6%` = RF_Top6_Prob_NoSpend ) %>% arrange(`RF Pred`),  digits = 1))

# (No Net Spend) RF plots vs actual league placement
ggplot(clubdata45, aes(x = leaguepos, y = RF_Predicted_NoSpend, label = Club)) +
  geom_point(color = "blue", size = 3) +
  geom_text_repel(size = 3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_reverse(breaks = 1:20) +
  scale_x_reverse(breaks = 1:20) +
  labs( title = "Random Forest (No Net Spend): Predicted vs Actual (2024–25)",
        x = "Actual Position",
        y = "Predicted Position (RF No Net Spend)" ) +theme_minimal()  
# combining the tables for better comparison 
comparison_full <- clubdata45 %>%
  transmute( Club, Actual = leaguepos,
             Linear = Predicted_Position,
             RF_NoSpend = RF_Predicted_NoSpend,
             Linear_Top6_Percent = Top6_Prob,
             RF_Top6_Percent = RF_Top6_Prob_NoSpend,
             Linear_Residual = Actual - Linear,
             RF_Residual = Actual - RF_NoSpend )

cat("FULL MODEL COMPARISON TABLE (WITH %TOP6 & RESIDUALS)")
print(kable(
  comparison_full %>% arrange(Linear),
  digits = 1,
  col.names = c("Club", "Actual", "Linear", "RF NoSpend", "Linear Top6%", "RF Top6%", "Linear Resid", "RF Resid")
))

# All residuals 
total_linear_resid <- sum(abs(comparison_full$Linear_Residual))
total_rf_resid <- sum(abs(comparison_full$RF_Residual))
cat("\nTotal Absolute Residuals:\n")
cat(sprintf("Linear Model: %.1f\n", total_linear_resid))
cat(sprintf("Random Forest (No Net Spend): %.1f\n", total_rf_resid))

# comparison (Linear vs RF No Net Spend)
comparison_long <- clubdata45 %>%
  select(Club, leaguepos, Predicted_Position, RF_Predicted_NoSpend) %>%
  rename(Actual = leaguepos, Linear = Predicted_Position, RF = RF_Predicted_NoSpend) %>%
  pivot_longer(cols = c(Linear, RF), names_to = "Model", values_to = "Predicted")

# color and shape schemes
model_colors <- c("Linear" = "darkblue", "RF" = "forestgreen")
model_shapes <- c("Linear" = 16, "RF" = 17)

# Combined Plot
ggplot(comparison_long, aes(x = Actual, y = Predicted, label = Club)) +
  geom_point(aes(color = Model, shape = Model), size = 3) +
  geom_text_repel(aes(color = Model), size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_reverse(breaks = 1:20) +
  scale_x_reverse(breaks = 1:20) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  labs( title = "2024–25: Predicted vs Actual League Positions", subtitle = "Linear (Market Value Only) vs RF (No Net Spend)", x = "Actual Position",  y = "Predicted Position", color = "Model",shape = "Model" ) + theme_minimal() 