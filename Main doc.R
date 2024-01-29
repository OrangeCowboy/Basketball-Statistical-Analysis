#--- LIBRARIES ---
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("performance")
install.packages("see")
install.packages("patchwork")
install.packages("gganimate")
install.packages("MASS")
install.packages("coefplot")
library(tidyverse)
library(ggrepel) #This always us to stop overlapping data point labels
library(performance) #qq and homogenity
library(see)
library(patchwork)
library(gganimate)
library(MASS)
library(coefplot)
library(car)

# "Hypothesis: The pace at which a team plays (TeamPace) has a significant impact on a player's Player Efficiency Rating (PER). Specifically, players from teams with higher pace are more likely to have higher PER values due to increased opportunities for offensive and defensive plays."


#disabling cache
options(dplyr.cache = FALSE)


#--- DATA LOADING ---
nba_data <- read.csv('C:/Users/afgha/Documents/Modern Statistical Computing/2022-2023 NBA Player Stats - Regular.csv', sep = ';')

nba_data <- nba_data %>%
  filter(Tm != "TOT") # Exclude players belonging to TOT team

#--- DATA EXPLORATION ---
summary(nba_data)
str(nba_data)

#--- DATA VISUALIZATION ---
ggplot(data = nba_data, aes(x = Age, y = PTS, color = Age)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE, color = "gray50", fill = "gray30", alpha = 0.3) +
  scale_color_viridis_c() +
  xlab("Age") +
  ylab("Points Scored per Game") +
  ggtitle("Age vs. Points Scored in NBA 2022-2023 Regular Season") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

ggplot(data = nba_data, aes(x = TRB)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  xlab("Total Rebounds per Game") +
  ylab("Density") +
  ggtitle("Density Plot of Rebounds per Game") +
  theme_minimal()


ggplot() +
  geom_density(data = nba_data, aes(x = PTS, fill = "Points Scored"), alpha = 0.7) +
  geom_density(data = nba_data, aes(x = AST, fill = "Assists"), alpha = 0.7) +
  geom_vline(data = nba_data, aes(xintercept = mean(PTS)), color = "blue", linetype = "dashed", size = 1) +
  geom_vline(data = nba_data, aes(xintercept = median(PTS)), color = "red", linetype = "dashed", size = 1) +
  xlab("Points Scored / Assists per Game") +
  ylab("Density") +
  ggtitle("Density Plot of Points Scored and Assists per Game") +
  theme_minimal() +
  scale_fill_manual(values = c("Points Scored" = "skyblue", "Assists" = "orange"))


# histogram of FG%
ggplot(nba_data, aes(x = FG.)) +
  geom_histogram(bins = 20, fill = "cyan", color = "black") +
  xlab("Field Goal Percentage (FG.)") +
  ylab("Frequency") +
  ggtitle("Histogram of Field Goal Percentage (FG.)")





#--- DATA MERGING/CLEANING ---

# New data frame with pace to convert uPER to PER
team_pace_data <- data.frame(
  Tm = c("MIL", "BOS", "PHI", "DEN", "CLE", "MEM",
         "SAC", "NYK", "BRK", "PHO", "GSW", "LAC",
         "MIA", "LAL", "MIN", "NOP", "ATL", "TOR",
         "CHI", "OKC", "DAL", "UTA", "IND", "WAS",
         "ORL", "POR", "CHO", "HOU", "SAS", "DET"),
  TeamPace = c(101.45, 99.15, 97.44, 98.74, 96.27, 101.50,
               100.99, 97.75, 98.77, 98.83, 102.54, 98.84,
               96.76, 101.92, 101.55, 99.58, 101.56, 97.85,
               99.18, 101.94, 97.21, 101.02, 101.68, 99.16,
               99.66, 99.25, 101.47, 99.74, 102.07, 99.88)
)

# Merge team_pace_data with nba_data based on matching team abbreviations (Tm)
nba_data <- left_join(nba_data, team_pace_data, by = "Tm")


#--- FUNCTIONS ---

# VOP is Value of Possession: VOP represents the average number of points scored per possession
calculate_VOP <- function(data) {
  total_points <- sum(data$PTS)
  
  total_possessions <- 0.96 * (sum(data$FGA) - sum(data$ORB) + sum(data$TOV) + 0.44 * sum(data$FTA)) + sum(data$DRB)
  
  VOP <- total_points / total_possessions
  
  return(VOP)
}

VOP <- calculate_VOP(nba_data)


# Calculating all the team stats
calculate_team_stats <- function(data) {
  team_stats <- data %>%
    group_by(Tm) %>%
    summarize(team_AST = sum(AST),
              team_FG = sum(FG))
  
  return(team_stats)
}

team_stats <- calculate_team_stats(nba_data)


coefSummary= function(lmfit, level=0.95, digits=3) {
  require(tidyverse)
  if (!('lm' %in% class(lmfit))) stop('lmfit must be of class lm')
  b= round(coef(lmfit), digits)
  ci= round(confint(lmfit, level=level), digits)
  ci= paste('(',ci[,1],',',ci[,2],')',sep='')
  pval= round(summary(lmfit)$coef[,4],5)
  pval[pval < 0.00001]= '<0.00001'
  ans= tibble(names(b), b, ci, pval)
  names(ans)= c('Parameter','Estimate','Conf. Int.','P-value')
  return(ans)
}

#--- DATA CALCULATION ---

# Calculate league averages
lg_PTS <- sum(nba_data$PTS) / nrow(nba_data)
lg_FG <- sum(nba_data$FG) / nrow(nba_data)
lg_FGA <- sum(nba_data$FGA) / nrow(nba_data)
lg_FT <- sum(nba_data$FT) / nrow(nba_data)
lg_PF <- sum(nba_data$PF) / nrow(nba_data)
lg_TRB <- sum(nba_data$TRB) / nrow(nba_data)
lg_ORB <- sum(nba_data$ORB) / nrow(nba_data)
lg_DRB <- sum(nba_data$ORB) / nrow(nba_data)
lg_AST <- sum(nba_data$AST) / nrow(nba_data)
lg_FTA <- sum(nba_data$FTA) / nrow(nba_data)

# Calculate the average team pace for each unique team
team_avg_pace <- nba_data %>%
  group_by(Tm) %>%
  summarize(avg_pace = mean(TeamPace, na.rm = TRUE))

# Calculate the league average team pace
lg_TeamPace <- mean(team_avg_pace$avg_pace, na.rm = TRUE)

factor <- (2 / 3) - (0.5 * (lg_AST / lg_FG)) / (2 * (lg_FG / lg_FT))
DRB_percent <- (lg_TRB - lg_ORB) / lg_TRB


nba_data <- nba_data %>%
  mutate(uPER = (1 / MP) * (X3P + (2/3) * AST + (2 - factor * (team_stats$team_AST / team_stats$team_FG)) * FG +
                              (FT * 0.5 * (1 + (1 - (team_stats$team_AST / team_stats$team_FG)) + (2/3) * (team_stats$team_AST / team_stats$team_FG))) -
                              VOP * TOV - VOP * DRB_percent * (FGA - FG) -
                              VOP * 0.44 * (0.44 + (0.56 * DRB_percent)) * (FTA - FT) +
                              VOP * (1 - DRB_percent) * (TRB - ORB) + VOP * DRB_percent * ORB +
                              VOP * STL + VOP * DRB_percent * BLK -
                              PF * ((lg_FT / lg_PF) - 0.44 * (lg_FTA / lg_PF) * VOP)))

# Calculate the league-average uPER
lguPER <- mean(nba_data$uPER, na.rm = TRUE)

nba_data <- nba_data %>%
  mutate(PER = (uPER * lg_TeamPace / TeamPace) * 15 / lguPER)

nba_data_filtered <- nba_data %>% 
  filter(G >= 30)


# Scatter plot of PER >30 players with ggrepel for non-overlapping labels
ggplot(nba_data_filtered, aes(x = PER, y = PTS)) +
  geom_point(aes(color = PER > 30, size = PER > 30)) +
  geom_text_repel(aes(label = ifelse(PER > 30, Player, "")), 
                  box.padding = 0.5, point.padding = 0.5, size = 3,
                  max.overlaps = 50) +  # Set max.overlaps to a higher value
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "purple")) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 3.5)) +  # Adjust the size for different points
  guides(size = FALSE) +  # Hide the legend for size
  xlab("Player Efficiency Rating (PER)") +
  ylab("Points Scored per Game") +
  ggtitle("Scatter Plot of PER vs. Points Scored") +
  labs(subtitle = "Player Efficiency Rating: The most comprehensive advanced statistic in basketball, quantifying a player's value to their team") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

# Create the box plot
ggplot(nba_data_filtered, aes(y = PTS)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  ylab("Points per Game (PTS)") +
  ggtitle("Box Plot of Points per Game") +
  theme_minimal()

ggplot(nba_data_filtered, aes(y = PER)) +
  geom_boxplot(fill = "red", color = "black", alpha = 0.7) +
  ylab("Player Efficiency Rating (PER)") +
  ggtitle("Box Plot of PER") +
  theme_minimal()
# min: 5.724
# 1st Qu: 12.487
# Median: 15.176
# Mean 16.018
# 3rd Qu.: 18.677
# max.: 35.886


ggplot(nba_data_filtered, aes(x = FT.)) +
  geom_histogram(bins = 20, fill = "cyan", color = "black") +
  xlab("Free Throw Percentage (FT.)") +
  ylab("Frequency") +
  ggtitle("Histogram of Free Throw Percentage (FT.)")



#--- Linear Models ---

model1 <- lm(PTS ~ FGA + FTA + AST, data = nba_data_filtered)
coefSummary(model1)
# The estimated coefficient for FGA is approximately 1.08. This means that, on average, for each additional field goal attempt (FGA) made by a player, their points scored (PTS) increase by around 1.08 points.
# The estimated coefficient for FTA is approximately 0.911. This indicates that, on average, for each additional free throw attempt (FTA) made by a player, their points scored (PTS) increase by approximately 0.911 points.
# The estimated coefficient for AST is approximately -0.081. This means that, on average, for each additional assist (AST) made by a player, their points scored (PTS) decrease by around 0.081 points.

# The 95% confidence interval for the coefficient of FGA is (1.043, 1.111). This means that we are 95% confident that the true coefficient for FGA lies between 1.043 and 1.111.
# The 95% confidence interval for the coefficient of FTA is (0.836, 0.987). This indicates that we are 95% confident that the true coefficient for FTA lies between 0.836 and 0.987.

# The p-value for FGA is less than 0.00001 (represented as <0.00001). This indicates that the coefficient for FGA is statistically significant, and we reject the null hypothesis that there is no relationship between FGA and PTS.
# The p-value for FTA is less than 0.00001 (represented as <0.00001). This means that the coefficient for FTA is statistically significant, and we reject the null hypothesis that there is no relationship between FTA and PTS.
# The p-value for AST is 0.01076. This value is less than the common significance level of 0.05, indicating that the coefficient for AST is statistically significant, and we reject the null hypothesis that there is no relationship between AST and PTS.

anova(model1)

# The F-value for FGA is very high (23144.0805), and the associated p-value is less than 0.00001, indicating that the model including FGA is statistically significant in explaining the variability in PTS.
# The F-value for FTA is also high (562.7649), and the associated p-value is less than 0.00001, indicating that the model including FTA is statistically significant in explaining the variability in PTS.
# The F-value for AST is 6.5686, and the associated p-value is 0.01076, which is less than 0.05. This indicates that the model including AST is statistically significant, although its effect may be relatively smaller compared to FGA and FTA.

summary(model1)
# Multiple R-squared (R^2): The multiple R-squared value is 0.9839, 
# which means that approximately 98.39% of the variance in the points scored (PTS) can be explained by the predictor variables (FGA, FTA, AST) included in the model. This high R-squared value suggests that the linear model accounts for a substantial amount of the variability in the points scored by the players.

# performance::check_model(model1, check = "qq")
# performance::check_model(model1, check ="homogeneity")

qqPlot(model1, id=0.05, col="blue", main="QQ Plot")
plot(model1, which = 1) # residuals vs fitted values

#_________________________


# Fit the linear regression model with TeamPace and other predictors
PERmodel <- lm(PER ~ TeamPace + FGA + FTA + AST + Age + Pos + MP, data = nba_data_filtered)

# Summary of the model
summary(PERmodel)

# R-squared: The multiple R-squared value is 0.7955, which means that approximately 79.55% of the variance in the Player Efficiency Rating (PER) can be explained by the predictor variables (TeamPace, FGA, FTA, AST, Age, Pos, MP) included in the model. This value indicates a reasonably good fit of the model to the data.
# Adjusted R-squared: The adjusted R-squared value is 0.7902, which takes into account the number of predictor variables and the sample size. It penalizes the R-squared for including additional variables that may not contribute significantly to the model's fit.
# F-statistic: The F-statistic is 147.8 with a very low p-value (< 2.2e-16). This indicates that the overall model is statistically significant in explaining the variability in the Player Efficiency Rating (PER).
# Coefficients: Each predictor variable in the model has an associated coefficient (Estimate), standard error (Std. Error), t-value, and p-value (Pr(>|t|)). The coefficients represent the estimated effect of each predictor variable on the PER, holding other variables constant.


# Coefficient summary
coefSummary(PERmodel)

# Intercept: The intercept represents the estimated PER when all other predictor variables are zero. In this case, the intercept is 14.4, but its confidence interval (0.015, 28.812) includes zero, indicating that the true intercept might be closer to zero, and the confidence in its estimation is not high (p-value = 0.04976).
# TeamPace: The coefficient for TeamPace is 0.004 with a large confidence interval (-0.136, 0.144), and a high p-value (0.95171). This suggests that there is no statistically significant relationship between TeamPace and PER.
# FGA and FTA: The coefficients for both FGA and FTA are positive and statistically significant (p < 0.00001), indicating that as the number of field goals attempts and free throw attempts per game increases, the Player Efficiency Rating (PER) also tends to increase.
# AST: The coefficient for AST is positive and statistically significant (p < 0.00001), suggesting that more assists per game are associated with higher Player Efficiency Ratings (PER).
# Age: The coefficient for Age is positive but not statistically significant (p = 0.44312). This indicates that age does not have a significant effect on PER.
# Pos: The model includes four dummy variables representing different player positions (PosPF, PosPG, PosSF, PosSG). All four coefficients are negative and statistically significant (p < 0.00001). These coefficients suggest that players in positions PF, PG, SF, and SG tend to have lower PER values compared to the baseline position (usually Center, not explicitly shown here).
# MP: The coefficient for MP (minutes played per game) is negative and statistically significant (p < 0.00001), indicating that players who play more minutes tend to have lower Player Efficiency Ratings (PER).

# Check model assumptions
qqPlot(PERmodel, id=0.05, col="blue", main="QQ Plot")
plot(PERmodel, which = 1) # residuals vs fitted values
#When the homogeneity plot is not a straight line, it suggests that the variance of the residuals changes systematically with the predicted values, which can lead to biased and inefficient estimates.

# Perform Box-Cox transformation and find the optimal lambda
boxcox_results <- boxcox(PERmodel) # Optimal lambda is already at 1, no need for a boxcox transformation

# ANOVA to test overall significance of the model
anova(PERmodel)

# TeamPace: The F-statistic is 0.0876, and the associated p-value is 0.767357. The p-value is much greater than the typical significance level (e.g., 0.05), indicating that TeamPace is not a statistically significant predictor of PER. The model does not provide strong evidence that TeamPace has a significant impact on PER.
# FGA: The F-statistic is 997.0196, and the associated p-value is < 2.2e-16 (represented as '***'). This indicates that FGA is a highly significant predictor of PER. The extremely low p-value suggests that the coefficient for FGA is not equal to zero, and it has a significant impact on PER. Players with more field goal attempts (FGA) tend to have different PER values than those with fewer attempts.
# FTA: The F-statistic is 270.0883, and the associated p-value is < 2.2e-16 ('***'). Similar to FGA, FTA is also a highly significant predictor of PER. Players with more free throw attempts (FTA) tend to have different PER values than those with fewer attempts.
# AST: The F-statistic is 8.4777, and the associated p-value is 0.003807 ('**'). AST (assists per game) is a statistically significant predictor of PER. Players with more assists tend to have different PER values than those with fewer assists.
# Age: The F-statistic is 3.7574, and the associated p-value is 0.053316 ('.'). Age is marginally significant in predicting PER. The p-value is close to the typical significance level of 0.05, suggesting that age may have a weaker, but still potentially meaningful, impact on PER.
# Pos: The F-statistic is 37.5055, and the associated p-value is < 2.2e-16 ('***'). The categorical variable Pos (Player's position) is a highly significant predictor of PER. Different player positions have a significant impact on PER, and there are significant differences in PER among different positions.
# MP: The F-statistic is 49.0441, and the associated p-value is 1.142e-11 ('***'). MP (minutes played per game) is a highly significant predictor of PER. Players with more playing time tend to have different PER values than those with less playing time.
# Residuals: This represents the error term, the unexplained variability in PER not accounted for by the predictor variables in the model.

# Create the coefficient plot
coefplot(PERmodel)


#---- better fit

# Create scatter plots of PER against each predictor
predictors <- c("TeamPace", "FGA", "FTA", "AST", "Age", "MP") # Assuming these are your predictors
plots_per_predictor <- lapply(predictors, function(var) {
  ggplot(nba_data_filtered, aes_string(x = var, y = "PER")) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(title = paste("Scatter plot of PER against", var))
})

# Print the scatter plots
print(plots_per_predictor)

nba_data_filtered$residuals <- residuals(PERmodel)

# Create scatter plots of residuals against each predictor
plots_residuals_predictor <- lapply(predictors, function(var) {
  ggplot(nba_data_filtered, aes_string(x = var, y = "residuals")) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residuals vs", var))
})

# Print the residual plots
print(plots_residuals_predictor) #
