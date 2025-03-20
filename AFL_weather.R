library(fitzRoy)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(knitr)
library(readr)
####################################################################################################################################
afl_weather <- fetch_results_afl(2018)
colnames(afl_weather)

afl_weather <- afl_weather %>% 
  mutate(
    weather_desc = weather.description,
    temperature = weather.tempInCelsius, 
    weather_type = weather.weatherType,
    date = as.Date(match.date, format = '%d/%m/%y'),
    season = year(date), 
    match_id = match.matchId,
    round = match.round,
    home_team = match.homeTeam.name,
    away_team = match.awayTeam.name
  ) %>% 
  select(season, date, match_id, round, home_team, 
         away_team, weather_desc, temperature, weather_type) 

afl_weather <- afl_weather %>%
  mutate(
    is_wet = ifelse(
      str_detect(str_to_lower(weather_desc), 
                 "rain|shower|drizzle|storm|thunderstorm|windy rain|
                 rain developing|rain easing|rain periods|possible shower|showers"), 
      1,  # Assign 1 if wet and rainy
      0   # Assign 0 otherwise
    )
  ) %>% select(
    season, date, match_id, round, home_team, away_team, is_wet
  )

afl_results <- fetch_results_afl(2018)
colnames(afl_results)
afl_results <- afl_results %>% 
  mutate(
    date = as.Date(match.date, format = '%d/%m/%y'),
    match_id = match.matchId,
    round = match.round,
    home_team = match.homeTeam.name,
    away_team = match.awayTeam.name,
    home_goals = homeTeamScore.matchScore.goals,
    home_behinds = homeTeamScore.matchScore.behinds,
    home_score = homeTeamScore.matchScore.totalScore,
    away_score = awayTeamScore.matchScore.totalScore,
    away_goals = awayTeamScore.matchScore.goals,
    away_behinds = awayTeamScore.matchScore.behinds,
    season = year(date)  
  ) %>% 
  select(
    season, date, match_id, round, home_team, home_goals, home_behinds,
    home_score, away_team, away_goals, away_behinds, away_score
  )

afl_results <- afl_results %>%  
  mutate(
    home_win = ifelse(
      home_score > away_score,  # Added missing comma
      1,  # Assign 1 if home team wins
      0   # Assign 0 otherwise
    )
  ) %>% 
  select(
    season, date, match_id, round, home_team, home_goals, home_behinds,
    home_score, away_team, away_goals, away_behinds, away_score, home_win 
  )

str(afl_results)
str(afl_weather )
afl_results <- afl_results %>% mutate(match_id = as.character(match_id))
afl_weather <- afl_weather %>% mutate(match_id = as.character(match_id))

afl_results_weather <- afl_results %>% 
  left_join(afl_weather, by = "match_id") %>% mutate(
    season = season.x,
    date = date.x,
    round = round.x,
    home_team = home_team.x,
    away_team = away_team.x
  ) %>% select(
    season, date, round, match_id, home_team, home_goals, home_behinds, home_score, away_team, away_goals, away_behinds,
    away_score, home_win, is_wet
  )

afl_results_weather <- afl_results_weather %>%
  mutate(
    round = as.numeric(str_sub(round, -2, -1))  
  ) %>%
  select(
    season, date, round, match_id, home_team, home_goals, home_behinds, home_score, 
    away_team, away_goals, away_behinds, away_score, home_win, is_wet
  )

# Checks
missing_values <- colSums(is.na(afl_results_weather))
print(missing_values)

missing_match_id_count <- sum(is.na(afl_results_weather$match_id))
print(missing_match_id_count)

duplicate_match_ids <- afl_results_weather %>% 
  count(match_id) %>% 
  filter(n > 1)
print(duplicate_match_ids)

str(afl_results_weather)
####################################################################################################################################
afl_ladder <- fetch_ladder_afl(2018, round = 1:23)
colnames(afl_ladder)
afl_ladder <- afl_ladder %>% mutate(
  round = round_number,
  team = team.name
) %>% select(
  season, round, team, position
)

afl_results_weather <- afl_results_weather %>%
  left_join(afl_ladder, by = c("season", "round", "home_team" = "team")) %>%
  rename(home_ladder_position = position)  

afl_results_weather <- afl_results_weather %>%
  left_join(afl_ladder, by = c("season", "round", "away_team" = "team")) %>%
  rename(away_ladder_position = position) %>% select(
    season, date, round, match_id, home_team, home_ladder_position, home_goals, home_behinds, home_score,
    away_team, away_ladder_position, away_goals, away_behinds, away_score, home_win, is_wet
  )
####################################################################################################################################
# Does wet weather affect total score - does it affect goals v behinds

# Summary when is_wet = 1 or 0

score_summary <- afl_results_weather %>%
  group_by(is_wet) %>%
  summarize(
    avg_total_score = mean(home_score + away_score),
    sd_total_score = sd(home_score + away_score),
    median_total_score = median(home_score + away_score),
    n_games = n()
  )

t_test_total <- t.test(
  (home_score + away_score) ~ is_wet, 
  data = afl_results_weather
)
print(t_test_total)

# Summary for goals and behinds

goals_behinds_summary <- afl_results_weather %>%
  group_by(is_wet) %>%
  summarize(
    total_goals = sum(home_goals + away_goals),
    total_behinds = sum(home_behinds + away_behinds),
    goals_per_game = mean(home_goals + away_goals),
    behinds_per_game = mean(home_behinds + away_behinds),
    goals_to_behinds_ratio = total_goals / total_behinds,
    n_games = n()
  )

t_test_goals <- t.test(
  (home_goals + away_goals) ~ is_wet, 
  data = afl_results_weather
)
print(t_test_goals)

t_test_behinds <- t.test(
  (home_behinds + away_behinds) ~ is_wet, 
  data = afl_results_weather
)
print(t_test_behinds)

# Plots

match_score <- ggplot(afl_results_weather, aes(x = factor(is_wet), y = home_score + away_score)) +
  geom_boxplot(fill = c("#F8766D", "#00BFC4")) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    title = "Total Match Score by Weather Condition",
    x = "Weather Condition",
    y = "Total Score"
  ) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  theme_minimal()
match_score


afl_long <- afl_results_weather %>%
  mutate(
    total_goals = home_goals + away_goals,
    total_behinds = home_behinds + away_behinds
  ) %>%
  select(match_id, is_wet, total_goals, total_behinds) %>%
  pivot_longer(
    cols = c(total_goals, total_behinds),
    names_to = "score_type",
    values_to = "count"
  )

goals_behinds <- ggplot(afl_long, aes(x = factor(is_wet), y = count, fill = score_type)) +
  geom_boxplot() +
  labs(
    title = "Goals vs Behinds by Weather Condition",
    x = "Weather Condition",
    y = "Count",
    fill = "Score Type"
  ) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  scale_fill_discrete(labels = c("Behinds", "Goals")) +
  theme_minimal()
goals_behinds

afl_ratio <- afl_results_weather %>%
  group_by(match_id, is_wet) %>%
  summarize(
    goals = sum(home_goals + away_goals),
    behinds = sum(home_behinds + away_behinds),
    ratio = goals / behinds
  )

ratio <- ggplot(afl_ratio, aes(x = factor(is_wet), y = ratio)) +
  geom_boxplot(fill = c("#F8766D", "#00BFC4")) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    title = "Goals to Behinds Ratio by Weather Condition",
    x = "Weather Condition",
    y = "Goals/Behinds Ratio"
  ) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  theme_minimal()
ratio

# So from this we can conclusively say, wet weather significantly reduces total match scores with a p-value of 0.01.
# In dry conditions, the average total score is 169.36 points
# In wet conditions, the average total score is 158.68 points
# This represents approximately a 10.68-point decrease in wet weather
# The 95% confidence interval (2.52 to 18.84) confirms this decrease is statistically meaningful.
# 
# Goals are significantly reduced in wet weather:
# In dry conditions, teams score an average of 24.52 goals per match
# In wet conditions, this drops to 22.79 goals per match
# This represents approximately a 1.73-goal decrease
# 
# Behinds are NOT significantly affected:
# Dry conditions: 22.21 behinds per match
# Wet conditions: 21.94 behinds per match
# The difference of 0.27 behinds is not statistically significant therefore wet weather should not affect
# player goal kicking accuracy later on unless there is more kicks on the full or not making the distance
# This makes intuitive sense - wet conditions likely make it harder to execute clean ball handling needed for goals,
# while the overall difficulty of the game increases. The relatively stable number of behinds suggests players are 
# still getting shots on goal, but are less able to convert them into full 6-point scores in wet conditions.
####################################################################################################################################
# Does wet weather cause more upsets
# We will exclude the first 10 rounds of the season so we are working with established ladder positions
# If we include the early rounds then a team who is closer to Z in the alphabet but is actually a flag favourite
# will skew the results

afl_filtered <- afl_results_weather %>%
  filter(round > 10) %>% 
  filter(!is.na(home_ladder_position) & !is.na(away_ladder_position))

odds <- fetch_betting_odds_footywire(2018, 2018)
colnames(odds)

odds <- odds %>% mutate(
  date = as.Date(Date, format = '%d/%m/%y'),
  season = Season,
  round = Round,
  home_team = Home.Team,
  away_team = Away.Team,
  home_odds = Home.Win.Odds,
  away_odds = Away.Win.Odds
) %>% select(
  date, season, round, home_team, away_team, home_odds, away_odds
)

afl_weather_odds <- afl_results_weather %>%
  left_join(odds, by = c("date", "season", "home_team", "away_team")) %>% mutate(
    round = round.x
  ) %>% select(
    season, date, round, match_id, home_team, home_ladder_position, home_odds, home_goals, home_behinds,
    home_score, away_team, away_ladder_position, away_odds, away_goals, away_behinds, away_score,
    home_win, is_wet
  )

missing_odds <- sum(is.na(afl_weather_odds$home_odds))
print(missing_odds)


# For matches without odds data, we'll use ladder position 
afl_complete <- afl_weather_odds %>%
  mutate(
    favorite = case_when(
      !is.na(home_odds) & !is.na(away_odds) & home_odds < away_odds ~ "home",
      !is.na(home_odds) & !is.na(away_odds) & away_odds < home_odds ~ "away",
      !is.na(home_ladder_position) & !is.na(away_ladder_position) & 
        home_ladder_position < away_ladder_position ~ "home",
      !is.na(home_ladder_position) & !is.na(away_ladder_position) & 
        away_ladder_position < home_ladder_position ~ "away",
      TRUE ~ "even"
    ),
    # Flag whether we used odds or ladder for determining favorite
    used_odds = !is.na(home_odds) & !is.na(away_odds),
    strength_diff = case_when(
      used_odds & favorite == "home" ~ away_odds / home_odds,
      used_odds & favorite == "away" ~ home_odds / away_odds,
      !used_odds & favorite == "home" ~ (away_ladder_position - home_ladder_position)/18,
      !used_odds & favorite == "away" ~ (home_ladder_position - away_ladder_position)/18,
      TRUE ~ 0
    ),
    upset = case_when(
      favorite == "home" & home_win == 0 ~ 1,
      favorite == "away" & home_win == 1 ~ 1,
      TRUE ~ 0
    )
  )

upset_summary <- afl_complete %>%
  group_by(is_wet, used_odds) %>%
  summarize(
    total_games = n(),
    total_upsets = sum(upset),
    upset_percentage = round(100 * total_upsets / total_games, 1)
  )
print(upset_summary)

chi_test <- chisq.test(table(afl_complete$is_wet, afl_complete$upset))
print(chi_test)

# Include a control variable for whether we used odds or ladder position
logistic_model <- glm(
  upset ~ is_wet + strength_diff + used_odds + is_wet:strength_diff, 
  data = afl_complete,
  family = "binomial"
)
print(summary(logistic_model))

home_underdog_analysis <- afl_complete %>%
  filter(favorite == "away") %>% 
  group_by(is_wet) %>%
  summarize(
    total_games = n(),
    home_wins = sum(home_win),
    home_win_percentage = round(100 * home_wins / total_games, 1)
  )
print(home_underdog_analysis)

ggplot(home_underdog_analysis, aes(x = factor(is_wet), y = home_win_percentage, fill = factor(is_wet))) +
  geom_col() +
  geom_text(aes(label = paste0(home_win_percentage, "%")), vjust = -0.5) +
  labs(
    title = "Home Underdog Win Percentage by Weather Condition",
    x = "Weather Condition",
    y = "Win Percentage",
    fill = "Weather"
  ) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  theme_minimal() +
  theme(legend.position = "none")

# The Chi-Square Test was used to determine whether there is a significant relationship between 
# weather conditions and upsets.
# The chi-square test is ideal for categorical data and helps assess whether two categorical variables 
# are independent of each other.
# In this case, we are checking if upsets happen more or less frequently in wet weather
# The test compares the actual number of upsets in wet and dry conditions with the expected number 
# under the assumption that weather does not impact upsets.
# A significant result suggests that upsets are not randomly distributed across different weather conditions.
# 
# The p-value from the chi-square test was 0.0025, which is significant.
# This indicates that the probability of observing such a difference means
# weather likely has an influence on upsets.
# 
# Lets look at the GLM
# Intercept -0.8905, p = 0.0003
# The negative intercept suggests that, in the baseline scenario the probability of an upset occurring is relatively low.
# 
# Weather Condition is_wet: -1.2956, p = 0.0477
# Upsets are significantly less likely in wet weather.
# This aligns with the Chi-Square test results
# 
# Strength Difference strength_diff: -0.5977, p = 0.0145
# Greater differences in team strength reduce the likelihood of an upset.
# his is expected if a strong team faces a weak opponent, the stronger team is more likely to win, 
# 
# Betting Odds vs. Ladder Position used_odds: 1.7415, p = 0.0032 
# Using betting odds instead of ladder position increases the likelihood of identifying upsets. 
# 
# Interaction Between Weather & Strength Difference is_wet:strength_diff: -0.2075, p = 0.6893
# The interaction effect between weather and strength difference is not statistically significant.
# In other words, while wet weather makes upsets less likely overall, 
# its effect does not significantly depend on how evenly matched the teams are.
####################################################################################################################################
# Are there teams more suited to wet weather footy

afl_player_stats <- fetch_player_stats_footywire(2018)
colnames(afl_player_stats)

afl_player_stats <- afl_player_stats %>%
  mutate(
    date = as.Date(Date, format = '%d/%m/%y'), 
    season = year(date),  
    match_id = Match_id,
    round = Round,
    name = Player,
    team = Team,
    opposition = Opposition,
    goal_assists = GA,
    contested_possessions = CP,
    uncontested_possessions = UP,
    effective_disposals = ED,
    disposal_efficiency = DE,
    contested_marks = CM,
    marks_inside_50 = MI5,
    one_percenters = `One.Percenters`,  
    bounces = BO,
    time_on_ground = TOG,
    kicks = K,
    handballs = HB,
    disposals = D,
    marks = M,
    goals = G,
    behinds = B,
    tackles = T,
    hit_outs = HO,
    inside_50s = I50,
    clearances = CL,
    clangers = CG,
    rebound_50s = R50,
    frees_for = FF,
    frees_against = FA,
    afl_fantasy_points = AF,
    supercoach_points = SC,
    centre_clearances = CCL,
    stoppage_clearances = SCL,
    score_involvements = SI,
    metres_gained = MG,
    turnovers = TO,
    interceptions = ITC,
    tackles_inside_50 = T5
  ) %>%
  select(
    season, date, match_id, round, name, team, opposition, 
    goal_assists, contested_possessions, uncontested_possessions, 
    effective_disposals, disposal_efficiency, contested_marks, marks_inside_50, 
    one_percenters, bounces, time_on_ground, kicks, handballs, disposals, marks, 
    goals, behinds, tackles, hit_outs, inside_50s, clearances, clangers, rebound_50s, 
    frees_for, frees_against, afl_fantasy_points, supercoach_points, centre_clearances, 
    stoppage_clearances, score_involvements, metres_gained, turnovers, interceptions, tackles_inside_50
  )

afl_player_stats <- afl_player_stats %>%
  mutate(round = parse_number(round)) %>% 
  filter(!is.na(round)) 

afl_team_stats <- afl_player_stats %>%
  group_by(season, round, team) %>%
  summarise(
    total_goal_assists = sum(goal_assists, na.rm = TRUE),
    total_contested_possessions = sum(contested_possessions, na.rm = TRUE),
    total_uncontested_possessions = sum(uncontested_possessions, na.rm = TRUE),
    total_effective_disposals = sum(effective_disposals, na.rm = TRUE),
    avg_disposal_efficiency = mean(disposal_efficiency, na.rm = TRUE),
    total_contested_marks = sum(contested_marks, na.rm = TRUE),
    total_marks_inside_50 = sum(marks_inside_50, na.rm = TRUE),
    total_one_percenters = sum(one_percenters, na.rm = TRUE),
    total_bounces = sum(bounces, na.rm = TRUE),
    avg_time_on_ground = mean(time_on_ground, na.rm = TRUE),
    total_kicks = sum(kicks, na.rm = TRUE),
    total_handballs = sum(handballs, na.rm = TRUE),
    total_disposals = sum(disposals, na.rm = TRUE),
    total_marks = sum(marks, na.rm = TRUE),
    total_goals = sum(goals, na.rm = TRUE),
    total_behinds = sum(behinds, na.rm = TRUE),
    total_tackles = sum(tackles, na.rm = TRUE),
    total_hit_outs = sum(hit_outs, na.rm = TRUE),
    total_inside_50s = sum(inside_50s, na.rm = TRUE),
    total_clearances = sum(clearances, na.rm = TRUE),
    total_clangers = sum(clangers, na.rm = TRUE),
    total_rebound_50s = sum(rebound_50s, na.rm = TRUE),
    total_frees_for = sum(frees_for, na.rm = TRUE),
    total_frees_against = sum(frees_against, na.rm = TRUE),
    total_afl_fantasy_points = sum(afl_fantasy_points, na.rm = TRUE),
    total_supercoach_points = sum(supercoach_points, na.rm = TRUE),
    total_centre_clearances = sum(centre_clearances, na.rm = TRUE),
    total_stoppage_clearances = sum(stoppage_clearances, na.rm = TRUE),
    total_score_involvements = sum(score_involvements, na.rm = TRUE),
    total_metres_gained = sum(metres_gained, na.rm = TRUE),
    total_turnovers = sum(turnovers, na.rm = TRUE),
    total_interceptions = sum(interceptions, na.rm = TRUE),
    total_tackles_inside_50 = sum(tackles_inside_50, na.rm = TRUE)
  ) %>%
  arrange(season, round, team)
sum(is.na(afl_team_stats))

afl_complete <- afl_complete %>%
  left_join(
    afl_team_stats, 
    by = c("season", "round", "home_team" = "team")
  ) %>%
  rename_with(~ paste0("home_", .), -c(season, round, home_team, away_team)) %>%
  left_join(
    afl_team_stats, 
    by = c("season", "round", "away_team" = "team")
  ) %>%
  rename_with(~ paste0("away_", .), -c(season, round, home_team, away_team))

columns_to_remove <- grep("^away_home_away_", colnames(afl_complete), value = TRUE)
afl_complete <- afl_complete %>%
  select(-all_of(columns_to_remove))
afl_complete <- afl_complete %>%
  rename_with(~ gsub("^away_home_", "", .)) # Remove unnecessary "away_home_" prefixes
colnames(afl_complete)


####################################################################################################################################
# Does wet weather affect player statistics - does it affect accuracy, are some players suited to wet weather


####################################################################################################################################

