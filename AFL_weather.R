library(fitzRoy)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
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
    round_number = as.numeric(str_sub(round, -2, -1)), 
    round_category = ifelse(round_number <= 10, 1, 2)  
  ) %>%
  select(-round) %>%  
  rename(round = round_category) %>% select(
    season, date, round, match_id, home_team, home_goals, home_behinds, home_score, away_team, away_goals, away_behinds,
    away_score, home_win, is_wet
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
# Does wet weather affect total score










####################################################################################################################################
# Does wet weather affect who will win 











####################################################################################################################################
# Does wet weather affect player statistics

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

####################################################################################################################################

