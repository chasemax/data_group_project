library(tidyverse)

team_results <- read_csv("https://byu.box.com/shared/static/mmsaiq1tfo4mfd4zvrbdxppsaenta331.csv")
team_conferences <- read_csv("data_group_project/data/MTeamConferences.csv")
conferences <- read_csv("data_group_project/data/Conferences.csv")
seeds <- read_csv("data_group_project/data/MNCAATourneySeeds.csv")
tourney_results <- read_csv("data_group_project/data/MNCAATourneyCompactResults.csv")


game_team_results <- team_results %>%
  mutate(GameID = row_number()) %>%
  select(-WLoc, -DayNum, -NumOT) %>%
  pivot_longer(
    cols = c(WTeamID, LTeamID),
    names_to = "WinTeam",
    values_to = "TeamID"
  ) %>%
  mutate(WinTeam = if_else(WinTeam == "WTeamID", 'Y', 'N')) %>%
  pivot_longer(
    cols = c(WScore, LScore, WFGM:LPF),
    names_to = "Stat",
    values_to = "StatValue"
  ) %>%
  filter((WinTeam == 'Y' & str_sub(Stat, end=1) == 'W') | (WinTeam == 'N' & str_sub(Stat, end=1) == 'L')) %>%
  mutate(Stat = str_sub(Stat, 2)) %>%
  select(-WinTeam) %>%
  pivot_wider(
    names_from = Stat,
    values_from = StatValue
  )

seeds_clean <- seeds %>%
  mutate(Seed = parse_number(Seed))

team_season_stats <- game_team_results %>%
  group_by(TeamID, Season) %>%
  summarize(across(Score:PF, ~mean(.x))) %>%
  ungroup() %>%
  inner_join(team_conferences, by = c('Season' = 'Season', 'TeamID' = 'TeamID')) %>%
  inner_join(conferences, by = c('ConfAbbrev' = 'ConfAbbrev')) %>%
  select(-ConfAbbrev) %>%
  rename(Conference = Description) %>%
  left_join(seeds_clean, by = c('Season' = 'Season', 'TeamID' = 'TeamID'))

set.seed(42)

shuffled_t_results <- tourney_results %>%
  filter(Season >= 2003) %>%
  select(-DayNum, -WScore, -LScore, -WLoc, -NumOT) %>%
  mutate(GameID = row_number()) %>%
  mutate(FirstTeam = runif(nrow(.))) %>%
  mutate(Team1ID = if_else(FirstTeam > 0.5, WTeamID, LTeamID), 
         Team2ID = if_else(FirstTeam <= 0.5, WTeamID, LTeamID),
         Team1Won = if_else(FirstTeam > 0.5, 1, 0)
  ) %>%
  select(-WTeamID, -LTeamID, -FirstTeam)

ready_to_model <- shuffled_t_results %>%
  left_join(team_season_stats, by = c('Season' = 'Season', 'Team1ID' = 'TeamID')) %>%
  left_join(team_season_stats, by = c('Season' = 'Season', 'Team2ID' = 'TeamID'), suffix = c(".1", ".2")) %>%
  janitor::clean_names()


library(caret)

predictions <- ready_to_model %>% 
  mutate(predicted=if_else(seed_1 >= seed_2, 0, 1)) %>% 
  select(predicted, team1won) %>% 
  rename(actual=team1won)

ready_to_model %>% 
  mutate(predicted=if_else(seed_1 >= seed_2, 0, 1)) %>% 
  select(seed_1, seed_2, predicted, team1won) %>% 
  rename(actual=team1won)

predictions

cm <- table(predictions$predicted, predictions$actual)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])

accuracy
