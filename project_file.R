library(tidyverse)

team_results <- read_csv("https://byu.box.com/shared/static/mmsaiq1tfo4mfd4zvrbdxppsaenta331.csv")

team_results %>%
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
