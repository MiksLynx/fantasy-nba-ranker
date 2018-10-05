library(tidyverse)
library(readxl)
library(stringr)
library(purrr)

sheet_list <- lapply(excel_sheets('fantasy nba stats.xlsx'), read_excel, path='fantasy nba stats.xlsx')

teams <- c('Atl', 'Bkn', 'Bos', 'Cha', 'Chi', 'Cle', 'Dal', 'Den', 'Det', 'GS', 'Hou', 'Ind', 'LAC', 'LAL', 'Mem',
  'Mia', 'Mil', 'Min', 'No', 'NY', 'OKC', 'Orl', 'Phi', 'Phx', 'Por', 'SA', 'Sac', 'Tor', 'Utah', 'Wsh')
pats <- paste0(teams, collapse = "|")

myData <- plyr::rbind.fill(sheet_list) %>%
  mutate(
    position = str_replace_all(TEAM_POS, pats, ''),
    primaryPosition = str_replace_all(position, ',(.*)', ''),
    PG = ifelse(grepl('PG', position), 1, 0),
    SG = ifelse(grepl('SG', position), 1, 0),
    C = ifelse(grepl('C', position), 1, 0),
    PF = ifelse(grepl('PF', position), 1, 0),
    SF = ifelse(grepl('SF', position), 1, 0)
  ) %>%
  select(-TEAM_POS, -position) %>%
  gather(PG, SG, C, PF, SF, key = 'position', value = 'yn') %>%
  filter(yn == 1, TOT != '--') %>%
  select(-yn) %>%
  mutate_at(vars(FGM:AVG), funs(as.numeric(.))) %>%
  mutate(finalPoints = FGM - FGA + FTM - FTA + REB + AST + STL + BLK - TO + PTS)


#### Initial analysis
# Since roster size is 13, but position restrictions restrict to 15 players, then we will assume roster size 15

initialRestrictionParams <- tribble(
  ~pos, ~slots,
  "PG", 3,
  "SG", 3,
  "PF", 3,
  "SF", 3,
  "C", 3
) %>%
  mutate(slots = slots * 14)

replacementPlayer <- function(pos, slots) {
  myProjection %>%
    filter(position == pos) %>%
    arrange(desc(finalPoints)) %>%
    filter(row_number() <= slots) %>%
    group_by(position) %>%
    summarise(rp = mean(finalPoints))
}

rp <- pmap(initialRestrictionParams, replacementPlayer) %>% bind_rows()

projVorp <- myData %>%
  left_join(rp, by = 'position') %>%
  mutate(vorp = finalPoints - rp) %>%
  select(position, PLAYER, finalPoints, vorp) %>%
  arrange(desc(vorp))




#### Actual draft case. After each pick we need to remove picked players and recalculate relative ratings

pickedPlayers <- c(
  'Giannis Antetokounmpo',
  'Lebron James',
  'Anthony Davis',
  'James Harden',
  'Kevin Durant',
  'Russell Westbrook'
)

filteringCondition <- paste0(pickedPlayers, collapse = "|")

leftPlayers <- myData %>%
  filter(!(str_detect(tolower(PLAYER), tolower(filteringCondition))))

# To fill in picked positions we will just choose primary position of picked player
pickedPositions <- myData %>%
  filter(str_detect(tolower(PLAYER), tolower(filteringCondition))) %>%
  distinct(PLAYER, primaryPosition) %>%
  group_by(primaryPosition) %>%
  summarise(pickedCount = n()) %>%
  rename(pos = primaryPosition)

currentRestrictions <- initialRestrictionParams %>%
  left_join(pickedPositions, by = 'pos') %>%
  mutate(
    pickedCount = ifelse(is.na(pickedCount), 0, pickedCount),
    slots = slots - pickedCount
  ) %>%
  select(-pickedCount)

actualProjection <- leftPlayers %>%
  group_by(position) %>%
  mutate(
    avg = mean(finalPoints),
    vorp = finalPoints - avg
  ) %>%
  arrange(desc(vorp))

actualReplacementPlayer <- function(pos, slots) {
  actualProjection %>%
    filter(position == pos) %>%
    arrange(desc(finalPoints)) %>%
    filter(row_number() <= slots) %>%
    group_by(position) %>%
    summarise(rp = mean(finalPoints))
}

rp <- pmap(currentRestrictions, actualReplacementPlayer) %>% bind_rows()

projVorp <- leftPlayers %>%
  left_join(rp, by = 'position') %>%
  mutate(vorp = finalPoints - rp) %>%
  select(position, PLAYER, finalPoints, vorp) %>%
  arrange(desc(vorp))

