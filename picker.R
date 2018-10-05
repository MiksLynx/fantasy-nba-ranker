
#### Actual draft case. After each pick we need to remove picked players and recalculate relative ratings

# pickedPlayers <- c(
#   'Giannis Antetokounmpo'
#   , 'Lebron James'
#   , 'Karl-Anthony Towns'
#   , 'Kevin Durant'
#   , 'Nikola Jokic'
#   , 'Damian Lillard'
#   , 'Kyle Lowry'
#   , 'Ben Simmons'
#   , 'John Wall'
#   , 'DeMar DeRozan'
#   , 'Jimmy Butler'
#   , 'Anthony Davis'
#   , 'James Harden'
#   , 'Stephen Curry'
#   , 'Andre Drummond'
#   , 'LaMarcus Aldridge'
#   , 'Kawhi Leonard'
#   , 'Joel Embiid'
#   , 'Paul George'
#   , 'Russell Westbrook'
#   , 'Victor Oladipo'
#   , 'Bradley Beal'
#   , 'Kevin Love'
#   , 'Chris Paul'
#   , 'Donovan Mitchell'
#   , 'Kemba Walker'
#   , 'Jrue Holiday'
#   , 'Kyrie Irving'
#   , 'Devin Booker'
#   ,'DeAndre Jordan'
#   , 'Marc Gasol'
#   , 'CJ McCollum'
#   , 'Draymond Green'
#   , 'Enes Kanter'
#   , 'Khris Middleton'
#   , 'Clint Capela'
#   , 'Dwight Howard'
#   , 'Lou Williams'
#   , 'Rudy Gobert'
#   , 'Goran Dragic'
#   , 'Deandre Ayton'
#   , 'Hassan Whiteside'
#   , 'Ricky Rubio'
#   , 'Klay Thompson'
#   , 'Jayson Tatum'
#   , 'Julius Randle'
#   , 'Blake Griffin'
#   , 'Gordon Hayward'
#   , 'John Collins'
#   , 'Mike Conley'
#   , 'Tobias Harris'
#   , 'Brandon Ingram'
#   , 'Luka Doncic'
#   , 'Andrew Wiggins'
#   , 'Lonzo Ball'
#   , 'Eric Bledsoe'
#   , 'Dennis Smith Jr.'
#   , 'Al Horford'
#   , 'Nikola Vucevic'
#   , 'Harrison Barnes'
#   , 'Jonas Valanciunas'
#   
# )

# Load players that are marked to be picked
pickedPlayers <- read_excel('picked-players.xlsx') %>%
  filter(isPicked == 1) %>%
  .$player

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

leftPlayers %>%
  left_join(rp, by = 'position') %>%
  mutate(vorp = finalPoints - rp) %>%
  select(position, PLAYER, finalPoints, vorp) %>%
  arrange(desc(vorp)) %>%
  slice(1:10)

