library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")
options("digits.secs"=6)

# Load HandStrengthener and Kiwi data from directories
D <- LoadFromDirectory("data", event="Game")

save(D, file = 'data_all_raw.rda', compress=TRUE)
# load('data_all_raw.rda')

#############
# Format D
#############

D = D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
          arrange(Timestamp) %>%
          mutate(time_delta = Timestamp - lag(Timestamp),
                 time_delta = as.numeric(time_delta),
                 time_delta = ifelse(is.na(time_delta), 0, time_delta))

D = D %>% rename(GameTitle = i1, Participant = i2, Condition = i3)
D = D %>% mutate(Participant = as.numeric(Participant),
                 InputWindowOrder = as.numeric(InputWindowOrder))

# Filter out data happening before GameRunning event.
D = D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>% arrange(Timestamp) %>%
  mutate(isGame = ifelse(Event == "GameRunning", 1, 0),
         isGame = cumsum(isGame)) %>%
  filter(isGame == 1)

D = D %>% mutate(InputWindowClosedID = NA,
             InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", 1, 0),
             InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", cumsum(InputWindowClosedID), InputWindowClosedID),
             InputWindowClosedID = ifelse(Event == "GameStopped", -1, InputWindowClosedID),
             InputWindowClosedID = ifelse(Event == "GameRunning", -1, InputWindowClosedID),
             InputWindowClosedID = ifelse(Event == "InputWindowChange" & InputWindow == "Open", -1, InputWindowClosedID),
             InputWindowClosedFill = ifelse(InputWindowClosedID == 0, NA, InputWindowClosedID)) %>%
      tidyr::fill(InputWindowClosedFill, .direction="down")

# Create InputWindowOrderFilled column.
D = D %>% group_by(GameTitle, Participant, Condition) %>% 
  mutate(InputWindowOrder = ifelse(Event == "GameStopped", -1, InputWindowOrder),
         InputWindowOrder = ifelse(Event == "GameRunning", -1, InputWindowOrder),
         InputWindowOrder = ifelse(InputWindow == "Closed", -1, InputWindowOrder),
         Period = NA,
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Closed", "RestPeriod", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindowClosedID == max(InputWindowClosedID, na.rm=T), "PostGame", Period),
         Period = ifelse(Event == "InputWindowChange" & InputWindow == "Open", "OpenPeriod", Period),
         Period = ifelse(Event == "GameRunning", "PreGame", Period),
         InputWindowOrderFilled = InputWindowOrder) %>%
  tidyr::fill(InputWindowOrderFilled, .direction="down") %>%
  tidyr::fill(Period, .direction="down")

#############
# Load Likert Data
#############

L_kiwi <- gsheet2tbl('https://docs.google.com/spreadsheet/ccc?key=1qew_UwZLD9Pkh4ogGUBTuV6nPEI_ITq4cjcnLnUfoBg#gid=1363353697')
L_hand <- gsheet2tbl('https://docs.google.com/spreadsheet/ccc?key=1qew_UwZLD9Pkh4ogGUBTuV6nPEI_ITq4cjcnLnUfoBg#gid=237988817')

L_kiwi = L_kiwi %>% mutate(Participant = NULL,
                           Participant = KiwiParticipant,
                           GameTitle = "Kiwi",
                           KiwiParticipant = NULL)
L_hand$GameTitle = "HandStrength"

L <- L_kiwi %>% bind_rows(L_hand)

# Filter out non-valid data.
valid_pids = 1:16
L <- L %>% filter(Participant %in% valid_pids)

# Create ConditionOrderTotal
# Calculates order of conditions, also based by which game is first.
L <- L %>% group_by(Participant) %>% fill(GameOrder, .direction = c("downup"))
L <- L %>% ungroup() %>%
  mutate(OrderAll = Order,
         OrderAll = ifelse(GameTitle == "Kiwi" & GameOrder == "HandKiwi", OrderAll+4, OrderAll),
         OrderAll = ifelse(GameTitle == "HandStrength" & GameOrder == "KiwiHand", OrderAll+4, OrderAll))
  

#############
# Merge
#############

D <- D %>% left_join(L, by=c('GameTitle' = 'GameTitle', 'Condition' = 'Condition', 'Participant' = 'Participant'))

#############
# Filter out missed conditions
#############

D <- D %>% filter(!Condition %in% c('50A_WRONG', '50A-2'))

#############
# Save D
#############

save(D, file = 'data_all.rda', compress=TRUE)
