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

D = D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS"))
D = D %>% rename(GameTitle = i1, Participant = i2, Condition = i3)
D = D %>% mutate(Participant = as.numeric(Participant))

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

valid_pids = 1:16
L <- L %>% filter(Participant %in% valid_pids)


#############
# Merge
#############

D <- D %>% left_join(L, by=c('GameTitle' = 'GameTitle', 'Condition' = 'Condition', 'Participant' = 'Participant'))

#############
# Save D
#############

save(D, file = 'data_all.rda', compress=TRUE)
