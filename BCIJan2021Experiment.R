library(plotly)
library(lubridate)
library(gsheet)
library(dplyr)

source("utils/loadrawdata.R")

options("digits.secs"=6)
vistemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

timetemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", xaxis = list(tickformat="%H:%M:%S.%L ms"))#, yaxis = list(range=c(0,1.1)))

df_kiwi = LoadFromDirectory('Kiwi', event = "Game", sample = "Sample", meta = "Meta", sep=';')
df_hand = LoadFromDirectory('HandStrength', event = "Game", sample = "Sample", meta = "Meta", sep=';')

df_kiwi = df_kiwi %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS"))
df_hand = df_hand %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS"))

df_kiwi = df_kiwi %>% rename(GameTitle = i0, PID = i1, Condition = i2)
df_hand = df_hand %>% rename(GameTitle = i0, PID = i1, Condition = i2)


df_kiwi = df_kiwi %>% mutate(PID = as.numeric(PID))
df_hand = df_hand %>% mutate(PID = as.numeric(PID))


#############
# Summaries #
#############
# Merge df_kiwi and df_hand
df_kiwi = df_kiwi %>% mutate(Game = "Kiwi")
df_hand = df_hand %>% mutate(Game = "Hand")

df_all = df_kiwi %>% bind_rows(df_hand)

trial_summary <- df_all %>% ungroup() %>% group_by(PID,Condition) %>%
  filter(Event == "GameDecision") %>%
  summarize(rejInput = sum(TrialResult == "RejInput"),
            accInput = sum(TrialResult == "AccInput"),
            fabInput = sum(TrialResult == "FabInput"),
            BCIProcessingMode = unique(BCIProcessingMode),
            Game = unique(Game),
            totalTrials = rejInput+accInput+fabInput)

trial_summary <- trial_summary %>% ungroup() %>%
  mutate(rate = (accInput)/totalTrials,
         session = 1,
         session = cumsum(session))

p_overall <- trial_summary %>% group_by(Condition) %>%
  summarize(avg_rate = mean(rate),
            sum_acc = sum(accInput))

##############
# Perc-Frust #
##############

url = 'https://docs.google.com/spreadsheet/ccc?key=1qew_UwZLD9Pkh4ogGUBTuV6nPEI_ITq4cjcnLnUfoBg#gid=1363353697'
dr_kiwi <- gsheet2tbl(url)
url = 'https://docs.google.com/spreadsheet/ccc?key=1qew_UwZLD9Pkh4ogGUBTuV6nPEI_ITq4cjcnLnUfoBg#gid=237988817'
dr_hand <- gsheet2tbl(url)

valid_pids = 1:13
dr_hand <- dr_hand %>% filter(Participant %in% valid_pids)
dr_kiwi = dr_kiwi %>% mutate(Participant = NULL,
                             Participant = KiwiParticipant,
                             Game = "Kiwi")
dr_hand = dr_hand %>% mutate(Game = "Hand")
dr_kiwi <- dr_kiwi %>% filter(Participant %in% valid_pids)

dr_hand <- dr_hand %>% mutate(Participant = as.numeric(Participant))
dr_kiwi <- dr_kiwi %>% mutate(Participant = as.numeric(Participant))

dr_all = dr_hand %>% bind_rows(dr_kiwi)

############ Condition to Perceived Control ############

vistemplate %>%
  add_trace(data = dr_all, x=~Condition, y=~jitter(PercNormalized, amount=.03),
            type='scatter',mode='markers', color=~Participant)

############ Perceived Control to Level of Control ############

trial_combined = trial_summary %>% left_join(dr_all, by=c("PID" = "Participant", "Condition" = "Condition", "Game" = "Game"))

PercControlCurve <- lm(as.numeric(trial_combined$rate) ~ as.numeric(trial_combined$PercNormalized))

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Game == "Hand"), x=~rate, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=~Condition, symbol=I('square'), size=2) %>%
  add_trace(data = trial_combined %>% filter(Game == "Kiwi"), x=~rate, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=~Condition, symbol=I('o'), size=2) %>%
  layout(yaxis = list(range=c(0,1.1)), xaxis = list(range=c(0,1.1)))

############ Pure Conditions vs Perceived Control ############

qfitPure <- lm(PercNormalized ~ rate, data = trial_combined %>% filter(Condition %in% c("100A", "50A")))
PercRateCurve.Pure <- data.frame(x = (0:100)/100, y = predict(qfitPure, data.frame(rate = (0:100)/100), se.fit=T))

summary(qfitPure)

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("100A", "50A")), x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~PID) %>%
  add_trace( data=PercRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Fab+Pure Conditions vs Perceived Control ############

qfitFab <- lm(PercNormalized ~ rate, data = trial_combined %>% filter(Condition %in% c("100B", "50B")))
PercRateCurve.Fab <- data.frame(x = (0:10)/10, y = predict(qfitFab, data.frame(rate = (0:10)/10), se.fit=T))

summary(qfitFab)

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("100B", "50B")), x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03),
            type="scatter", mode='markers', color=I('orange')) %>%
  add_trace( data=PercRateCurve.Fab, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace( data=PercRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("100A", "50A")), x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03),
            type="scatter", mode='markers', color=I('SeaGreen')) %>%
  layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Pure Conditions vs Frustration ############

qfrustPure <- lm(FrustNormalized ~ rate, data = trial_combined %>% filter(Condition %in% c("100A", "50A")))
FrustRateCurve.Pure <- data.frame(x = (0:100)/100, y = predict(qfrustPure, data.frame(rate = (0:100)/100), se.fit=T))

summary(qfrustPure)

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("100A", "50A")), x=~jitter(rate, amount=.03), y=~jitter(FrustNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~PID) %>%
  add_trace( data=FrustRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Pure+Fab Conditions vs Frustration ############

qfrustFab <- lm(FrustNormalized ~ rate, data = trial_combined %>% filter(Condition %in% c("100B", "50B")))
FrustRateCurve.Fab <- data.frame(x = (0:10)/10, y = predict(qfrustFab, data.frame(rate = (0:10)/10), se.fit=T))

summary(qfrustFab)

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("50B")), x=~jitter(rate, amount=.03), y=~jitter(FrustNormalized, amount=.03),
            type="scatter", mode='markers', color=I('orange')) %>%
  add_trace( data=FrustRateCurve.Fab, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace( data=FrustRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("50A")), x=~jitter(rate, amount=.03), y=~jitter(FrustNormalized, amount=.03),
            type="scatter", mode='markers', color=I('SeaGreen')) %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Perceived Control vs Frustration ############

qfrustPerc <- lm(FrustNormalized ~ rate, data = trial_combined)
FrustPercCurve <- data.frame(x = (0:10)/10, y = predict(qfrustPerc, data.frame(rate = (0:10)/10), se.fit=T))
summary(qfrustPerc)

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("50B","100B")), x=~jitter(PercNormalized, amount=.02), y=~jitter(FrustNormalized, amount=.02), color=I('orange'),
            type="scatter", mode='markers', color=~PID) %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("50A","100A")), x=~jitter(PercNormalized, amount=.02), y=~jitter(FrustNormalized, amount=.02), color=I('seagreen'),
            type="scatter", mode='markers', color=~PID) %>%
  add_trace( data=FrustPercCurve, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Perceived Control"),showlegend=F)


# We need to do an aggregate frustration thing as a follow-up to do, based on the order people rated. to improve
# the rating between frustration and the fabricated.

vistemplate %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("50B")), x=~jitter(rate, amount=.02), y=~jitter(PercNormalized, amount=.02), color=I('skyblue'),
            type="scatter", mode='markers', color=~PID) %>%
  add_trace(data = trial_combined %>% filter(Condition %in% c("100A")), x=~jitter(rate, amount=.02), y=~jitter(PercNormalized, amount=.02), color=I('seagreen'),
            type="scatter", mode='markers', color=~PID) %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Perceived Control"),showlegend=F)















qfitAll <- lm(rate ~ PercNormalized, data = trial_combined)
PercRateCurve.All <- data.frame(x = (0:10)/10, y = predict(qfitAll, data.frame(PercNormalized = (0:10)/10), se.fit=T))

vistemplate %>%
  add_trace(data = trial_combined, x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~PID) %>%
  #add_trace( data=PercRateCurve.All, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

FfitAll <- lm(rate ~ FrustNormalized, data = trial_combined)
FrustRateCurve.All <- data.frame(x = (0:10)/10, y = predict(FfitAll, data.frame(PercNormalized = (0:10)/10), se.fit=T))

vistemplate %>%
  add_trace(data = trial_combined, x=~jitter(PercNormalized, amount=.03), y=~jitter(FrustNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~PID) %>%
  #add_trace( data=PercRateCurve.All, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Perceived Control"),showlegend=F)

timetemplate %>%
  add_trace(data = df_kiwi %>% filter(PID == "1"), x =~Framecount, y=~BCIConfidence, type="scattergl", mode="markers")





#################################################
# Influence of 50A on people's level of control #
#################################################

vistemplate %>%
  add_trace(data = trial_combined, x=~rate, y=~jitter(PercNormalized, amount=.03),
            type='scatter',mode='markers', color=~Condition, size=2)