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
trial_summary <- df_kiwi %>% ungroup() %>% group_by(PID,Condition) %>%
  filter(Event == "GameDecision") %>%
  summarize(rejInput = sum(TrialResult == "RejInput"),
            accInput = sum(TrialResult == "AccInput"),
            fabInput = sum(TrialResult == "FabInput"),
            BCIProcessingMode = unique(BCIProcessingMode),
            totalTrials = rejInput+accInput+fabInput)

trial_summary <- trial_summary %>% ungroup() %>%
  mutate(rate = (accInput)/totalTrials,
         session = 1,
         session = cumsum(session))

p_overall <- trial_summary %>% group_by(Condition) %>%
  summarize(avg_rate = mean(rate),
            sum_acc = sum(accInput))

vistemplate %>%
  add_trace(data = trial_summary, x=~Condition, y=~rate,
            type='scatter',mode='markers', color=~PID)

##############
# Perc-Frust #
##############

url = 'https://docs.google.com/spreadsheet/ccc?key=1qew_UwZLD9Pkh4ogGUBTuV6nPEI_ITq4cjcnLnUfoBg#gid=1363353697'
dr_kiwi <- gsheet2tbl(url)
url = 'https://docs.google.com/spreadsheet/ccc?key=1qew_UwZLD9Pkh4ogGUBTuV6nPEI_ITq4cjcnLnUfoBg#gid=237988817'
dr_hand <- gsheet2tbl(url)

valid_pids = 1:10
dr_hand <- dr_hand %>% filter(Participant %in% valid_pids)
dr_kiwi <- dr_hand %>% filter(Participant %in% valid_pids)

dr_hand <- dr_hand %>% mutate(Participant = as.numeric(Participant))
dr_kiwi <- dr_hand %>% mutate(Participant = as.numeric(Participant))

dr_hand = dr_hand %>% mutate()

vistemplate %>%
  add_trace(data = dr_hand, x=~Condition, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=~Participant)

trial_combined = trial_summary %>% left_join(dr_hand, by=c("PID" = "Participant", "Condition" = "Condition"))

vistemplate %>%
  add_trace(data = trial_combined, x=~rate, y=~jitter(PercNormalized, amount=.03),
            type='scatter',mode='markers', color=~Condition, size=2) %>%
  layout(yaxis = list(range=c(0,1.1)), xaxis = list(range=c(0,1.1)))

timetemplate %>%
  add_trace(data = df_kiwi %>% filter(PID == "1"), x =~Framecount, y=~BCIConfidence, type="scattergl", mode="markers")

PercControlCurve <- lm(as.numeric(trial_combined$rate) ~ as.numeric(trial_combined$PercNormalized))
base::summary(PercControlCurve)

#################################################
# Influence of 50A on people's level of control #
#################################################

vistemplate %>%
  add_trace(data = trial_combined, x=~rate, y=~jitter(PercNormalized, amount=.03),
            type='scatter',mode='markers', color=~Condition, size=2)