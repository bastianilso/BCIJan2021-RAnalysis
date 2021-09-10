library(plotly)
library(lubridate)
library(tidyverse)
library(gsheet)

options("digits.secs"=6)
fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_all.rda')


#############
# Summaries #
#############

# St = Summary of Trials
St <- D %>% ungroup() %>% group_by(GameTitle, Participant,Condition) %>%
  filter(Event == "GameDecision") %>%
  summarize(rejInput = sum(TrialResult == "RejInput"),
            accInput = sum(TrialResult == "AccInput"),
            fabInput = sum(TrialResult == "FabInput"),
            otherInput = sum(!TrialResult %in% c("RejInput", "AccInput", "FabInput")),
            BCIProcessingMode = unique(BCIProcessingMode),
            GameTitle = unique(GameTitle),
            ConditionOrder = unique(OrderAll),
            totalTrials = rejInput+accInput+fabInput,
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized),
            GameOrder = unique(GameOrder),
            gender = unique(Gender),
            comment = unique(Comment),
            )


St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(time_total = sum(time_delta),
            time_total_min = time_total / 60) %>% right_join(St)

# Count the number of motor imagery attempts in total
St <- D %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  filter(Period %in% c("RestPeriod", "OpenPeriod")) %>%
  summarize(mi_recog = sum(Event == "MotorImagery")) %>% right_join(St)

# Group by input window. Count the number of attempts in each window.
St <- D %>% ungroup() %>% filter(Period %in% c("OpenPeriod")) %>% group_by(GameTitle, Participant, Condition, InputWindowOrderFilled) %>%
  summarize(mi_recog_window = sum(Event == "MotorImagery"),
            time_window = sum(time_delta)) %>%
  filter(InputWindowOrderFilled > -1) %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(mi_recog_trial = sum(mi_recog_window > 0),
            mi_recog_window = sum(mi_recog_window),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)

St <- St %>% ungroup() %>%
  mutate(rate_trial = (mi_recog_trial)/totalTrials,
         rate_feedback = accInput/ totalTrials,
         session = 1,
         session = cumsum(session),
         bci_experience = ifelse(grepl("BCIExperience",comment), TRUE, FALSE),
         mi_experience = ifelse(grepl("MIExperience",comment), TRUE, FALSE))




# Group by Rest Periods, count the number of attempts within each rest period.
# The rest period prior to Trial 1 is excluded, and the resting period after the last trial is excluded.
St <- D %>% ungroup() %>% filter(Period %in% c("RestPeriod"),
                                 InputWindowClosedFill > -1,
                                 InputWindowClosedFill < max(InputWindowClosedFill, na.rm=T)) %>%
  group_by(GameTitle, Participant, Condition) %>%
  summarize(mi_recog_rest = sum(Event == "MotorImagery"),
            time_rest = sum(time_delta),
            time_rest_min = time_rest / 60) %>% right_join(St)

St <- St %>% mutate(rest_window_mi_ratio = as.numeric(mi_recog_window) / as.numeric(mi_recog_rest),
                    fp_rate_minute = mi_recog_rest / time_rest_min,
              mi_window_frequency = mi_recog_window / time_window_min)

# Sc = Summary of Conditions
Sc <- St %>% ungroup() %>% group_by(GameTitle, Condition) %>%
  summarize(avg_rate = mean(rate_trial),
            sum_acc = sum(accInput))

# Sp = Summary of Participants
Sp <- St %>% ungroup() %>% group_by(Participant, GameTitle) %>%
  summarize(
            gender = unique(gender),
            mean_rate = mean(rate_trial),
            min_rate = min(rate_trial),
            max_rate = max(rate_trial),
            mean_feedback = mean(rate_feedback),
            bci_experience = unique(bci_experience),
            time_total = mean(time_total),
            mi_recog_total = sum(mi_recog),
            mi_recog_rest = sum(mi_recog_rest),
            mi_recog_window = sum(mi_recog_window),
            fp_rate_minute = mean(fp_rate_minute)
            )

# So = Summary of Order
So <- St %>% ungroup() %>% group_by(ConditionOrder) %>%
  summarize(mean_rate_trial = mean(rate_trial),
            mean_rate_feedback = mean(rate_feedback))
#############
# Latex Table with demographic and summary information
#############

Sp_table <- Sp %>% select(bci_experience, mean_rate, min_rate, max_rate, mean_feedback, time_total,
                          fp_rate_minute, gender, GameTitle) %>%
                    mutate(bci_experience = ifelse(bci_experience, "Yes", "No"),
                    mean_rate = paste(format(round(mean_rate * 100,0), nsmall = 0),"\\%"),
                    min_rate = paste(format(round(min_rate * 100,0), nsmall = 0),"\\%"),
                    max_rate = paste(format(round(max_rate * 100, 0), nsmall = 0),"\\%"),
                    mean_feedback = paste(format(round(mean_feedback * 100, 0), nsmall = 0), "\\%"),
                    time_total = format(round(time_total,0), nsmall = 0),
                    fp_rate_minute = format(round(fp_rate_minute,2), nsmall = 2),
                    across(everything(), as.character))
Sp_table <- Sp_table %>%
             rename(Gender = gender, `Mean Rate` = mean_rate, `Min. Rate` = min_rate,
                    `Max. Rate` = max_rate, `Mean Pos. Feedback` = mean_feedback, `BCI Experience` = bci_experience,
                    `Mean Time (s)` = time_total, `False Positives (pr min)` = fp_rate_minute)

Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant, GameTitle), names_to = "Feature") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
message(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


  group_by(Feature) %>% summarise(text = paste(fm_value, sep = " & ", collapse = " & "),
  text = paste(text, "\\ ")) %>% V

#############
# Linear regression models
#############
  
summary(lm(PercNormalized ~ rate_feedback, data = St %>% filter(GameTitle == "HandStrength")))
summary(lm(PercNormalized ~ rate_trial, data = St %>% filter(GameTitle == "HandStrength")))
summary(lm(FrustNormalized ~ rate_feedback, data = St))
summary(lm(FrustNormalized ~ rate_trial, data = St))
summary(lm(PercNormalized ~ fp_rate_minute, data = St))


    
#############
# Participants' ratings by Game Title
#############
  
St %>% group_by(GameTitle) %>%
    summarize(rate_feedback = mean(rate_feedback),
              rate_trial = mean(rate_trial),
              PercNormalized = mean(PercNormalized),
              FrustNormalized = mean(FrustNormalized))
  
St %>% group_by(GameOrder, GameTitle) %>%
  summarize(rate_feedback = mean(rate_feedback),
            rate_trial = mean(rate_trial),
            PercNormalized = mean(PercNormalized),
            FrustNormalized = mean(FrustNormalized)) %>% View()

  
#############
# MI recognition by Condition Order
#############

summary(lm(rate_trial ~ ConditionOrder, data=St))

fig1 <- fig %>%
    add_trace(data = St, x=~ConditionOrder, y=~jitter(rate_trial, amount=.02),
              type='box', boxpoints = 'all', pointpos=0, color=I('white'), symbol=I('o'),
              marker=list(size=6, color = 'grey'), line = list(color = 'black'),
              hoverinfo='text', text=~paste(Participant, Condition, GameOrder)) %>%
    layout(showlegend=F, yaxis = list(range=c(-0.05,1.1), title="Avg. MI recognition", showticklabels=T), xaxis = list(range=c(0,9), title="Condition Order"))
  
orca(fig1, "fig/mi-recognition-by-condition-order.pdf", width=450, height=350)

#############
# Influence of BCI Experience and MI Experience on participant ratings
#############

St %>% group_by(bci_experience) %>%
  summarize(rate_trial = mean(rate_trial),
            rate_feedback = mean(rate_feedback),
            PercNormalized = mean(PercNormalized),
            perc_bias = mean(PercNormalized * rate_trial),
            frust_bias = mean(FrustNormalized * rate_trial),
            FrustNormalized = mean(FrustNormalized)) %>% View()

St %>% group_by(mi_experience) %>%
  summarize(rate_trial = mean(rate_trial),
            rate_feedback = mean(rate_feedback),
            PercNormalized = mean(PercNormalized),
            FrustNormalized = mean(FrustNormalized)) %>% View()


#############
# Perceived Control to False Positive Rate
#############

fig %>%
  add_trace(data = St, x=~mi_recog, y=~mi_recog_window+mi_recog_rest,
            type='scatter', mode='markers', color=I('white'), symbol=I('o'),
            marker=list(size=6, color = 'grey'),
            hoverinfo='text', text=~paste(Participant, Condition, GameOrder)) %>%
  layout(showlegend=F, yaxis = list(range=c(-0.05,120), title="Input Window MI", showticklabels=T),
         xaxis = list(range=c(0,120), title="Rest period MI"))


#############
# Perceived Control to MI rate (ignoring feedback) by Condition
#############
FabCurve100A <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "100A") %>% select(rate_trial, PercNormalized)
FabCurve100A.Smooth = supsmu(FabCurve100A$rate_trial, FabCurve100A$PercNormalized, bass=10)
FabCurve100A <- lm(PercNormalized ~ rate_trial, data = FabCurve100A)
FabCurve100A <- data.frame(x = (1:100)/100, y = predict(FabCurve100A, data.frame(rate_trial = (1:100)/100)))
FabCurve100B <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "100B") %>% select(rate_trial, PercNormalized)
FabCurve100B.Smooth = supsmu(FabCurve100B$rate_trial, FabCurve100B$PercNormalized, bass=10)
FabCurve100B <- lm(PercNormalized ~ rate_trial, data = FabCurve100B)
FabCurve100B <- data.frame(x = (1:100)/100, y = predict(FabCurve100B, data.frame(rate_trial = (1:100)/100)))
FabCurve50A <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "50A") %>% select(rate_trial, PercNormalized)
FabCurve50A.Smooth = supsmu(FabCurve50A$rate_trial, FabCurve50A$PercNormalized, bass=10)
FabCurve50A <- lm(PercNormalized ~ rate_trial, data = FabCurve50A)
FabCurve50A <- data.frame(x = (1:100)/100, y = predict(FabCurve50A, data.frame(rate_trial = (1:100)/100)))
FabCurve50B <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "50B") %>% select(rate_trial, PercNormalized)
FabCurve50B.Smooth = supsmu(FabCurve50B$rate_trial, FabCurve50B$PercNormalized, bass=10)
FabCurve50B <- lm(PercNormalized ~ rate_trial, data = FabCurve50B)
FabCurve50B <- data.frame(x = (1:100)/100, y = predict(FabCurve50B, data.frame(rate_trial = (1:100)/100)))


fig1 <- fig %>%
  add_trace(name = "100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_trial, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "100% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_trial, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=FabCurve100A, x=~x, y=~y, line=list(shape = 'spline'), color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
  add_trace(data=FabCurve100B, x=~x, y=~y, line=list(shape = 'spline'), color=I('grey70'), type='scatter', mode='lines', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="MI Activation"))

fig2 <- fig %>%
  add_trace(name = "50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~rate_trial, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "50% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~rate_trial, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=FabCurve50A, x=~x, y=~y, color=I('black'), line=list(shape = 'spline'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FabCurve50B, x=~x, y=~y, color=I('grey70'), line=list(shape = 'spline'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title=""), xaxis = list(range=c(-0.05,1.1), title="MI Activation"))

fig_c <- subplot(fig1, fig2, titleY = TRUE, titleX = TRUE) %>% 
  layout(title = 'Conditions w/ Fab. Input to Control Conditions')
fig_c

orca(fig_c, "fig/fabricated-input-to-control-trial-activation.pdf", width=950, height=350)


#############
# Bar chart depicting ratings
#############

fig1 <- fig %>%
  add_trace(data = St %>% filter(GameTitle == "HandStrength") %>% arrange(Participant, ConditionOrder),
            y=~PercNormalized, type='scatter', mode='markers+lines', color=I('grey80'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  add_trace(data = St %>% filter(GameTitle == "HandStrength") %>% arrange(Participant, ConditionOrder),
            y=~FrustNormalized, type='scatter', mode='markers', color=I('red'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  layout(showlegend=F, barmode='overlay', yaxis = list(title="Hand"),
         xaxis = list(range=c(-1,64), tickmode='array', tickvals=0:63,ticktext=~paste(Participant,Condition), title=""))  

fig2 <- fig %>%
  add_trace(data = St %>% filter(GameTitle == "Kiwi") %>% arrange(Participant, ConditionOrder),
            y=~PercNormalized, type='scatter', mode='markers+lines', color=I('grey80'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  add_trace(data = St %>% filter(GameTitle == "Kiwi") %>% arrange(Participant, ConditionOrder),
            y=~FrustNormalized, type='scatter', mode='markers', color=I('red'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  layout(showlegend=F, barmode='overlay', yaxis = list(title="Kiwi"),
         xaxis = list(range=c(-1,64), tickmode='array', tickvals=0:62, ticktext=~paste(Participant,Condition), title=""))  

fig_c <- subplot(fig1, fig2, titleY = TRUE, titleX = TRUE, nrows=2, margin=0.09) %>% 
  layout(title = 'Participant ratings of Perceived Control (grey) and Frustration (red)')
fig_c

 orca(fig_c, "fig/participant-rating-analysis-chart.pdf", width=950, height=475)



#############
# Bar chart depicting total number of activations, false positives, negatives
#############

fig1 <- fig %>%
  add_trace(data = St %>% filter(GameTitle == "HandStrength") %>% arrange(Participant, ConditionOrder), y=~mi_recog, type='bar', color=I('grey80'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  add_trace(data = St %>% filter(GameTitle == "HandStrength") %>% arrange(Participant, ConditionOrder), y=~mi_recog_window, type='bar', color=I('grey60'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  layout(showlegend=F, barmode='overlay', yaxis = list(title="Hand"),
         xaxis = list(range=c(-1,64), tickmode='array', tickvals=0:63,ticktext=~paste(Participant,Condition), title=""))  
fig1 <- fig1 %>%
  add_trace(data = tibble(x=c(-10,100), y=c(20,20)), x=~x, y=~y, type='scatter', color=I('black'),
            mode='lines', line=list(width=1))

fig2 <- fig %>%
  add_trace(data = St %>% filter(GameTitle == "Kiwi") %>% arrange(Participant, ConditionOrder), y=~mi_recog, type='bar', color=I('grey80'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  add_trace(data = St %>% filter(GameTitle == "Kiwi") %>% arrange(Participant, ConditionOrder), y=~mi_recog_window, type='bar', color=I('grey60'),
            marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
  layout(showlegend=F, barmode='overlay', yaxis = list(title="Kiwi"),
         xaxis = list(range=c(-1,64), tickmode='array', tickvals=0:62, ticktext=~paste(Participant,Condition), title=""))  

fig2 <- fig2 %>%
  add_trace(data = tibble(x=c(-10,100), y=c(20,20)), x=~x, y=~y, type='scatter', color=I('black'),
            mode='lines', line=list(width=1))

fig_c <- subplot(fig1, fig2, titleY = TRUE, titleX = TRUE, nrows=2, margin=0.09) %>% 
  layout(title = 'Motor Imagery Detection Counts')
fig_c

orca(fig_c, "fig/motor-imagery-event-count-bar-charts.pdf", width=950, height=475)


#############
# Perceived Control to MI-attempt-based Control (Feedback rate) by Condition (Linear)
#############

  
fig1 <- fig %>%
  add_trace(name= "100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_feedback, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "100% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_feedback, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  layout(yaxis = list(range=c(-0.05,1.1)), xaxis = list(range=c(-0.05,1.1), title="Outcome-based control (No Limit)"))

fig2 <- fig %>% 
  add_trace(name = "50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate_feedback, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "50% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate_feedback, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('square-open'), marker=list(size=7)) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="", showticklabels=F), xaxis = list(range=c(-0.05,1.1), title="Outcome-based Control (50% Limit)"))

fig_c <- subplot(fig1, fig2, titleY = TRUE, titleX = TRUE) %>% 
  layout(title = 'Conditions w/ Fab. Input to Control Conditions')
fig_c

orca(fig_c, "fig/fabricated-input-to-control-feedback-rate.pdf", width=950, height=350)
#############
# Perceived Control to Outcome-based Control by Condition
#############

# Story 1: We can move people upwards, if we provide them fabricated input.
FabCurve100A <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "100A") %>% select(rate_feedback, PercNormalized)
FabCurve100A <- lm(PercNormalized ~ rate_feedback, data = FabCurve100A)
FabCurve100A <- data.frame(x = (1:100)/100, y = predict(FabCurve100A, data.frame(rate_feedback = (1:100)/100)))
FabCurve100B <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "100B") %>% select(rate_feedback, PercNormalized)
FabCurve100B <- lm(PercNormalized ~ rate_feedback, data = FabCurve100B)
FabCurve100B <- data.frame(x = (1:100)/100, y = predict(FabCurve100B, data.frame(rate_feedback = (1:100)/100)))
FabCurve50A <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "50A") %>% select(rate_feedback, PercNormalized)
FabCurve50A <- lm(PercNormalized ~ rate_feedback, data = FabCurve50A)
FabCurve50A <- data.frame(x = (1:50)/100, y = predict(FabCurve50A, data.frame(rate_feedback = (1:50)/100)))
FabCurve50B <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "50B") %>% select(rate_feedback, PercNormalized)
FabCurve50B <- lm(PercNormalized ~ rate_feedback, data = FabCurve50B)
FabCurve50B <- data.frame(x = (1:50)/100, y = predict(FabCurve50B, data.frame(rate_feedback = (1:50)/100)))

fig1 <- fig %>%
  add_trace(name= "100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_feedback, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "100% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_feedback, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=FabCurve100A, x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FabCurve100B, x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1)), xaxis = list(range=c(-0.05,1.1), title="Outcome-based Control (No Limit)"))

fig2 <- fig %>% 
  add_trace(name = "50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate_feedback, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "50% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate_feedback, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('square-open'), marker=list(size=7)) %>%
  add_trace(data=FabCurve50A, x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FabCurve50B, x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="", showticklabels=F), xaxis = list(range=c(-0.05,1.1), title="Outcome-based Control (50% Limit)"))

fig_c <- subplot(fig1, fig2, titleY = TRUE, titleX = TRUE) %>% 
  layout(title = 'Conditions w/ Fab. Input to Control Conditions')

orca(fig_c, "fig/fabricated-input-to-control.pdf", width=950, height=350)
