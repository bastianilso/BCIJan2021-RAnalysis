library(plotly)
library(lubridate)
library(tidyverse)
library(gsheet)
library(lme4) # for linear mixed models
library(MuMIn) # for R-squared values that measures how much variance is explained.
source("utils/visutils.R")

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
            posTrial = sum(TrialResult == "FabInput" | TrialResult == "AccInput"),
            negTrial = sum(TrialResult == "RejInput"),
            otherInput = sum(!TrialResult %in% c("RejInput", "AccInput", "FabInput")),
            BCIProcessingMode = unique(BCIProcessingMode),
            GameTitle = unique(GameTitle),
            ConditionOrder = unique(OrderAll),
            totalTrials = rejInput+accInput+fabInput,
            PercNormalized = unique(PercNormalized),
            PercNormalized_first_diff = unique(PercNormalized_first_diff),
            PercNormalized_prev_diff = unique(PercNormalized_prev_diff),
            PercNormalized_rel = unique(PercNormalized_rel),
            FrustNormalized = unique(FrustNormalized),
            GameOrder = unique(GameOrder),
            gender = unique(Gender),
            comment = unique(Comment),
            color = unique(Color),
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
  summarize(mi_recog_window = ifelse(sum(Event == "MotorImagery") > 0, 1,0), #Whether MI happened in the window
            mi_recog_window_count = sum(Event == "MotorImagery"), #How much MI happened in the window
            time_window = sum(time_delta)) %>%
  filter(InputWindowOrderFilled > -1) %>% ungroup() %>% group_by(GameTitle, Participant, Condition) %>%
  summarize(mi_recog_trial = sum(mi_recog_window > 0),
            mi_recog_window = sum(mi_recog_window),
            time_window = sum(time_window),
            time_window_min = time_window / 60) %>%
  right_join(St)

St <- St %>% ungroup() %>%
  mutate(rate_trial = (mi_recog_trial)/totalTrials,
         rate_feedback = posTrial/ totalTrials,
         rate_fabInput = fabInput/totalTrials,
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
            mi_experience = unique(mi_experience),
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

# Group people and count them based on 
# Group Low: Participants with very low (more than half feedback was negative)
# Group Exact: Participants close to the target rate with few false positives.
# Group FP: Participants with more than double the false positives.
St <- St %>% mutate(mi_group = "ExactGroup",
                  mi_group = ifelse(mi_recog_window < (totalTrials*0.5), "LowGroup",mi_group),
                  mi_group = ifelse(mi_recog > (totalTrials*2), "FPGroup",mi_group))

St %>% ungroup() %>% group_by(GameTitle, mi_group) %>%
  summarize(count = n())

## Create Participant Groups.
#Inclusion Criteria 1: Participants with max ratings below 50%
St = St %>% group_by(GameTitle, Participant) %>% select(rate_feedback) %>%
  summarise(rate_feedback = mean(rate_feedback)) %>% 
  mutate(group_feedback = "MID",
         group_feedback = ifelse(rate_feedback < 0.4, "LOW", group_feedback),
         group_feedback = ifelse(rate_feedback > 0.8, "HIGH", group_feedback)) %>% 
  select(GameTitle,Participant,group_feedback) %>% right_join(St)

# Inclusion Criteria 2: Participants with high feedback variation (Range)
St = St %>% group_by(GameTitle, Participant) %>% select(rate_feedback) %>%
  summarise(rate_feedback_range = max(rate_feedback) - min(rate_feedback)) %>%
  mutate(group_feedback_range = "MID",
         group_feedback_range = ifelse(rate_feedback_range <= 0.25, "LOW", group_feedback_range),
         group_feedback_range = ifelse(rate_feedback_range > 0.5, "HIGH", group_feedback_range)) %>%
  select(GameTitle, Participant, group_feedback_range) %>% right_join(St)

# Inclusion Criteria 3: Participants with high mean ratings (60-100%, medium 40%-60%, low <40%)
St = St %>% group_by(GameTitle, Participant) %>% select(rate_trial) %>%
  summarise(rate_trial = min(rate_trial)) %>% 
  mutate(group_trial = "MID",
         group_trial = ifelse(rate_trial < 0.4, "LOW", group_trial),
         group_trial = ifelse(rate_trial > 0.75, "HIGH", group_trial)) %>% 
  select(GameTitle,Participant,group_trial) %>% right_join(St)

# Group people and count them based on
# Group low variability: Participants which produced a consistent number of MI events.
# Group high variability: Participants with high variability in MI events.
St %>% ungroup() %>% group_by(GameTitle, Participant) %>%
  summarize(mi_stability = ifelse(length(unique(mi_group)) > 1, "unstable", "stable"),
            stability_type = paste(unique(mi_group), collapse=",")) %>% 
  ungroup() %>% group_by(GameTitle, stability_type) %>%
  summarize(count = n(),
            participant = paste(unique(Participant), collapse=","))

############
#  Aggregate summary of variation and low recognition.
############
# Calculate variation change for each participant
St %>%
  group_by(Participant, GameTitle) %>%
  summarize(median_value = median(rate_trial),
            min_value = min(rate_trial),
            max_value = max(rate_trial),
            percentage_change_max = ((max_value - median_value ) / max_value) * 100,
            percentage_change_min = ((median_value - min_value )  / median_value) * 100
  ) %>%
  filter(percentage_change_max < 25, percentage_change_min < 25)

# Calculate who had less than half the expected MI events for each participant
St %>%
  group_by(Participant, GameTitle) %>%
  summarize(max_value = max(rate_trial)) %>%
  filter(max_value < 0.5)


#############
# Rough visual inspection of people's ratings to the feedback rate
#############

fig_g0 <- St %>% filter(group_trial == 'LOW') %>% group_by(GameTitle, Participant) %>%
  arrange(rate_feedback) %>% 
  do(p=plot_ly(., type='bar') %>%
       add_trace(name="Feedback rate", x=~rate_feedback, y=~rate_feedback, width =.065, color=I('grey80'), type='bar') %>%
       add_trace(name="Fabricated Input", x=~rate_feedback, y=~rate_fabInput, width=.065, color=I('grey60'), type='bar') %>%
       add_trace(name="Perceived Control",x=~jitter(rate_feedback,amount=.02), y=~PercNormalized, color=I('black'), 
                 type='scatter', mode='lines+markers') %>%
       add_trace(name="Frustration",x=~jitter(rate_feedback,amount=.02), y=~FrustNormalized, color=I('FireBrick'), 
                 type='scatter', mode='lines+markers') %>%
       layout(showlegend=T, annotations=list(showarrow=F,x=0.5,y=-0.1,text=~paste(GameTitle, Participant)),
              barmode='overlay', xaxis=list(tickvals="", title='', range=c(-0.1,1.1)),
              yaxis=list(linecolor = toRGB("black"), linewidth = 1, showline = TRUE,
                         tickvals = list(0, 0.16,0.33, 0.5,0.66, 0.83, 1),
                         title='', range=c(-0.1,1.1),showticklabels=F))) %>%
  subplot(nrows=1, shareX =T, shareY =F, heights=c(0.8), margin=0.001) %>% 
  layout(showlegend=F,margin=list(l=0,r=0,b=0),
         yaxis=list(linecolor = toRGB("black"), linewidth = 1, showline = TRUE,
                    tickvals = list(0, 0.16,0.33, 0.5,0.66, 0.83, 1),
                    title='', range=c(-0.1,1.1), showticklabels=T),
         title='Group 1: Participants with Low MI rates<sup>
         Inclusion Criteria: Mean MI rate lower than 40%')

orca(fig_g0, "fig/study1-g0_group_low_mi.pdf", width=850, height=200)


fig_g1 <- St %>% filter(group_trial == 'HIGH') %>% group_by(GameTitle, Participant) %>%
  arrange(rate_feedback) %>% 
  do(p=plot_ly(., type='bar',mode='overlay') %>%
       add_trace(name="Feedback rate", x=~rate_feedback, y=~rate_feedback, width =.065, color=I('grey80'), type='bar',mode='overlay') %>%
       add_trace(name="Fabricated Input", x=~rate_feedback, y=~rate_fabInput, width=.065, color=I('grey60'), type='bar',mode='overlay') %>%
       add_trace(name="Perceived Control",x=~jitter(rate_feedback,amount=.02), y=~PercNormalized, color=I('black'), 
                 type='scatter', mode='lines+markers') %>%
       add_trace(name="Frustration",x=~jitter(rate_feedback,amount=.02), y=~FrustNormalized, color=I('FireBrick'), 
                 type='scatter', mode='lines+markers') %>%
       layout(showlegend=T, annotations=list(showarrow=F,x=0.5,y=-0.1,text=~paste(GameTitle, Participant)),
              barmode='overlay', xaxis=list(tickvals="", title='', range=c(-0.1,1.1)),
              yaxis=list(linecolor = toRGB("black"), linewidth = 1, showline = TRUE,
                         tickvals = list(0, 0.16,0.33, 0.5,0.66, 0.83, 1),
                         title='', range=c(-0.1,1.1), showticklabels=F))) %>%
  subplot(nrows=1, shareX =T, shareY =F, heights=c(0.8), margin=0.001) %>% 
  layout(showlegend=F,margin=list(l=0,r=0,b=0),
         yaxis=list(linecolor = toRGB("black"), linewidth = 1, showline = TRUE,
                    tickvals = list(0, 0.16,0.33, 0.5,0.66, 0.83, 1),
                    title='', range=c(-0.1,1.1), showticklabels=T),
         title='Group 2: Participants with High MI Rates<sup>
         Inclusion Criteria: Mean MI rate higher than 75%')

orca(fig_g1, "fig/study1-g1_group_high_mi.pdf", width=950, height=200)

fig_g2 <- St %>% filter(group_feedback_range == 'HIGH') %>% group_by(GameTitle, Participant) %>%
  arrange(rate_feedback) %>% 
  do(p=plot_ly(., type='bar',mode='overlay') %>%
       add_trace(name="Feedback rate", x=~rate_feedback, y=~rate_feedback, width =.065, color=I('grey80'), type='bar',mode='overlay') %>%
       add_trace(name="Fabricated Input", x=~rate_feedback, y=~rate_fabInput, width=.065, color=I('grey60'), type='bar',mode='overlay') %>%
       add_trace(name="Perceived Control",x=~jitter(rate_feedback,amount=.02), y=~PercNormalized, color=I('black'), 
                 type='scatter', mode='lines+markers') %>%
       add_trace(name="Frustration",x=~jitter(rate_feedback,amount=.02), y=~FrustNormalized, color=I('FireBrick'), 
                 type='scatter', mode='lines+markers') %>%
       layout(showlegend=T, annotations=list(showarrow=F,x=0.5,y=-0.1,text=~paste(GameTitle, Participant)),
              barmode='overlay', xaxis=list(tickvals="", title='', range=c(-0.1,1.1)),
              yaxis=list(linecolor = toRGB("black"), linewidth = 1, showline = TRUE,
                         tickvals = list(0, 0.16,0.33, 0.5,0.66, 0.83, 1),
                         title='', range=c(-0.1,1.1),showticklabels=F))) %>%
  subplot(nrows=1, shareX =T, shareY =F, heights=c(0.8), margin=0.001) %>% 
  layout(showlegend=F,margin=list(l=0,r=0,b=0),
         yaxis=list(linecolor = toRGB("black"), linewidth = 1, showline = TRUE,
                    tickvals = list(0, 0.16,0.33, 0.5,0.66, 0.83, 1),
                    title='', range=c(-0.1,1.1), showticklabels=T),
         title='Group 2: Participants with High Feedback Variance<sup>
         Inclusion Criteria: More than 50% variance in feedback')

orca(fig_g2, "fig/study1-g2_high_feedback_variance.pdf", width=850, height=200)

export <- St %>% filter(GameTitle == "Kiwi") %>% ungroup %>% arrange(Participant) %>% select(Participant, Condition, FrustNormalized, rate_feedback) %>% rename(Frustration = FrustNormalized, control = rate_feedback)
save(export, file = 'export.rda', compress=TRUE)
  

#############
# Latex Table with demographic and summary information
#############

Sp_table <- Sp %>% select(mi_experience, mean_rate, min_rate, max_rate, mean_feedback, time_total,
                          fp_rate_minute, gender, GameTitle) %>%
                    mutate(mi_experience = ifelse(mi_experience, "Yes", "No"),
                    mean_rate = paste(format(round(mean_rate * 100,0), nsmall = 0),"\\%"),
                    min_rate = paste(format(round(min_rate * 100,0), nsmall = 0),"\\%"),
                    max_rate = paste(format(round(max_rate * 100, 0), nsmall = 0),"\\%"),
                    mean_feedback = paste(format(round(mean_feedback * 100, 0), nsmall = 0), "\\%"),
                    time_total = format(round(time_total,0), nsmall = 0),
                    fp_rate_minute = format(round(fp_rate_minute,2), nsmall = 2),
                    across(everything(), as.character))
Sp_table <- Sp_table %>%
             rename(Gender = gender, `Mean Rate` = mean_rate, `Min. Rate` = min_rate,
                    `Max. Rate` = max_rate, `Mean Pos. Feedback` = mean_feedback, `MI Experience` = mi_experience,
                    `Mean Time (s)` = time_total, `False Positives (pr min)` = fp_rate_minute)

Sp_table = Sp_table %>% pivot_longer(cols=-c(Participant, GameTitle), names_to = "Feature") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
message(paste(Sp_table %>% apply(.,1,paste,collapse=" & "), collapse=" \\\\ "))


  group_by(Feature) %>% summarise(text = paste(fm_value, sep = " & ", collapse = " & "),
  text = paste(text, "\\ ")) %>% V

  
#############
# Check for Balance in observations (St)
#############
# Check for overall balance.
# Expected balance: 
# Conclusion: Except P11, All rated had the same number of conditions/colors
St %>% group_by(GameTitle) %>% filter(Participant != 11) %>%
    summarize(Con_50A = sum(Condition == '50A'),
              Con_50B = sum(Condition == '50B'),
              Con_100A = sum(Condition == '100A'),
              Con_100B = sum(Condition == '100B'),
              Gender_M = sum(gender == 'M'),
              Gender_F = sum(gender == 'F'),
              KiwiHand = sum(GameOrder == 'KiwiHand'),
              HandKiwi = sum(GameOrder == 'HandKiwi'),
              Hand = sum(GameTitle == 'HandStrength'),
              Kiwi = sum(GameTitle == 'Kiwi'),
              Color_yellow = sum(color == 'Yellow'),
              Color_blue = sum(color == 'Blue'),
              Color_grey = sum(color == 'Grey'),
              Color_red = sum(color == 'Red'),
              Color_green = sum(color == 'Green')) %>% view()

St %>% group_by(GameTitle, Participant) %>%
  summarize(Con_50A = sum(Condition == '50A'),
            Con_50B = sum(Condition == '50B'),
            Con_100A = sum(Condition == '100A'),
            Con_100B = sum(Condition == '100B'),
            Gender_M = sum(gender == 'M'),
            Gender_F = sum(gender == 'F'),
            KiwiHand = sum(GameOrder == 'KiwiHand'),
            HandKiwi = sum(GameOrder == 'HandKiwi'),
            Hand = sum(GameTitle == 'HandStrength'),
            Kiwi = sum(GameTitle == 'Kiwi'),
            Color_yellow = sum(color == 'Yellow'),
            Color_blue = sum(color == 'Blue'),
            Color_grey = sum(color == 'Grey'),
            Color_red = sum(color == 'Red'),
            Color_green = sum(color == 'Green')) %>% view()

# Check for balance within color order
# Conclusion: Imbalanced
St %>% group_by(GameTitle, Participant) %>% arrange(ConditionOrder) %>%
  summarize(color_order = paste0(color, collapse='')) %>%
  group_by(color_order) %>%
  summarize(n = n()) %>% view()

# Check for balance within condition order
# Conclusion: Imbalanced
St %>% group_by(GameTitle, Participant) %>% arrange(ConditionOrder) %>%
  summarize(condition_order = paste0(Condition, collapse='')) %>%
  group_by(condition_order) %>%
  summarize(n = n()) %>% view()

#Q: How do we make up for imbalance in our data set?
# 1) We can use a "Type III ANOVA"


#############
# Repeated-measures friedman test
#############
  
#friedman.test.with.post.hoc(FrustrationEpisode ~ as.factor(Condition) | as.factor(Partcipant), D_excl %>% )
D_fry <- D_excl %>% dplyr::select(PerceivedControlEpisode,FrustrationEpisode, Condition, Participant) %>% filter(Condition != "100")
D_fry <- as.data.frame(D_fry)
D_fry$Condition = as.factor(D_fry$Condition)
D_fry$Participant = as.integer(D_fry$Participant) # Note to self: friedman test requires participant numbers as integers, not factor.
friedman.test(FrustrationEpisode~Condition|Participant, D_fry)
friedman.test(PerceivedControlEpisode~Condition|Participant, D_fry)

#############
# Linear regression models
#############
# Since we collect more than one data point from each subject,
# our data is not independent. 
# For multiple measures per subject, you need to use mixed models.
# "Multiple responses from the same subject cannot be regarded as independent from each other".
# Random intercept model: A model where we assume participant provide different intercepts,
# but where we expect the slope to be the same. (1|Participant)
# Random slope model: A model where we assume participants also provide different slopes.
# (1+rate_feedback|Participant)

# Perceived Control to Positive Feedback
model.null = lmer(PercNormalized ~ (1|Participant), 
                  data = St, REML=F)
model.feedback = lmer(PercNormalized ~ rate_feedback + (1|Participant),
                      data = St, REML=F)

model.trial = lmer(PercNormalized ~ rate_trial + (1|Participant),
                      data = St, REML=F)

# Likelihood Ratio Test using ANOVA
anova(model.null, model.feedback)
anova(model.null, model.trial)

#Perceived Control to Fabricated Input
model.null = lmer(PercNormalized ~ (1|Participant), 
                  data = St, REML=F)
model.feedback = lmer(PercNormalized ~ rate_feedback + (1|Participant),
                      data = St, REML=F)
model.feedbackfab = lmer(PercNormalized ~ rate_feedback + rate_fabInput + (1|Participant),
                      data = St, REML=F)

# Likelihood Ratio Test using ANOVA
anova(model.null, model.fab)
anova(model.feedback, model.feedbackfab)

#Q: When we have different representations of the "same thing" - e.g. variables,
# how do we then test which variable is the better predictor?
#A: Use R-Squared to make an estimate of the variance the variable explains

r.squaredGLMM(model.fab, null=model.null)
r.squaredGLMM(model.feedbackfab, null=model.null)
r.squaredGLMM(model.trial, null=model.null)

#Frustration  to Positive Feedback
model.null = lmer(FrustNormalized ~ (1|Participant), 
                  data = St, REML=F)
model.feedback = lmer(FrustNormalized ~ rate_feedback + (1|Participant),
                      data = St, REML=F)
model.feedbackgame = lmer(FrustNormalized ~ rate_feedback + GameTitle + (1|Participant),
                      data = St, REML=F)
model.trial = lmer(FrustNormalized ~ rate_trial + (1|Participant),
                   data = St, REML=F)

boxplot(FrustNormalized ~ GameTitle, data = St) #GameTitle predicts frustration
St %>% group_by(GameTitle) %>% summarize(frust = mean(FrustNormalized),
                                         rate_feedback = mean(rate_feedback))
# Likelihood Ratio Test using ANOVA
anova(model.null, model.frust)
anova(model.null, model.feedback)
anova(model.feedback, model.feedbackgame)
anova(model.null, model.trial)

r.squaredGLMM(model.feedbackgame, null=model.null)
r.squaredGLMM(model.feedback, null=model.null)

#Positive Feedback Rate to Game
model.null = lmer(rate_feedback ~ (1|Participant), 
                  data = St, REML=F)
model.game = lmer(rate_feedback ~ GameTitle + (1|Participant),
                      data = St, REML=F)

# Likelihood Ratio Test using ANOVA
anova(model.null, model.game)


#############
# General range of people's control over the cap.
#############
D %>% 

#############
# Does Positive Feedback increase MI rate?
#############

summary(lm(mi_recog ~ rate_feedback, data = St))

fig %>%
  add_trace(data = St, type = 'bar')

#############
# Faceted plots of what drives people's ratings.
#############

# Perceived Control to Rate Feedback
St %>% filter(GameTitle == 'HandStrength') %>% group_by(Participant) %>% arrange(rate_feedback) %>%
  do(p=plot_ly(.,x=~rate_feedback, y=~PercNormalized_rel, color=I('black'), 
               type='scatter', mode='lines+markers') %>%
       add_text(text=~Participant, x=0.15, y=0.9, color=I('red')) %>%
       layout(xaxis=list(title='', range=c(0,1.1)), yaxis=list(title='', range=c(0,1.1)))) %>%
  subplot(nrows=5, shareX =T, shareY =T) %>% 
  layout(showlegend=F, xaxis=list(title='Feedback Rate'), yaxis=list(title='Perceived Control'))

# Perceived Control to MI Rate
St %>% filter(GameTitle == 'Kiwi') %>% group_by(Participant) %>% arrange(rate_trial) %>%
  do(p=plot_ly(.,x=~rate_trial, y=~PercNormalized_rel, color=I('black'), 
               type='scatter', mode='lines+markers') %>%
       add_text(text=~Participant, x=0.15, y=0.9, color=I('red')) %>%
       layout(xaxis=list(title='', range=c(0,1.1)), yaxis=list(title='', range=c(0,1.1)))) %>%
  subplot(nrows=5, shareX =T, shareY =T) %>% 
  layout(showlegend=F, xaxis=list(title='MI Rate'), yaxis=list(title='Perceived Control'))

# Perceived Control to Play Order
St %>% filter(GameTitle == 'Kiwi') %>% group_by(Participant) %>% arrange(ConditionOrder) %>%
  do(p=plot_ly(.,x=~ConditionOrder, y=~PercNormalized_rel, color=I('black'), 
               type='scatter', mode='lines+markers') %>%
       add_text(text=~Participant, x=0.4, y=0.9, color=I('red')) %>%
       layout(xaxis=list(title='', range=c(0,11)), yaxis=list(title='', range=c(0,1.1)))) %>%
  subplot(nrows=5, shareX =T, shareY =T) %>% 
  layout(showlegend=F, xaxis=list(title='Play Order'), yaxis=list(title='Perceived Control'))

# Perceived Control to Condition
St %>% filter(GameTitle == 'Kiwi') %>% group_by(Participant) %>% arrange(ConditionOrder) %>%
  do(p=plot_ly(.,x=~factor(Condition, levels = c(1, 2)), y=~PercNormalized_rel, color=I('black'), 
               type='scatter', mode='lines+markers') %>%
       add_text(text=~Participant, x=0.4, y=0.9, color=I('red')) %>%
       layout(xaxis=list(title='', range=c(-1,5)), yaxis=list(title='', range=c(0,1.1)))) %>%
  subplot(nrows=5, shareX =T, shareY =T) %>% 
  layout(showlegend=F, xaxis=list(title='Condition'), yaxis=list(title='Perceived Control'))



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
  add_trace(data = St %>% filter(Participant == 1), x=~Condition, y=~(jitter(rate_trial,amount=0.03)),
            type='scatter', mode='markers', color=~GameTitle, symbol=I('o'),
            marker=list(size=~20*(PercNormalized)),
            hoverinfo='text', text=~paste(PercNormalized, Participant, Condition, GameTitle, ConditionOrder)) %>%
  layout(showlegend=F, yaxis = list(range=c(-0.05,1.1), title="Perceived Control", showticklabels=T),
         xaxis = list(title="Condition"))


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
  add_trace(name = "100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_trial, y=~jitter(PercNormalized_prev_diff, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "100% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_trial, y=~jitter(PercNormalized_prev_diff, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=FabCurve100A, x=~x, y=~y, line=list(shape = 'spline'), color=I('black'), type='scatter', mode='lines', showlegend=F) %>%
  add_trace(data=FabCurve100B, x=~x, y=~y, line=list(shape = 'spline'), color=I('grey70'), type='scatter', mode='lines', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="MI Activation"))

fig2 <- fig %>%
  add_trace(name = "50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~rate_trial, y=~jitter(PercNormalized_prev_diff, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "50% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~rate_trial, y=~jitter(PercNormalized_prev_diff, amount=.02),
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
# Bar chart depicting mi recog to positive feedback rate.
#############
 
fig %>%
   add_trace(data = St, x=~jitter(rate_feedback,amount=.02), 
             y=~jitter( mi_recog_window - (mi_recog_rest / (time_rest),amount=.02), type='scatter', color=~Condition,
             marker = list(line = list(width = 0, color = 'rgb(0, 0, 0)'))) %>%
   add_trace(data = tibble(x=c(0,1),y=c(0,20)), x=~x, y=~y, type='scatter', mode='lines',
             line = list(width = 1, color = 'rgb(0, 0, 0)'))

# TODO: Normalize the mi_recog_window and mi_recog_rest by the time spent in each.
 
 
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
# Perceived Control to Outcome-based Control by Condition
#############

PercLine <- list("100A" = p_lin(St %>% filter(Condition == "100A"),"PercNormalized", "rate_trial"),
                 "100B" = p_lin(St %>% filter(Condition == "100B"),"PercNormalized", "rate_trial"),
                 "50A" = p_lin(St %>% filter(Condition == "50A"),"PercNormalized", "rate_trial"),
                 "50B" = p_lin(St %>% filter(Condition == "50B"),"PercNormalized", "rate_trial"))

FrustLine <- list("100A" = p_lin(St %>% filter(Condition == "100A"),"FrustNormalized", "rate_trial"),
                 "100B" = p_lin(St %>% filter(Condition == "100B"), "FrustNormalized", "rate_trial"),
                 "50A" = p_lin(St %>% filter(Condition == "50A"),"FrustNormalized", "rate_trial"),
                 "50B" = p_lin(St %>% filter(Condition == "50B"),"FrustNormalized", "rate_trial"))

fig1 <- fig %>%
  add_trace(name= "0-100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_trial, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "0-100% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_trial, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=PercLine[["100A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=PercLine[["100B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="MI Control"))

fig2 <- fig %>% 
  add_trace(name = "0-50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate_trial, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "0-50% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate_trial, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('square-open'), marker=list(size=7)) %>%
  add_trace(data=PercLine[["50A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=PercLine[["50B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="", showticklabels=F), xaxis = list(range=c(-0.05,1.1), title="MI Control (50% Negatives)"))

fig3 <- fig %>%
  add_trace(name= "0-100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_trial, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "0-100% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_trial, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=FrustLine[["100A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FrustLine[["100B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Frustration"), xaxis = list(range=c(-0.05,1.1), title="MI Control"))

fig4 <- fig %>% 
  add_trace(name = "0-50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate_trial, amount=.02), y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "0-50% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate_trial, amount=.02), y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('square-open'), marker=list(size=7)) %>%
  add_trace(data=FrustLine[["50A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FrustLine[["50B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="", showticklabels=F), xaxis = list(range=c(-0.05,1.1), title="MI Control (50% Negatives)"))



fig_c <- subplot(fig1,fig2, titleY = TRUE, titleX = TRUE) %>% 
  layout(legend=list(orientation='h', x = 0.20, y = 1.12), title = '')
fig_c
orca(fig_c, "fig/study1-perc-mi-control-fabricated-input.pdf", width=650, height=350)
fig_c <- subplot(fig3,fig4, titleY = TRUE, titleX = TRUE) %>% 
  layout(legend=list(orientation='h', x = 0.20, y = 1.12), title = '')
fig_c
orca(fig_c, "fig/study1-frust-mi-control-fabricated-input.pdf", width=650, height=350)



#############
# Perceived Control to Feedback Rate by Condition
#############

PercLine <- list("100A" = p_lin(St %>% filter(Condition == "100A"),"PercNormalized", "rate_feedback"),
                 "100B" = p_lin(St %>% filter(Condition == "100B"),"PercNormalized", "rate_feedback"),
                 "50A" = p_lin(St %>% filter(Condition == "50A"),"PercNormalized", "rate_feedback"),
                 "50B" = p_lin(St %>% filter(Condition == "50B"),"PercNormalized", "rate_feedback"))

FrustLine <- list("100A" = p_lin(St %>% filter(Condition == "100A"),"FrustNormalized", "rate_feedback"),
                  "100B" = p_lin(St %>% filter(Condition == "100B"), "FrustNormalized", "rate_feedback"),
                  "50A" = p_lin(St %>% filter(Condition == "50A"),"FrustNormalized", "rate_feedback"),
                  "50B" = p_lin(St %>% filter(Condition == "50B"),"FrustNormalized", "rate_feedback"))

fig1 <- fig %>%
  add_trace(name= "0-100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_feedback, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "30-100%", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_feedback, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('grey70'), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "0-50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate_feedback, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "30-80%", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate_feedback, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('grey70'), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('circle'), marker=list(size=7)) %>%
  add_trace(name='0-50%', data=PercLine[["50A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', line=list(dash = 'dash'), showlegend=T, name='0-50%') %>%
  add_trace(name='30-80%', data=PercLine[["50B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line',line=list(dash = 'dash'), showlegend=T) %>%
  add_trace(name = "0-100%", data=PercLine[["100A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=T, name='0-100%') %>%
  add_trace(name = "30-100%", data=PercLine[["100B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=T) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Perceived Control"), xaxis = list(range=c(-0.05,1.1), title="Feedback"))


fig2 <- fig %>%
  add_trace(name= "0-100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate_feedback, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "30-100%", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate_feedback, y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I('grey70'), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "0-50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate_feedback, amount=.02), y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "30-80%", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate_feedback, amount=.02), y=~jitter(FrustNormalized, amount=.02),
            type='scatter',mode='markers', color=I('grey70'), hoverinfo='text',text=~paste(Participant,Condition, GameTitle), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name='0-50%', data=FrustLine[["50A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', line=list(dash = 'dash'), showlegend=T, name='0-50%') %>%
  add_trace(name='30-80%', data=FrustLine[["50B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line',line=list(dash = 'dash'), showlegend=T) %>%
  add_trace(name = "0-100%", data=FrustLine[["100A"]], x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=T, name='0-100%') %>%
  add_trace(name = "30-100%", data=FrustLine[["100B"]], x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=T) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Frustration"), xaxis = list(range=c(-0.05,1.1), title="Feedback"))
fig1
fig2
orca(fig1, "fig/study1-feedback-rate-to-perceived-control.pdf", width=650, height=350)
orca(fig2, "fig/study1-feedback-rate-to-frustration.pdf", width=650, height=350)
#############
# Condition Design to Actuality 
#############



fig1 <- fig %>%
  add_trace(name='100%', data = data.frame(x=c(0,18),y=c(1.0,1.0)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='0%', data = data.frame(x=c(0,18),y=c(0,0)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='Hand Strengthener', data = St %>% filter(GameTitle %in% c("HandStrength"), Condition == "100A"),
            color=I('black'),symbol=I('circle'), x=~Participant, y=~rate_feedback) %>%
  add_trace(name='Kiwi', data = St %>% filter(GameTitle %in% c("Kiwi"), Condition == "100A"),
            color=I('black'),symbol=I('x'), x=~Participant, y=~rate_feedback) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="Feedback Rate"), xaxis = list(title="Condition 0-100%"))

fig2 <- fig %>%
  add_trace(name='30%', data = data.frame(x=c(0,18),y=c(0.3,0.3)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='100%', data = data.frame(x=c(0,18),y=c(1.0,1.0)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='30-100%', data = St %>% filter(GameTitle %in% c("HandStrength"), Condition == "100B"),
            color=I('black'), symbol=I('circle'), x=~Participant, y=~rate_feedback, showlegend=F) %>%
  add_trace(name='30-100%', data = St %>% filter(GameTitle %in% c("Kiwi"), Condition == "100B"),
            color=I('black'), symbol=I('x'), x=~Participant, y=~rate_feedback, showlegend=F) %>%  
  layout(yaxis = list(range=c(-0.05,1.1), title=" ", showticklabels=F), xaxis = list(title="Condition 30-100%"))

fig3 <- fig %>%
  add_trace(name='50%', data = data.frame(x=c(0,18),y=c(0.5,0.5)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='0%', data = data.frame(x=c(0,18),y=c(0,0)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='0-50%', data = St %>% filter(GameTitle %in% c("HandStrength"), Condition == "50A"),
            color=I('black'), symbol=I('circle'), x=~Participant, y=~rate_feedback,showlegend=F) %>%
  add_trace(name='0-50%', data = St %>% filter(GameTitle %in% c("Kiwi"), Condition == "50A"),
            color=I('black'), symbol=I('x'), x=~Participant, y=~rate_feedback, showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title=" ", showticklabels=F), xaxis = list(title="Condition 0-50%"))

fig4 <- fig %>%
  add_trace(name='30%', data = data.frame(x=c(0,18),y=c(0.3,0.3)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='80%', data = data.frame(x=c(0,18),y=c(0.8,0.8)), x=~x, y=~y,
            type='scatter',mode='lines', color=I('black'), showlegend=F) %>%
  add_trace(name='30-80%', data = St %>% filter(GameTitle %in% c("HandStrength"), Condition == "50B"),
            color=I('black'), symbol=I('circle'), x=~Participant, y=~rate_feedback, showlegend=F) %>%
  add_trace(name='30-80%', data = St %>% filter(GameTitle %in% c( "Kiwi"), Condition == "50B"),
            color=I('black'), symbol=I('x'), x=~Participant, y=~rate_feedback, showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title=" ", showticklabels=F), xaxis = list(title="Condition 30-80%"))
fig_c <- subplot(fig1,fig2,fig3,fig4, titleY = TRUE, titleX = TRUE, margin=0.005) %>% 
  layout(legend=list(orientation='h', x = 0.0, y = 1.12), title = '')
fig_c
orca(fig_c, "fig/study1-level-of-control-participant.pdf", width=950, height=350)
