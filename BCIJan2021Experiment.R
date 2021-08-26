library(plotly)
library(lubridate)
library(gsheet)
library(dplyr)

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
            totalTrials = rejInput+accInput+fabInput,
            PercNormalized = unique(PercNormalized),
            FrustNormalized = unique(FrustNormalized),
            gender = unique(Gender),
            bci_experience = unique(Comment)
            )
St <- St %>% ungroup() %>%
  mutate(rate = (accInput)/totalTrials,
         session = 1,
         session = cumsum(session))

# Sc = Summary of Conditions
Sc <- St %>% ungroup() %>% group_by(GameTitle, Condition) %>%
  summarize(avg_rate = mean(rate),
            sum_acc = sum(accInput))

# Sp = Summary of Participants
Sp <- St %>% ungroup() %>% group_by(Participant) %>%
  summarize(
            gender = unique(gender),
            mean_rate = mean(rate),
            min_rate = min(rate),
            max_rate = max(rate),
            bci_experience = sum(bci_experience == "BCIExperience")
            )
#############
# Latex Table with demographic and summary information
#############

Sp <- Sp %>% mutate(bci_experience = ifelse(bci_experience > 0, "Yes", "No"),
                    bci_experience = ifelse(is.na(bci_experience), "No", bci_experience),
                    mean_rate = format(round(mean_rate,2), nsmall = 2),
                    min_rate = format(round(min_rate,2), nsmall = 2),
                    max_rate = format(round(max_rate,2), nsmall = 2),
                    across(everything(), as.character)) %>%
             rename(Gender = gender, `Mean Rate` = mean_rate, `Min. Rate` = min_rate, `Max. Rate` = max_rate, `BCI Experience` = bci_experience)

Sp_table = Sp %>% pivot_longer(cols=-Participant, names_to = "Feature") %>%
  pivot_wider(names_from = Participant, values_from = value)

paste(colnames(Sp_table), collapse=" & ")
Sp_table %>% apply(.,1,paste,collapse=" & ")


  group_by(Feature) %>% summarise(text = paste(fm_value, sep = " & ", collapse = " & "),
  text = paste(text, "\\ ")) %>% V


#############
# Perceived Control to Level of Control by Condition
#############

# Story 1: We can move people upwards, if we provide them fabricated input.
FabCurve100A <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "100A") %>% select(rate, PercNormalized)
FabCurve100A <- lm(PercNormalized ~ rate, data = FabCurve100A)
FabCurve100A <- data.frame(x = (1:100)/100, y = predict(FabCurve100A, data.frame(rate = (1:100)/100)))
FabCurve100B <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "100B") %>% select(rate, PercNormalized)
FabCurve100B <- lm(PercNormalized ~ rate, data = FabCurve100B)
FabCurve100B <- data.frame(x = (1:100)/100, y = predict(FabCurve100B, data.frame(rate = (1:100)/100)))
FabCurve50A <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "50A") %>% select(rate, PercNormalized)
FabCurve50A <- lm(PercNormalized ~ rate, data = FabCurve50A)
FabCurve50A <- data.frame(x = (1:50)/100, y = predict(FabCurve50A, data.frame(rate = (1:50)/100)))
FabCurve50B <- St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"),Condition == "50B") %>% select(rate, PercNormalized)
FabCurve50B <- lm(PercNormalized ~ rate, data = FabCurve50B)
FabCurve50B <- data.frame(x = (1:50)/100, y = predict(FabCurve50B, data.frame(rate = (1:50)/100)))

fig1 <- fig %>%
  add_trace(name= "100%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "100A"), x=~rate, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('circle'), marker=list(size=8)) %>%
  add_trace(name = "100% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "100B"), x=~rate, y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('o'), marker=list(size=8)) %>%
  add_trace(data=FabCurve100A, x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FabCurve100B, x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), ), xaxis = list(range=c(-0.05,1.1), title="BCI Control (No Limit)"))

fig2 <- fig %>% 
  add_trace(name = "50%", data = St %>% filter(GameTitle %in% c("HandStrength", "Kiwi"), Condition == "50A"), x=~jitter(rate, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I("rgba(0, 0, 0, 0.7)"), symbol=I('square'), marker=list(size=7)) %>%
  add_trace(name = "50% + 30% F", data = St %>% filter(GameTitle %in% c("HandStrength","Kiwi"), Condition == "50B"), x=~jitter(rate, amount=.02), y=~jitter(PercNormalized, amount=.02),
            type='scatter',mode='markers', color=I('black'), symbol=I('square-open'), marker=list(size=7)) %>%
  add_trace(data=FabCurve50A, x=~x, y=~y, color=I('black'), type='scatter', mode='line', showlegend=F) %>%
  add_trace(data=FabCurve50B, x=~x, y=~y, color=I('grey70'), type='scatter', mode='line', showlegend=F) %>%
  layout(yaxis = list(range=c(-0.05,1.1), title="", showticklabels=F), xaxis = list(range=c(-0.05,1.1), title="BCI Control (50% Limit)"))

fig_c <- subplot(fig1, fig2, titleY = TRUE, titleX = TRUE) %>% 
  layout(title = 'Conditions w/ Fab. Input to Control Conditions')

orca(fig_c, "fig/fabricated-input-to-control.pdf", width=950, height=350)

#############
# Perceived Control to Level of Control by GameTitle
#############



# # # OLDER CODE # # #

############ Pure Conditions vs Perceived Control ############

qfitPure <- lm(PercNormalized ~ rate, data = St %>% filter(Condition %in% c("50A", "100A")))
PercRateCurve.Pure <- data.frame(x = (0:100)/100, y = predict(qfitPure, data.frame(rate = (0:100)/100), se.fit=T))

summary(qfitPure)



fig %>%
  add_trace(data = trial_bee, x=~x, y=~y, color=I('darkgray'),
            type="scatter", mode='markers', marker=list(color='rgba(0.3,0.3,0.3,0.5)', line=list(color=I('black'),width=1))) %>%
  add_trace( data=PercRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines", color=I('black')) %>%
  layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Level of Control"),showlegend=F)

beeswarm::swarmx(x = St$rate, y=St$PercNormalized, xsize=1, ysize=1)

beeswarm::swarmx(x=length(iris$Sepal.Width), y=iris$Sepal.Width)

index <- rep(0, 100)
str(index)
values <- rnorm(100)
str(values)
plot(index,values)

str(iris$Sepal.Width)
str(iris$Sepal.Length)

beeswarm::swarmx(length(iris$Sepal.Width), y=iris$Sepal.Width, cex=0.9) %>% View()

beeswarm::beeswarm(x ~ y, data=data.frame(x=iris$Sepal.Width, y=iris$Sepal.Length), cex=0.7,
                   spacing = 0.1, priority='density')


############ Fab+Pure Conditions vs Perceived Control ############

qfitFab <- lm(PercNormalized ~ rate, data = St %>% filter(Condition %in% c("100B", "50B")))
PercRateCurve.Fab <- data.frame(x = (0:10)/10, y = predict(qfitFab, data.frame(rate = (0:10)/10), se.fit=T))

summary(qfitFab)

fig %>%
  add_trace(data = St %>% filter(Condition %in% c("100B", "50B")), x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03),
            type="scatter", mode='markers', color=I('orange')) %>%
  add_trace( data=PercRateCurve.Fab, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace( data=PercRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace(data = St %>% filter(Condition %in% c("100A", "50A")), x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03),
            type="scatter", mode='markers', color=I('SeaGreen')) %>%
  layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Pure Conditions vs Frustration ############

qfrustPure <- lm(FrustNormalized ~ rate, data = St %>% filter(Condition %in% c("100A", "50A")))
FrustRateCurve.Pure <- data.frame(x = (0:100)/100, y = predict(qfrustPure, data.frame(rate = (0:100)/100), se.fit=T))

summary(qfrustPure)

fig %>%
  add_trace(data = St %>% filter(Condition %in% c("100A", "50A")), x=~jitter(rate, amount=.03), y=~jitter(FrustNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~Participant) %>%
  add_trace( data=FrustRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Pure+Fab Conditions vs Frustration ############

qfrustFab <- lm(FrustNormalized ~ rate, data = St %>% filter(Condition %in% c("100B", "50B")))
FrustRateCurve.Fab <- data.frame(x = (0:10)/10, y = predict(qfrustFab, data.frame(rate = (0:10)/10), se.fit=T))

summary(qfrustFab)

fig %>%
  add_trace(data = St %>% filter(Condition %in% c("50B")), x=~jitter(rate, amount=.03), y=~jitter(FrustNormalized, amount=.03),
            type="scatter", mode='markers', color=I('orange')) %>%
  add_trace( data=FrustRateCurve.Fab, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace( data=FrustRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  add_trace(data = St %>% filter(Condition %in% c("50A")), x=~jitter(rate, amount=.03), y=~jitter(FrustNormalized, amount=.03),
            type="scatter", mode='markers', color=I('SeaGreen')) %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

############ Perceived Control vs Frustration ############

qfrustPerc <- lm(FrustNormalized ~ rate, data = St)
FrustPercCurve <- data.frame(x = (0:10)/10, y = predict(qfrustPerc, data.frame(rate = (0:10)/10), se.fit=T))
summary(qfrustPerc)

fig %>%
  add_trace(data = St %>% filter(Condition %in% c("50B","100B")), x=~jitter(PercNormalized, amount=.02), y=~jitter(FrustNormalized, amount=.02), color=I('orange'),
            type="scatter", mode='markers', color=~Participant) %>%
  add_trace(data = St %>% filter(Condition %in% c("50A","100A")), x=~jitter(PercNormalized, amount=.02), y=~jitter(FrustNormalized, amount=.02), color=I('seagreen'),
            type="scatter", mode='markers', color=~Participant) %>%
  add_trace( data=FrustPercCurve, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Perceived Control"),showlegend=F)


# We need to do an aggregate frustration thing as a follow-up to do, based on the order people rated. to improve
# the rating between frustration and the fabricated.

fig %>%
  add_trace(data = St %>% filter(Condition %in% c("50B")), x=~jitter(rate, amount=.02), y=~jitter(PercNormalized, amount=.02), color=I('skyblue'),
            type="scatter", mode='markers', color=~Participant) %>%
  add_trace(data = St %>% filter(Condition %in% c("100A")), x=~jitter(rate, amount=.02), y=~jitter(PercNormalized, amount=.02), color=I('seagreen'),
            type="scatter", mode='markers', color=~Participant) %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Perceived Control"),showlegend=F)


qfitAll <- lm(rate ~ PercNormalized, data = St)
PercRateCurve.All <- data.frame(x = (0:10)/10, y = predict(qfitAll, data.frame(PercNormalized = (0:10)/10), se.fit=T))

fig %>%
  add_trace(data = St, x=~jitter(rate, amount=.03), y=~jitter(PercNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~Participant) %>%
  #add_trace( data=PercRateCurve.All, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Rate"),showlegend=F)

FfitAll <- lm(rate ~ FrustNormalized, data = St)
FrustRateCurve.All <- data.frame(x = (0:10)/10, y = predict(FfitAll, data.frame(PercNormalized = (0:10)/10), se.fit=T))

fig %>%
  add_trace(data = St, x=~jitter(PercNormalized, amount=.03), y=~jitter(FrustNormalized, amount=.03), color=I('darkgray'),
            type="scatter", mode='markers', color=~Participant) %>%
  #add_trace( data=PercRateCurve.All, x=~x, y=~y.fit, type="scatter", mode="lines") %>%
  layout(yaxis = list(range=c(0,1), title="Frustration"), xaxis=list(range=c(0,1),title="Perceived Control"),showlegend=F)

timetemplate %>%
  add_trace(data = df_kiwi %>% filter(Participant == "1") %>% arrange(Framecount), x =~Framecount, y=~BCIConfidence, type="scattergl", mode="markers+lines")





#################################################
# Influence of 50A on people's level of control #
#################################################

fig %>%
  add_trace(data = St, x=~rate, y=~jitter(PercNormalized, amount=.03),
            type='scattergl',mode='markers', color=~Condition, size=2)


############ Horizontal Box Plot ############

fig %>%
  add_trace(data = St %>% filter(Condition %in% c("100A")), x=~rate, y=~as.factor(PerceivedControlEpisode), color=I('black'),
            type="box", boxpoints="all", jitter=.02, marker=list(color='rgba(0.3,0.3,0.3,0.5)', line=list(color=I('black'),width=1)))
  #add_trace( data=PercRateCurve.Pure, x=~x, y=~y.fit, type="scatter", mode="lines", color=I('black')) %>%
  #layout(yaxis = list(range=c(0,1), title="Perceived Control"), xaxis=list(range=c(0,1),title="Level of Control"),showlegend=F)