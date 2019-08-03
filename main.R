source("read.R")
source("visualize.R")
source("analyze.R")


###########################################################
# If you're in a rush, just run this chunk for the model! #
###########################################################

## Retrieve subjects' data (this could take a while the first time)

eeg = read.sessions("/share/data/michael/exp7data/subjects/s112/ses1/s112_ses1_sfinal.mat",
                    "/share/data/michael/exp7data/subjects/s112/ses2/s112_ses2_sfinal.mat",
                    "/share/data/michael/exp7data/subjects/s116/ses1/s116_ses1_sfinal.mat",
                    "/share/data/michael/exp7data/subjects/s143/ses1/s143_ses1_sfinal.mat")


## Calculate features (this will take a smaller while)

features = generate.features(eeg) %>%
  filter(Reaction.Time.ms >= 350)


## Modeling reaction time vs the peaks
lm(Reaction.Time.ms ~ Time.ms.N200 + Time.ms.P300, data = features) %>% # the (P300 - N200) eliminates the 
  summary()

###########################################################
# Beyond here is further analysis of the data and model!  #
###########################################################


## Draw some trials to see where the N200s & P300s were placed

some.trials = features %>%
  pull(Trial) %>%
  head(20)

eeg$N200.Data %>%
  visualize.trials(some.trials, features, N200 = T, RT = T)

eeg$P300.Data %>%
  visualize.trials(some.trials, features, P300 = T, RT = T)


## Histograms to assess what the features and response look like

features.hist(features, Time.ms.N200)
qqnorm(features$Time.ms.N200); qqline(features$Time.ms.N200)

features.hist(features, Time.ms.P300)
qqnorm(features$Time.ms.P300); qqline(features$Time.ms.P300)

features.hist(features, Reaction.Time.ms)
qqnorm(features$Reaction.Time.ms); qqline(features$Reaction.Time.ms)


## Plot of reaction time vs N200

ggplot(features, aes(Reaction.Time.ms, Time.ms.N200)) +
  geom_point(na.rm = T) +
  geom_smooth(method = "lm") +
  xlab("Reaction Time (ms)")


## Plot of reaction time vs P300

ggplot(features, aes(Reaction.Time.ms, Time.ms.P300)) +
  geom_point(na.rm = T) +
  geom_smooth(method = "lm") +
  xlab("Reaction Time (ms)")


## Modeling reaction time vs the peaks individually
## (since there are more trials where *at least one* exists, vs. *both* exist)

lm(Reaction.Time.ms ~ Time.ms.N200, data = features) %>%
  summary()

lm(Reaction.Time.ms ~ Time.ms.P300, data = features) %>%
  summary()


## Checking for a correlation between N200 and P300

ggplot(features, aes(Time.ms.N200, Time.ms.P300)) +
  geom_point(na.rm = T) +
  geom_smooth(method = "lm")

lm(Time.ms.N200 ~ Time.ms.P300, data = features) %>%
  summary()
