source("read_eeg_mat.R")
source("visualize.R")
source("analyze.R")

###################

## Retrieve subjects' data

s112.ses1 = read.eeg.mat("/share/data/michael/exp7data/subjects/s112/ses1/s112_ses1_sfinal.mat")
s112.ses2 = read.eeg.mat("/share/data/michael/exp7data/subjects/s112/ses2/s112_ses2_sfinal.mat")
s116.ses1 = read.eeg.mat("/share/data/michael/exp7data/subjects/s116/ses1/s116_ses1_sfinal.mat")
s143.ses1 = read.eeg.mat("/share/data/michael/exp7data/subjects/s143/ses1/s143_ses1_sfinal.mat")

eeg.data = merge.sessions(s112.ses1, s112.ses2) # we require unique trial numbers!
  
## Calculate features

features = generate.features(eeg.data) %>%
  left_join(eeg.data$Info, by = "Trial") %>% # To add the reaction times to the data frame
  mutate(RT.Above.Avg = Reaction.Time.ms > mean(Reaction.Time.ms),  # Simpler response vars to predict
         RT.Percentile = ntile(Reaction.Time.ms, 10)) %>%
  filter(Reaction.Time.ms >= 350)#

## Old N200 calculations, to compare to new ones

features.csv = read.csv("s112ses1_trialdataNAN.csv") %>%
  mutate(Reaction.Time.ms = RT * 1000) %>%
  mutate(N200 = N200latencies * 1000) %>%
  mutate(RT.Above.Avg = Reaction.Time.ms > mean.RT)

## Draw some trials

eeg.data$N200.Data %>%
  visualize.trials(features, 1:20)

eeg.data$P300.Data %>%
  visualize.trials(features, 1:20)

## Modeling

# Histograms
features.hist(features, N200)
qqnorm(features$N200); qqline(features$N200)

features.hist(features, P300)
qqnorm(features$P300); qqline(features$P300)

features.hist(features, Reaction.Time.ms)
qqnorm(features$Reaction.Time.ms); qqline(features$Reaction.Time.ms)


# Boxplot of N200 separated by RT.Above.Avg
ggplot(features, aes(RT.Above.Avg, N200)) + 
  geom_boxplot() + 
  xlab("Reaction Time Above Average?")

# Boxplot of P300 separated by RT.Above.Avg
ggplot(features, aes(RT.Above.Avg, P300)) + 
  geom_boxplot() + 
  xlab("Reaction Time Above Average?")


# Boxplot of N200 separated by RT.Percentile
# Note that early RT groups won't appear because they are too quick
ggplot(features, aes(factor(RT.Percentile), N200)) +
  geom_boxplot() +
  xlab("Reaction Time Percentile")

# Boxplot of P300 separated by RT.Percentile
ggplot(features, aes(factor(RT.Percentile), P300)) +
  geom_boxplot() +
  xlab("Reaction Time Percentile")


# Plot of reaction time vs N200
ggplot(features, aes(Reaction.Time.ms, N200)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Reaction Time (ms)")

# Plot of reaction time vs P300
ggplot(features, aes(Reaction.Time.ms, P300)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Reaction Time (ms)")


# Modeling RT.Above.Avg
glm(RT.Above.Avg ~ N200 + P300, family = "binomial",
    data = features) %>%
  summary()

# Modeling RT.Percentile
lm(RT.Percentile ~ N200 + P300, data = features) %>%
  summary()

# Modeling reaction time
lm(Reaction.Time.ms ~ N200 + P300, data = features) %>%
  summary()
