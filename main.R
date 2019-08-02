source("read_eeg_mat.R")
source("visualize.R")
source("analyze.R")

SHOW.VISUALS = F

###################

## Retrieve subjects' data

s112.ses1 = read.eeg.mat("/share/data/michael/exp7data/subjects/s112/ses1/s112_ses1_sfinal.mat")
s112.ses2 = read.eeg.mat("/share/data/michael/exp7data/subjects/s112/ses2/s112_ses2_sfinal.mat")
s116.ses1 = read.eeg.mat("/share/data/michael/exp7data/subjects/s116/ses1/s116_ses1_sfinal.mat")
s143.ses1 = read.eeg.mat("/share/data/michael/exp7data/subjects/s143/ses1/s143_ses1_sfinal.mat")

eeg = merge.sessions(s112.ses1, s112.ses2, s116.ses1, s143.ses1) # we require unique trial numbers!

# Calculate features

features = generate.features(eeg) %>%
  mutate(RT.Above.Avg = Reaction.Time.ms > mean(Reaction.Time.ms),  # Simpler response vars to predict
         RT.Percentile = ntile(Reaction.Time.ms, 10)) %>%
  filter(Reaction.Time.ms >= 350)

## Draw some trials

if(SHOW.VISUALS)
{
  some.trials = features %>%
    pull(Trial) %>%
    head(20)
  
  eeg$N200.Data %>%
    visualize.trials(some.trials, features, N200 = T, RT = T)
  
  eeg$P300.Data %>%
    visualize.trials(some.trials, features, P300 = T, RT = T)
}

## Modelling

if(SHOW.VISUALS)
{
  # Histograms
  features.hist(features, Time.ms.N200)
  qqnorm(features$Time.ms.N200); qqline(features$Time.ms.N200)

  features.hist(features, Time.ms.P300)
  qqnorm(features$Time.ms.P300); qqline(features$Time.ms.P300)

  features.hist(features, Reaction.Time.ms)
  qqnorm(features$Reaction.Time.ms); qqline(features$Reaction.Time.ms)


  # Boxplot of N200 separated by RT.Above.Avg
  ggplot(features, aes(RT.Above.Avg, Time.ms.N200)) + 
    geom_boxplot(na.rm = T) + 
    xlab("Reaction Time Above Average?")

  # Boxplot of P300 separated by RT.Above.Avg
  ggplot(features, aes(RT.Above.Avg, Time.ms.P300)) + 
    geom_boxplot(na.rm = T) + 
    xlab("Reaction Time Above Average?")

  # Boxplot of N200 separated by RT.Percentile
  # Note that early RT groups won't appear because they are too quick
  ggplot(features, aes(factor(RT.Percentile), Time.ms.N200)) +
    geom_boxplot(na.rm = T) +
    xlab("Reaction Time Percentile")

  # Boxplot of P300 separated by RT.Percentile
  ggplot(features, aes(factor(RT.Percentile), Time.ms.P300)) +
    geom_boxplot(na.rm = T) +
    xlab("Reaction Time Percentile")


  # Plot of reaction time vs N200
  ggplot(features, aes(Reaction.Time.ms, Time.ms.N200)) +
    geom_point(na.rm = T) +
    geom_smooth(method = "lm") +
    xlab("Reaction Time (ms)")

  # Plot of reaction time vs P300
  ggplot(features, aes(Reaction.Time.ms, Time.ms.P300)) +
    geom_point(na.rm = T) +
    geom_smooth(method = "lm") +
    xlab("Reaction Time (ms)")
}

# Modeling RT.Above.Avg
glm(RT.Above.Avg ~ Time.ms.N200, family = "binomial", data = features) %>%
  summary()

glm(RT.Above.Avg ~ Time.ms.P300, family = "binomial", data = features) %>% # Does the P300 only occur when the N200 occurs?
  summary()

# Modeling RT.Percentile
lm(RT.Percentile ~ Time.ms.N200, data = features) %>%
  summary()

lm(RT.Percentile ~ Time.ms.P300, data = features) %>%
  summary()

# Modeling reaction time
lm(Reaction.Time.ms ~ Time.ms.N200, data = features) %>%
  summary()

lm(Reaction.Time.ms ~ Time.ms.P300, data = features) %>%
  summary()

lm(Reaction.Time.ms ~ Time.ms.N200 + Time.ms.P300, data = features) %>%
  summary()

# Checking for a correlation between N200 and P300

if(SHOW.VISUALS)
{
  ggplot(features, aes(Time.ms.N200, Time.ms.P300)) +
    geom_point(na.rm = T) +
    geom_smooth(method = "lm")
}

lm(Time.ms.N200 ~ Time.ms.P300, data = features) %>%
  summary()
