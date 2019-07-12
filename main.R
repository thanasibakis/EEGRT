source("read_eeg_mat.R")
source("visualize.R")
source("analyze.R")

###################

## First, run read_eeg_mat.R
## For some reason, cluster computing is not working when
##   the code is in a function scope
## So, we're keeping it as a script for now.

eeg.data = read.eeg.mat("/share/data/cidlab/s112_ses1_sfinal.mat")
eeg.info = eeg.data$Info

## We now have eeg.data, which contains the sample values
##   for each trial and channel combination
## We also have eeg.info, which contains the reaction times
##   and conditions of each trial
## You can left join these by trial when you need reaction times
##   and samples in the same data frame
## We also have channel.weights, which specifies the weights to
##   use when averaging samples over all channels (instead of
##   using even weights on only some channels)
  
## Calculate features

mean.RT = mean(eeg.info$Reaction.Time.ms, na.rm = T)

features = generate.features(eeg.data) %>%
  left_join(eeg.info, by = "Trial") %>% # To add the reaction times to the data frame
  mutate(RT.Above.Avg = Reaction.Time.ms > mean.RT,
         RT.Percentile = ntile(Reaction.Time.ms, 10)) # Simpler response vars to predict

## Old N200 calculations, to compare to new ones

features.csv = read.csv("s112ses1_trialdataNAN.csv") %>%
  mutate(Reaction.Time.ms = RT * 1000) %>%
  mutate(N200 = N200latencies * 1000) %>%
  mutate(RT.Above.Avg = Reaction.Time.ms > mean.RT)

## Draw some trials

eeg.data$N200.Data %>%
  visualize.trials(features, 1:20)

eeg.data$P300.Data %>%
  visualize.trials(features, 20:40)

## Modeling

clean.trials = features %>%
  filter(Reaction.Time.ms >= 350)

# Histograms
clean.trials %>% features.hist(N200)
clean.trials %>% features.hist(P300)
clean.trials %>% features.hist(Reaction.Time.ms)

# Boxplot of N200 separated by RT.Above.Avg
ggplot(clean.trials, aes(RT.Above.Avg, N200)) + 
  geom_boxplot() + 
  xlab("Reaction Time Above Average?")

# Boxplot of N200 separated by RT.Percentile
# Note that early RT groups won't appear because they are too quick
ggplot(clean.trials, aes(factor(RT.Percentile), N200)) +
  geom_boxplot() +
  xlab("Reaction Time Percentile")

# Boxplot of P300 separated by RT.Above.Avg
ggplot(clean.trials, aes(RT.Above.Avg, P300)) + 
  geom_boxplot() + 
  xlab("Reaction Time Above Average?")

# Boxplot of P300 separated by RT.Percentile
ggplot(clean.trials, aes(factor(RT.Percentile), P300)) +
  geom_boxplot() +
  xlab("Reaction Time Percentile")

# Modeling RT.Above.Avg
glm(RT.Above.Avg ~ N200, family = "binomial",
    data = clean.trials) %>%
  summary()

# Modeling RT
lm(RT.Percentile ~ N200 + P300, data = clean.trials) %>%
  summary()
