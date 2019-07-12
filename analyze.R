library(dplyr)

DEPTH_WEIGHT = 0.95
CLOSENESS_WEIGHT = 0.05

## Various potential features of a model for reaction time
## All features will average across all channels given
generate.features = function(eeg.data)
{
  N200.locs = eeg.data$N200.Data %>%
    filter(!is.nan(Sample.Val)) %>%
    group_by(Trial) %>%
    group_map(peak.location, target.time = 200, positive = F, time.bounds = c(125, 275)) %>% # summarise doesn't seem to like when summary functions take entire dfs as input
    unlist()
  
  P300.locs = eeg.data$P300.Data %>%
    filter(!is.nan(Sample.Val)) %>%
    group_by(Trial) %>%
    group_map(peak.location, target.time = 300, positive = T, time.bounds = c(275, 800)) %>%
    unlist()
  
  trials.used = eeg.data$N200.Data %>%
    filter(!is.nan(Sample.Val)) %>%
    pull(Trial) %>%
    unique()
  
  data.frame(Trial = trials.used,
             N200 = N200.locs,
             P300 = P300.locs)
}

# Returns the estimated time of occurance for a peak near target.time but definitely within time.bounds
peak.location = function(eeg.trial.data, target.time, positive, time.bounds, ...) # group_map wants ...
{
  region = eeg.trial.data %>%
    filter(Time.ms >= time.bounds[1] & Time.ms <= time.bounds[2])
  
  max.amplitude = ifelse(positive, max(eeg.trial.data$Sample.Val), min(eeg.trial.data$Sample.Val))
    
  location = region %>%
    mutate(Score = peak.score(Time.ms, Sample.Val, target.time, time.bounds, max.amplitude)) %>%
    top_n(1, Score) %>%
    pull(Time.ms)
  
  # We should do more processing here to ensure it's actually a peak
  left.cutoff = time.bounds[1] + 10
  right.cutoff = time.bounds[2] - 10
  ifelse(location <= left.cutoff | location >= right.cutoff, NaN, location)
}

# Returns a score (0, 1) rating how "good" of a peak the given sample time/value is
peak.score = function(sample.time, sample.val, target.time, time.bounds, max.amplitude)
{
  closeness = 1 - abs(sample.time - target.time) / abs(max(time.bounds) - target.time)
  depth = sample.val / max.amplitude
  
  depth * DEPTH_WEIGHT + closeness * CLOSENESS_WEIGHT
}
