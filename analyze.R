library(dplyr)

## Various potential features of a model for reaction time
## All features will average across all channels given
generate.features.df = function(eeg)
{
  eeg %>%
    group_by(Trial) %>%
    summarise(Mean.Voltage = mean(Sample.Val), # mean of all voltage samples over time
              Variance.Voltage = var(Sample.Val),
              N200 = N200.location(.))
}

## Returns a possible N200 occurence time for a single given trial
N200.location = function(eeg.trial)
{
  eeg.trial %>%
    subset(Time.ms >= 175 & Time.ms <= 250) %>%
    top_n(1, Sample.Val) %>%
    pull(Time.ms)
  
}