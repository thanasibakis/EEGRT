library(ggplot2)
library(dplyr)

## Plot the EEG voltages over time, bounding the time axis if desired
## (If multiple trials are given, they will be averaged together)
visualize = function(eeg, lower.bound = 0, upper.bound = 5000, smoothed = T)
{
  avg.eeg = eeg %>%
    filter(Time.ms >= lower.bound & Time.ms <= upper.bound) %>%
    average.trials() # if only one trial is given, this does nothing
  
  if(smoothed)
    avg.eeg$Sample.Val = smooth.data(avg.eeg$Sample.Val)
  
  ggplot(avg.eeg, aes(Time.ms, Sample.Val)) +
    geom_line() +
    labs(x = "Time (ms)", y = "EEG Measure") +
    theme_minimal() + 
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_vline(xintercept = 300, alpha = 0.7, col = "light blue") +
    geom_vline(aes(xintercept = Reaction.Time), alpha = 0.5, col = "red") +
    annotate("rect", xmin = 175, xmax = 250, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "light blue")
}

## Applies a filter to smooth the voltages
smooth.data = function(dat)
{
  len = length(dat)
  smoothed = rep(0, len)
  
  for(i in 1:len)
  {
    i.to.avg = (i + c(-1, 1)) %>% subset(. >= 1 & . <= len)
    smoothed[i] = dat[i.to.avg] %>% mean()
  }
  
  smoothed
}
  
average.trials = function(eeg)
{
  eeg %>%
    filter(!is.nan(Reaction.Time)) %>% # since one NAN will make the mean RT also NAN
    group_by(Time.ms) %>%
    summarise(Sample.Val = mean(Sample.Val),
              Reaction.Time = mean(Reaction.Time),
              Trial = "Averaged") # Keep some value of "Trial" so it can still be referenced in other queries
}
  
  
