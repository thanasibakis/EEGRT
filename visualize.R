library(ggplot2)
library(dplyr)

## Plot the EEG voltages over time, bounding the time axis if desired
## (If multiple trials are given, you should add a facet after)
## (If multiple channels are given, a band will show the variation among them)
visualize.eeg = function(data, lower.bound = 125, upper.bound = 800)
{
  data %>%
    filter(Time.ms >= lower.bound & Time.ms <= upper.bound) %>%
    ggplot(aes(Time.ms, Sample.Val)) +
      geom_line() +
      labs(x = "Time (ms)", y = "EEG Measure") +
      theme_minimal() + 
      geom_hline(yintercept = 0, alpha = 0.5) +
      annotate("rect", xmin = 125, xmax = 275, ymin = -Inf, ymax = Inf,
               alpha = 0.3, fill = "light blue") +
      annotate("rect", xmin = 275, xmax = 800, ymin = -Inf, ymax = Inf,
               alpha = 0.3, fill = "light green")
}

# Visualize trials' EEG data along with given N200 & P300 locations
visualize.trials = function(data, features, trials)
{
  data %>%
    filter(Trial %in% trials) %>%
    left_join(features, by = "Trial") %>%
    visualize.eeg() +
      geom_vline(aes(xintercept = N200), col = "dark blue", alpha = 0.6) +
      geom_vline(aes(xintercept = P300), col = "dark green", alpha = 0.6) +
      facet_wrap( ~ Trial, ncol = floor(sqrt(length(trials))))
}

features.hist = function(features.df, feature, bin.width = 25)
{
  feature = enquo(feature) # dplyr.tidyverse.org/articles/programming.html#different-expressions
  
  ggplot(features.df, aes(!!feature)) + 
    geom_histogram(binwidth = bin.width, col = "dark grey", alpha = 0.3) +
    facet_wrap( ~ Condition) +
    theme_minimal()
}

## Applies a filter to smooth the voltages
smooth.data = function(dat)
{
  len = length(dat)
  smoothed = rep(0, len)
  
  for(i in 1:len)
  {
    i.to.avg = (i + c(-4, 4)) %>% subset(. >= 1 & . <= len)
    smoothed[i] = dat[i.to.avg] %>% mean()
  }
  
  smoothed
}
  
  
