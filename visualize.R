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
      geom_hline(yintercept = 0, alpha = 0.5)
}


# Visualize trials' EEG data along with given N200, P300, & RT locations
visualize.trials = function(data, trials, features, N200 = F, P300 = F, RT = F, ...)
{
  # We plot the annotations from here since they exist in only one row...
  # If we pull the rectangles from "data", they will be drawn for each sample
  #   and overlay on each other, making transparency not work
  annotations = features %>%
    filter(Trial %in% trials)

  plot = data %>%
    filter(Trial %in% trials) %>%
    visualize.eeg(...) +
      facet_wrap( ~ Trial, ncol = floor(sqrt(length(trials))))
  
  if(N200)
    plot = plot +
      geom_vline(aes(xintercept = Time.ms.N200), col = "dark blue", alpha = 0.6, data = annotations, na.rm = T) +
      geom_rect(aes(x = NULL, y = NULL, xmin = Range.Left.N200, xmax = Range.Right.N200), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "light blue", data = annotations, na.rm = T)
      
  if(P300)
    plot = plot +
      geom_vline(aes(xintercept = Time.ms.P300), col = "dark green", alpha = 0.6, data = annotations, na.rm = T) +
      geom_rect(aes(x = NULL, y = NULL, xmin = Range.Left.P300, xmax = Range.Right.P300), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "light green", data = annotations, na.rm = T)
      
  # Must reset x,y = NULL for the rectangles...
  # https://stackoverflow.com/questions/25286629/different-geom-rect-objects-for-facets
  
  if(RT)
    plot = plot + 
      geom_vline(aes(xintercept = Reaction.Time.ms), col = "red", alpha = 0.6, data = annotations) +
      geom_label(aes(x = Reaction.Time.ms, label = "RT"), y = 0, size = 2.5, data = annotations)
  
  plot
}


# Shortcut to plot a histogram of a model feature
features.hist = function(features.df, feature, bin.width = 25)
{
  feature = enquo(feature) # dplyr.tidyverse.org/articles/programming.html#different-expressions
  
  ggplot(features.df, aes(!!feature)) + 
    geom_histogram(binwidth = bin.width, col = "dark grey", alpha = 0.3, na.rm = T) +
    facet_wrap( ~ Condition) +
    theme_minimal()
}
