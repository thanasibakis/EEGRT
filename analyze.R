library(dplyr)

DERIVATIVE.RATIO = 3

N200.BOUNDS = c(left = 125, right = 350) # changed from 275)
P300.BOUNDS = c(left = 275, right = 800)

## Various potential features of a model for reaction time
## All features will average across all channels given
generate.features = function(eeg.data)
{
  # Start with a data frame of just trial numbers, and add on later
  N200.peaks = eeg.data$N200.Data %>%
    inner_join(eeg.data$Info, by = "Trial") %>% # gain access to reaction times
    group_by(Trial) %>%
    group_modify( ~ peak.location(.x, target.time = 200, positive = F, time.bounds = N200.BOUNDS, should.be.symmetric = T))
  
  P300.peaks = eeg.data$P300.Data %>%
    inner_join(eeg.data$Info, by = "Trial") %>%
    group_by(Trial) %>%
    group_modify( ~ peak.location(.x, target.time = 300, positive = T, time.bounds = P300.BOUNDS, should.be.symmetric = F))
  
  # Must full join the two peaks, because one can occur without the other
  full_join(N200.peaks, P300.peaks, by = c("Trial", "Condition", "Correct", "Reaction.Time.ms"), suffix = c(".N200", ".P300")) # must specify all "by", otherwise the suffix gets attached
}

# Returns the estimated time of occurance and amplitude for a peak near target.time but definitely within time.bounds
peak.location = function(eeg.trial.data, target.time, positive, time.bounds, should.be.symmetric)
{
  # Pick the peak with the largest amplitude
  peak = eeg.trial.data %>%
    filter(Time.ms >= time.bounds[1] & Time.ms <= time.bounds[2] & Time.ms <= eeg.trial.data$Reaction.Time.ms[1]) %>%
    top_n(1, Sample.Val * ifelse(positive, 1, -1))
  
  if(dim(peak)[1] == 0)
    return(peak) # no data, no peak
  
  # Calculate inflection points around the peak
  peak.range = peak.inflection.points(eeg.trial.data, peak$Time.ms)
  peak$Range.Left = peak.range["left"]
  peak$Range.Right = peak.range["right"]
  
  # Calculate derivatives around the peak
  peak.derivatives = peak.side.derivatives(eeg.trial.data, peak$Time.ms, peak.range)
  peak$Derivative.Left = peak.derivatives["left"]
  peak$Derivative.Right = peak.derivatives["right"]
  
  # Is this a "peak" just because the maximum value was a boundary value?
  is.boundary.peak = (peak$Time.ms <= time.bounds["left"] + 10) || (peak$Time.ms >= time.bounds["right"] - 10)
  
  # Is this peak too narrow/wide?
  peak.width.bad = (diff(peak.range) < 20) || (diff(peak.range) > 150)
  
  # Is this peak too lopsided, if it should be symmetric?
  derivative.ratio = abs(peak.derivatives["left"] / peak.derivatives["right"])
  is.lopsided = (derivative.ratio > DERIVATIVE.RATIO) || (derivative.ratio < 1 / DERIVATIVE.RATIO)
  
  # Return no peak if those conditions were met
  if(is.boundary.peak || peak.width.bad || should.be.symmetric && is.lopsided)
    return(head(peak, 0))
  else
    return(peak)
  
  # TODO: instead of saying there's no peak if the largest peak failed, try considering smaller peaks that may fit the bill
}

peak.inflection.points = function(eeg.trial.data, peak.time)
{
  derivatives = eeg.trial.data %>%
    mutate(dy = diff(eeg.trial.data$Sample.Val) %>% c(0, .)) %>%
    mutate(d2y = diff(dy) %>% c(0, .)) %>% # the derivative of the first point is 0, but we need to maintain the same column length
    filter(abs(d2y) < .005) %>%
    mutate(Is.Left.Side = Time.ms <= peak.time)
  
  c(left  = derivatives %>% filter(Is.Left.Side)  %>% pull(Time.ms) %>% max(),
    right = derivatives %>% filter(!Is.Left.Side) %>% pull(Time.ms) %>% min())
}

peak.side.derivatives = function(eeg.trial.data, peak.time, peak.range)
{
  derivatives = eeg.trial.data %>%
    filter(peak.range["left"] <= Time.ms & Time.ms <= peak.range["right"]) %>%
    mutate(Is.Left.Side = Time.ms <= peak.time) %>%
    group_by(Is.Left.Side) %>%
    summarise(Derivative = mean(diff(Sample.Val)))
    
  c(left  = derivatives %>% filter(Is.Left.Side)  %>% pull(Derivative), 
    right = derivatives %>% filter(!Is.Left.Side) %>% pull(Derivative))
}

