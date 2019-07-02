library(R.matlab)
library(parallel)
library(magrittr)

read.eeg.mat = function(path.to.mat)
{
  file.name = strsplit(path.to.mat, "/")[[1]] %>% tail(n = 1) %>% sub(".mat", '', .)
  
  ## First, check if we already converted this matlab file to our .RData format
  
  rdata.file.name = paste(file.name, ".RData", sep = '')
  
  if(file.exists(rdata.file.name))
  {
    print("Formatted RData exists, reading it directly...")
    
    load(rdata.file.name)
    return(list(Data = eeg.data, Info = eeg.info))
  }
  
  ## Convert the .mat to .rda, but maintaining the old structure that we need to format
  ## This takes a long time, so check if we already did this step earlier, too
  
  readMat.output.file.name = paste("readMat_output-", file.name, ".rds", sep = '') 
  
  if(file.exists(readMat.output.file.name))
  {
    print("Unformatted RDS exists, reading it directly...")
    eeg.matlab = readRDS(readMat.output.file.name)
    
  } else
  {
    print("No RDS exists, converting MAT file to RDS...")
    eeg.matlab = readMat(path.to.mat)
    
    print("Saving the unformatted RDS for future use...")
    saveRDS(eeg.matlab, readMat.output.file.name)
  }
  
  ## Begin formatting by loading the raw EEG data for subject 112
  
  eeg.data = eeg.matlab$data
  
  sample.rate = eeg.matlab$sr[1,1] * 1000 # in samples/ms
  num.samples = dim(eeg.data)[1]
  num.channels = dim(eeg.data)[2]
  num.trials = dim(eeg.data)[3]
  sample.times = 1:num.samples / sample.rate
  
  ## Load the 480 trial reaction times for subject 112
  
  reaction.times.ms = eeg.matlab$expinfo[[38]] / sample.rate
  
  ## Load the condition identifiers for the 480 trials
  
  conditions = eeg.matlab$expinfo[[36]]
  
  ## Load the correctness of the subject's 480 responses
  
  correct = eeg.matlab$expinfo[[37]]
  
  ## Form a data frame of all samples for a given channel and trial
  
  create.samples.df = function(channel, trial)
    data.frame(Channel = channel,
               Trial = trial,
               Sample.Val = eeg.data[, channel, trial ],
               Time.ms = sample.times)
  
  ## Form a data frame of the experiment results for a given trial
  ## We want to keep this separate from the above data frame, because this
  ##   info is redundant across all samples and all channels for the trial
  
  create.trial.info.df = function(trial)
    data.frame(Trial = trial,
               Reaction.Time.ms = reaction.times.ms[trial],
               Condition = conditions[trial],
               Correct = correct[trial])
  
  
  ## Create a cluster for faster function application
  
  print("Creating clusters...")
  cluster = makeCluster(4)
  clusterExport(cluster, c("eeg.data", "sample.times"),
                envir = environment()) # Important! Defaults to global env
  
  ## Get every combination of channel and trial
  
  channel.trial.combinations = as.list(expand.grid(1:num.channels, 1:num.trials)) # kind of like a cartesian product
  channels.list = channel.trial.combinations$Var1 # unpacking the tuples
  trials.list = channel.trial.combinations$Var2 
  
  ## Call create.samples.df for every combination of channel and trial
  
  print("Formatting the EEG samples data...")
  samples.df.list = clusterMap(cluster, create.samples.df, channels.list, trials.list)
  
  ## Call create.trial.info.df for every trial number
  
  print("Formatting the reaction times data...")
  trial.info.df.list = parLapply(cluster, 1:num.trials, create.trial.info)
  
  stopCluster(cluster)
  
  ## Merge the data frames from the apply functions
  
  print("Merging the results of the clusters...")
  eeg.data = do.call(rbind, samples.df.list)
  eeg.info = do.call(rbind, trial.info.df.list)
  
  ## Export the data frame so we don't have to do all this again next time
  
  print("Saving the formatted data for future use...")
  saveRDS(eeg, rdata.file.name)
  
  list(Data = eeg.data, Info = eeg.info)
}



