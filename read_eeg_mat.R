library(R.matlab)
library(parallel)
library(magrittr)

read.eeg.mat = function(path.to.mat)
{
  ## First, check if we already converted this matlab file to our .rda format
  
  rda.file.name = strsplit(path.to.mat, "/")[[1]] %>% tail(n = 1) %>% sub(".mat", ".rda", .)
  
  if(file.exists(rda.file.name))
  {
    print("Formatted RDA exists, reading it directly...")
    
    return(readRDS(rda.file.name))
  }
  
  ## Convert the .mat to .rda, but maintaining the old structure that we need to format
  ## This takes a long time, so check if we already did this step earlier, too
  
  readMat.output.file.name = paste("readMat_output", rda.file.name, sep = '-')
  
  if(file.exists(readMat.output.file.name))
  {
    print("Unformatted RDA exists, reading it directly...")
    eeg.matlab = readRDS(readMat.output.file.name)
    
  } else
  {
    print("No RDA exists, converting MAT file to RDA...")
    eeg.matlab = readMat(path.to.mat)
    
    print("Saving the unformatted RDA for future use...")
    saveRDS(eeg.matlab, readMat.output.file.name)
  }
  
  ## Begin formatting by loading the raw EEG data for subject 112
  
  eeg.data = eeg.matlab$data
  
  sample.rate = eeg.matlab$sr[1,1]
  num.samples = dim(eeg.data)[1]
  num.channels = dim(eeg.data)[2]
  num.trials = dim(eeg.data)[3]
  
  ## Load the 480 trial reaction times for subject 112
  
  rt = eeg.matlab$expinfo[[38]] / sample.rate
  
  ## Load the condition identifiers for the 480 trials
  
  conditions = eeg.matlab$expinfo[[36]]
  
  ## Load the correctness of the subject's 480 responses
  
  correct = eeg.matlab$expinfo[[37]]
  
  ## Get every combination of channel and trial
  
  channel.trial.combinations = as.list(expand.grid(1:num.channels, 1:num.trials)) # kind of like a cartesian product
  channels.list = channel.trial.combinations$Var1
  trials.list = channel.trial.combinations$Var2 # "zipping" these two lists gives you the combinations
  
  
  print("Creating clusters...")
  cluster = makeCluster(4)
  
  ## Form a data frame of all samples for a given channel and trial
  
  create.samples.df = function(channel, trial)
    data.frame(Channel = channel, Trial = trial, Sample.Num = 1:num.samples,
               Sample.Val = eeg.data[, channel, trial ], Time.ms = 1:num.samples / sample.rate * 1000,
               Reaction.Time = rt[trial], Condition = conditions[trial], Correct = correct[trial])
  
  ## Make all necessary variables accessible to the cluster
  
  print("Exporting data to clusters...")
  clusterExport(cluster, c("num.samples", "eeg.data", "rt", "conditions", "correct", "sample.rate")) 
  
  ## Call create.samples.df for every combination of channel and trial
  
  print("Formatting the data...")
  df.list = clusterMap(cluster, create.samples.df, channels.list, trials.list)
  
  stopCluster(cluster)
  
  ## Merge the data frames for each cluster/trial pair into one large one
  
  print("Merging the results of the clusters...")
  eeg = do.call(rbind, df.list)
  
  ## Export the data frame so we don't have to do all this again next time
  
  print("Saving the formatted data for future use...")
  saveRDS(eeg, rda.file.name)
  
  eeg
}



