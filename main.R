source("read_eeg_mat.R")
source("visualize.R")
source("analyze.R")

N200.CHANNELS = c(18, 16, 10, 4, 5, 6, 12, 19)
P300.CHANNELS = c(54, 79, 78, 77, 76, 75, 71, 67, 61)

###################

eeg.all.channels = read.eeg.mat("/share/data/cidlab/s112_ses1_sfinal.mat")

n200 = eeg %>% filter(Channel %in% N200.CHANNELS)
n200.no.fast.trials = n200 %>% filter(Reaction.Time >= 0.350)

for(cond in 1:6)
{
  pdf(paste("Channel", cond, ".pdf", sep = ''))
  
  (n200 %>%
    filter(Condition == cond) %>%
    visualize(upper.bound = 600) +
      ggtitle(paste("All Condition", cond, "Trials"), subtitle = "Correct and Incorrect Responses")) %>%
    print()
  
  (n200 %>%
    filter(Condition == cond) %>%
    top_n(40*4096*8, Trial) %>% # bottom (largest or "top") 40 recordings for all 4096 samples from all 8 N200 channels
    visualize(upper.bound = 600) +
      ggtitle(paste("Condition", cond, "Trials - Second Half"), subtitle = "Correct and Incorrect Responses")) %>%
    print()
  
  
  (n200.no.fast.trials %>%
    filter(Condition == cond) %>%
    visualize(upper.bound = 600) +
    ggtitle(paste("Reasonably Slow Condition", cond, "Trials"), subtitle = "Correct and Incorrect Responses")) %>%
    print()
  
  (n200.no.fast.trials %>%
    filter(Condition == cond) %>%
    top_n(40*4096*8, Trial) %>%
    visualize(upper.bound = 600) +
    ggtitle(paste("Reasonable Slow Condtion", cond, "Trials from Second Half Only"), subtitle = "Correct and Incorrect Responses")) %>%
    print()

  dev.off()  
}
  

