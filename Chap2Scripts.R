stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic=stat_fun)

library(boot)
library(ggplot2)
# take a simple random sample
samp_data <- data.frame(income=sample(loans_income, 1000, replace = TRUE), type='data_dist')
#take a sample of means of 5 values
samp_mean_05 <- data.frame(income = tapply(sample(loans_income, 1000*5, replace = TRUE),rep(1:1000, rep(5, 1000)), FUN = mean), type = 'mean_of_5')
# take a sample o f means of 20 values
samp_mean_20 <- data.frame(income - tapply(sample(loans_income, 1000*20, replace = TRUE),rep(1:1000, rep(20,1000)), FUN = mean), type = 'mean_of_20')
# bind the data.frames and convert type to a factor
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type, levels = c('data_dist', 'mean_of_5', 'mean_of_20'), lables=c('Data', 'Mean of 5', 'Mean of 20'))

#plot the histograms
ggplot(income, aes(x=income)) + 
  geom_histogram(bins=40) +
  facet_grid(type ~ .)


