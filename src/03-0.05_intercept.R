### load library ----
library(tidyverse)
library(plotrix)
library(fixest)
library(broom)

### set params ----
pset <- 0.05 # alpha level set as 0.05
### read in data----
#Here we used mixed effect model estimates coefficient, and assume a simple OLS without correction for p
data <- read_csv("data/overview_herbicide_biodiv_effects.csv")
data %<>% mutate(Treatment = factor(Treatment, levels = c("CONTROL", "LIGHT", "MODERATE", "INTENSIVE"))) %>% filter(taxon %in% c("woodies", "flowers")) %>% select(pred_sr, lower_sr) %>% mutate(sd_sr = ((pred_sr-lower_sr)/1.96))

Distri_params <- data.frame(Treatment = factor(c("CONTROL", "LIGHT", "MODERATE", "INTENSIVE"), levels = c("CONTROL", "LIGHT", "MODERATE", "INTENSIVE")), woody = data[1:4, "pred_sr"], woody_sd = data[1:4, "sd_sr"], flowr = data[5:8, "pred_sr"], flowr_sd = data[5:8,"sd_sr"])

colnames(Distri_params)[2:5] <- c("woody","woody_sd","flowr","flowr_sd")

sample_size <- 7 # seven blocks


### calculate power----
post_power <- data.frame(Group = c("CL", "CM", "CI"), woody_power = NA, flowr_power = NA)

# function to test power, iteration 999 times----
# copy code from manual
power_test <- function(effect, sd, sample_size){
  sig_results <- c()
  
  for (i in 1:999) {
    # Have to re-create the data EVERY TIME or it will just be the same data over and over
    tib <- tibble(
      X = rep(c(0,1),sample_size)
    ) %>%
      mutate(Y = effect*X + rnorm(sample_size*2, mean = 0, sd = sd))
    
    # Run the analysis
    model <- feols(Y ~ X, data = tib, se = 'hetero')
    
    # Get the results
    sig_results[i] <- tidy(model)$p.value[2] <= pset
  }
  
  sig_results %>%
    mean() %>%
    return()
}

# calculate power for three groups, two taxon----
for(i in 1:3){
  # get effect size for woody in light, moderate, intensive groups
  w_eff <- Distri_params$woody[i+1]-Distri_params$woody[1]
  # get sd for woody in light, moderate, intensive groups
  w_sd <- max(Distri_params$woody_sd[1])
  # calculate power
  post_power$woody_power[i] <- power_test(effect = w_eff, sd = w_sd, sample_size = sample_size)
  # get effect size for flower in light, moderate, intensive groups
  f_eff <- Distri_params$flowr[i+1]-Distri_params$flowr[1]
  # get sd for flower in light, moderate, intensive groups
  f_sd <- max(Distri_params$flowr_sd[1])
  # calculate power
  post_power$flowr_power[i] <- power_test(effect = f_eff, sd = f_sd, sample_size = sample_size)
}
 # store results
write.csv(post_power,"results/03-power.csv",row.names = F)

# calculate needed sample size----
sample_sizes_to_try <- c(2,3,4,5,7,10,15,20,30,40,50,100,200,300,400,500)


for(i in 1:3){
  # for three groups, two taxon, get effect sizes and sd
  w_eff <- Distri_params$woody[i+1]-Distri_params$woody[1]
  w_sd <- max(Distri_params$woody_sd[1])
  f_eff <- Distri_params$flowr[i+1]-Distri_params$flowr[1]
  f_sd <- max(Distri_params$flowr_sd[1])
  # woody first, calculate power along sample size gradient
  power_levels <- c()
  for(j in 1: length(sample_sizes_to_try)){
    power_levels[j] <- power_test(effect = w_eff, sd = w_sd, sample_size = sample_sizes_to_try[j])
  }
  power_results <- tibble(sample = sample_sizes_to_try,
                          power = power_levels)
  # plot curves
  ggplot(power_results, 
         aes(x = sample, y = power)) +
    geom_line(color = 'red', size = 1.5) + 
    # add a horizontal line at 90%
    geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
    # Prettify!
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Sample Size', y = 'Power')
  ggsave(paste0("results/03_woody_",post_power$Group[i],".png"), width = 6, height = 4) # save plot
  saveRDS(power_results,paste0("results/03_woody_",post_power$Group[i],".rds")) # save results
  # flower second, calculate power along sample size gradient
  power_levels <- c()
  for(j in 1: length(sample_sizes_to_try)){
    power_levels[j] <- power_test(effect = f_eff, sd = f_sd, sample_size = sample_sizes_to_try[j])
  }
  power_results <- tibble(sample = sample_sizes_to_try,
                          power = power_levels)
  # plot curve
  ggplot(power_results, 
         aes(x = sample, y = power)) +
    geom_line(color = 'red', size = 1.5) + 
    # add a horizontal line at 90%
    geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
    # Prettify!
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Sample Size', y = 'Power')
  ggsave(paste0("results/03_flowr_",post_power$Group[i],".png"), width = 6, height = 4) # store plot
  saveRDS(power_results,paste0("results/03_flowr_",post_power$Group[i],".rds")) # store results
}


# read rds and transform into csv
file <- dir("results") %>% as_tibble() %>% filter(str_detect(value, "rds"))

for(i in 1: nrow(file)){
  tmp <- readRDS(paste0("results/", file$value[i]))
  filename <- str_sub(file$value[i], 1, -4)
  write.csv(tmp, paste0("results/",filename,"csv"), row.names = F)
}



# As Light & Moderate groups of flowers are not showing the exact sample size needed for 80% power, I am running another sets of sample size gradient to better decide the exact value----



  i = 1 # light group
  sample_sizes_to_try <- seq(90,110,1)
  #get effect sizes and sd
  f_eff <- Distri_params$flowr[i+1]-Distri_params$flowr[1]
  f_sd <- max(Distri_params$flowr_sd[1])
  
  #. flower light group power calculation
  power_levels <- c()
  for(j in 1: length(sample_sizes_to_try)){
    power_levels[j] <- power_test(effect = f_eff, sd = f_sd, sample_size = sample_sizes_to_try[j])
  }
  power_results <- tibble(sample = sample_sizes_to_try,
                          power = power_levels)
  # plot curve
  ggplot(power_results, 
         aes(x = sample, y = power)) +
    geom_line(color = 'red', size = 1.5) + 
    # add a horizontal line at 90%
    geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
    # Prettify!
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Sample Size', y = 'Power')
  ggsave(paste0("results/03_flowr_finer_",post_power$Group[i],".png"), width = 6, height = 4) # store plot
  saveRDS(power_results,paste0("results/03_flowr_finer_",post_power$Group[i],".rds")) # store results

  
  i = 2 # moderate group
  sample_sizes_to_try <- seq(20,40,1)
  #get effect sizes and sd
  f_eff <- Distri_params$flowr[i+1]-Distri_params$flowr[1]
  f_sd <- max(Distri_params$flowr_sd[1])
  
  #. flower moderate group power calculation
  power_levels <- c()
  for(j in 1: length(sample_sizes_to_try)){
    power_levels[j] <- power_test(effect = f_eff, sd = f_sd, sample_size = sample_sizes_to_try[j])
  }
  power_results <- tibble(sample = sample_sizes_to_try,
                          power = power_levels)
  # plot curve
  ggplot(power_results, 
         aes(x = sample, y = power)) +
    geom_line(color = 'red', size = 1.5) + 
    # add a horizontal line at 90%
    geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
    # Prettify!
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Sample Size', y = 'Power')
  ggsave(paste0("results/03_flowr_finer_",post_power$Group[i],".png"), width = 6, height = 4) # store plot
  saveRDS(power_results,paste0("results/03_flowr_finer_",post_power$Group[i],".rds")) # store results

