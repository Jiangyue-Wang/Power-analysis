### load library ----
library(tidyverse)
library(plotrix)
library(fixest)
library(broom)

### set params ----
pset <- 0.05
### read in data----
#Here we used real richness value, and assume a t test comparison without correction for p
data <- read_csv("data/richness_abundance_trt.csv")
data %<>% mutate(Stand = as.factor(Stand), Block = as.factor(Block), Treatment = factor(Treatment, levels = c("CONTROL", "LIGHT", "MODERATE", "INTENSIVE"))) %>% select(Stand, Block, Treatment, spec_woody, SR_Flow) %>% na.omit()
data %>% group_by(Treatment) %>% summarise(n = n())
sample_size <- 7


Distri_params<- data %>% group_by(Treatment) %>% summarise(woody = mean(spec_woody), woody_sd = sd(spec_woody), flowr = mean(SR_Flow), flowr_sd = sd(SR_Flow))

### calculate power----
post_power <- data.frame(Group = c("CL", "CM", "CI"), woody_power = NA, flowr_power = NA)

# function to test power, iteration 999 times
power_test <- function(effect, sd, sample_size){
  sig_results <- c()
  
  for (i in 1:999) {
    # Have to re-create the data EVERY TIME or it will just be the same data over and over
    tib <- tibble(
      X = runif(sample_size, 0, 1)
    ) %>%
      mutate(Y = effect*X + rnorm(sample_size, mean = 0, sd = sd))
    
    # Run the analysis
    model <- feols(Y ~ X, data = tib, se = 'hetero')
    
    # Get the results
    sig_results[i] <- tidy(model)$p.value[2] <= pset
  }
  
  sig_results %>%
    mean() %>%
    return()
}

for(i in 1:3){
  w_eff <- Distri_params$woody[i+1]-Distri_params$woody[1]
  w_sd <- max(Distri_params$woody_sd[1])
  post_power$woody_power[i] <- power_test(effect = w_eff, sd = w_sd, sample_size = sample_size)
  
  f_eff <- Distri_params$flowr[i+1]-Distri_params$flowr[1]
  f_sd <- max(Distri_params$flowr_sd[1])
  post_power$flowr_power[i] <- power_test(effect = f_eff, sd = f_sd, sample_size = sample_size)
}

write.csv(post_power,"results/01-power.csv",row.names = F)
# calculate needed sample size----
sample_sizes_to_try <- c(7,10,15,20,25,30,35)

for(i in 1:3){
  w_eff <- Distri_params$woody[i+1]-Distri_params$woody[1]
  w_sd <- max(Distri_params$woody_sd[1])
  f_eff <- Distri_params$flowr[i+1]-Distri_params$flowr[1]
  f_sd <- max(Distri_params$flowr_sd[1])
  # woody first
  power_levels <- c()
  for(j in 1: length(sample_sizes_to_try)){
    power_levels[j] <- power_test(effect = w_eff, sd = w_sd, sample_size = sample_sizes_to_try[j])
  }
  power_results <- tibble(sample = sample_sizes_to_try,
                          power = power_levels)
  ggplot(power_results, 
         aes(x = sample, y = power)) +
    geom_line(color = 'red', size = 1.5) + 
    # add a horizontal line at 90%
    geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
    # Prettify!
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Sample Size', y = 'Power')
  ggsave(paste0("results/01_woody_",post_power$Group[i],".png"), width = 6, height = 4)
  saveRDS(power_results,paste0("results/01_woody_",post_power$Group[i],".rds"))
  # flower second
  power_levels <- c()
  for(j in 1: length(sample_sizes_to_try)){
    power_levels[j] <- power_test(effect = f_eff, sd = f_sd, sample_size = sample_sizes_to_try[j])
  }
  power_results <- tibble(sample = sample_sizes_to_try,
                          power = power_levels)
  ggplot(power_results, 
         aes(x = sample, y = power)) +
    geom_line(color = 'red', size = 1.5) + 
    # add a horizontal line at 90%
    geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
    # Prettify!
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Sample Size', y = 'Power')
  ggsave(paste0("results/01_flowr_",post_power$Group[i],".png"), width = 6, height = 4)
  saveRDS(power_results,paste0("results/01_flowr_",post_power$Group[i],".rds"))
}
