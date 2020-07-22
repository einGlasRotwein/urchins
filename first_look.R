
# First look at the data that Karolina transformed into a .csv file.

library(tidyverse)

data <- read.csv("./data/behavioral_data.csv")

head(data)

data %>% 
  group_by(contrast_diff_abs, condition) %>% 
  count()

# Look at reaction times for correct vs. incorrect no_go trials
data %>% 
  filter(condition == "no_go") %>% 
  mutate(feedback_type_rec = ifelse(feedback_type == 1, "reward", "punishment")) %>% 
  
  ggplot(aes(x = feedback_type_rec, y = response_time)) +
  geom_jitter(colour = "darkgreen", alpha = .3, size = 2)

