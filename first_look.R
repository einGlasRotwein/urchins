
# First look at the data that Karolina transformed into a .csv file.

library(tidyverse)

data <- read.csv("./data/behavioral_data.csv")

# How many trials per condition and contrast difference?
data %>% 
  group_by(contrast_diff_abs, condition) %>% 
  count()

# Calculate some recoded columns.
data <- data %>% 
  mutate(feedback_type_rec = ifelse(feedback_type == 1, "reward", "punishment"),
         response_rec = case_when(response == -1 ~ "right",
                                  response == 1 ~ "left",
                                  response == 0 ~ "no go"))

# Were no-gos in equal contrast trials punished?
data %>% 
  filter(condition == "go", response == 0, contrast_diff == 0) %>% 
  summarise(all_feedback = mean(feedback_type))

# Look at reaction times for correct vs. incorrect no_go trials
data %>% 
  filter(condition == "no_go") %>% 
  
  ggplot(aes(x = feedback_type_rec, y = response_time_diff)) +
  geom_jitter(colour = "darkgreen", alpha = .3, size = 2) +
  scale_y_continuous(breaks = seq(0, 2, .25)) +
  labs(y = "response time from go cue", x = "feedback")

data %>% 
  filter(condition == "go") %>% 
  
  ggplot(aes(x = feedback_type_rec, y = response_time_diff, colour = response_rec)) +
  geom_jitter(alpha = .6, size = 2) +
  scale_y_continuous(breaks = seq(0, 2, .25)) +
  labs(y = "response time from go cue", x = "feedback") +
  theme(legend.position = "top")

# Find out where the weird response time pattern (two "stripes") in no-go responses (in no-go and go trials)
# comes from.
weird_RTs <- data %>% 
  filter(response_rec == "no go") %>% 
  mutate(response_time_split = ifelse(response_time_diff > 1.7, "high", "low"))

# Differences in continuous measures?
weird_RTs %>% 
  group_by(response_time_split) %>% 
  summarise(mean_feedback = mean(feedback_type),
            mean_contrast_diff = mean(contrast_diff))

# Differences in discrete measures (mouse, session ...)?
# The longer response times for no-go trials (> 1.7 s) only occur for the mouse Forssmann
weird_RTs %>% 
  group_by(response_time_split, mouse) %>%
  count()

# ... and only in sessions 4, 5, 6 and 7.
weird_RTs %>% 
  group_by(response_time_split, session) %>%
  count()

# ... Which are Forssmann's sessions.
data %>% 
  filter(mouse == "Forssmann") %>% 
  summarise(sessions = unique(session), mouse = unique(mouse))

# Go back to no-go plot: Are the reaction times in the punishment condition that are too long
# all because of Forssmann?
data %>% 
  filter(condition == "no_go") %>% 
  mutate(Forssmann = ifelse(mouse == "Forssmann", TRUE, FALSE)) %>% 
  
  ggplot(aes(x = feedback_type_rec, y = response_time_diff, colour = Forssmann)) +
  geom_jitter(alpha = .5, size = 2) +
  scale_y_continuous(breaks = seq(0, 2, .25)) +
  labs(y = "response time from go cue", x = "feedback") +
  theme(legend.position = "top")

# And for go trials:
data %>% 
  filter(condition == "go") %>% 
  mutate(Forssmann = ifelse(mouse == "Forssmann", TRUE, FALSE),
         nogo = ifelse(response_rec == "no go", TRUE, FALSE)) %>% 
  
  ggplot(aes(x = feedback_type_rec, y = response_time_diff, colour = Forssmann,
             shape = nogo)) +
  geom_jitter(alpha = .4, size = 3) +
  scale_y_continuous(breaks = seq(0, 2, .25)) +
  labs(y = "response time from go cue", x = "feedback") +
  theme(legend.position = "top")

data %>% 
  group_by(condition) %>% 
  count()

# Exclude Forssmann from now on.
# Mutate responses into no go and other.
data_wof <- data %>% 
  filter(mouse != "Forssmann") %>%
  mutate(response_rec2 = ifelse(response_rec == "no go", "no go", "other"))

data_wof %>% 
  filter(condition == "go") %>% 
  
  ggplot(aes(x = factor(contrast_diff_abs), y = response_time_diff, colour = feedback_type_rec)) +
  geom_jitter(alpha = .3) +
  stat_summary(aes(fill = feedback_type_rec), size = 1, shape = 21, colour = "black") +
  theme(legend.position = "top") +
  facet_wrap(~response_rec2)

# Performance as a function of absolute contrast difference
data_wof %>% 
  filter(condition == "go") %>% 
  
  ggplot(aes(x = factor(contrast_diff_abs), y = feedback_type, colour = response_rec2)) +
  stat_summary(size = 1) +
  theme(legend.position = "top")

data_wof %>%
  filter(condition == "go") %>% 
  group_by(response_rec2, feedback_type_rec, contrast_diff_abs) %>% 
  count() %>% 
  
  ggplot(aes(x = factor(contrast_diff_abs), y = n, fill = response_rec2)) +
  geom_col() +
  theme(legend.position = "top") +
  facet_wrap(~feedback_type_rec)

data_wof_go <- data_wof %>%
  filter(condition == "go") %>% 
  mutate(response_type = case_when(feedback_type_rec == "reward" ~ "correct",
                                   feedback_type_rec == "punishment" & response_rec2 == "no go" ~ "no go",
                                   feedback_type_rec == "punishment" & response_rec2 == "other" ~ "wrong"),
         response_type = factor(response_type, levels = c("no go", "wrong", "correct")))

data_wof_go %>% 
  group_by(response_type, contrast_diff_abs) %>% 
  count() %>% 
  
  ggplot(aes(x = factor(contrast_diff_abs), y = n, fill = response_type)) +
  geom_col() +
  scale_fill_manual(values = c("darkgrey", "darkred", "darkgreen")) +
  theme(legend.position = "top")

data_wof_go %>% 
  group_by(response_type, contrast_diff_abs) %>% 
  count() %>% 
  group_by(contrast_diff_abs) %>% 
  mutate(percentage = n/sum(n)) %>% 
  
  ggplot(aes(x = factor(contrast_diff_abs), y = percentage, fill = response_type)) +
  geom_col() +
  scale_fill_manual(values = c("darkgrey", "darkred", "darkgreen")) +
  theme(legend.position = "top")

