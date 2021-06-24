library(tidyverse)
library(hrbrthemes)

theme_set(theme_ipsum())

model_performance <- read_csv("data/modelling/metrics/train_metrics_workflowsets.csv")

model_timing <- read_csv("data/modelling/metrics/timing.csv")

model_performance %>% 
  select(wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  ggplot(aes(rmse, rsq, color = wflow_id)) +
  geom_point()


model_timing %>% 
  ggplot(aes(x = 1, timing, label = wflow_id)) +
  #geom_jitter() +
  geom_label() +
  facet_wrap(~type, scale = "free_y")

model_timing %>% 
  select(wflow_id, type, timing) %>% 
  mutate(wflow_id = str_remove(wflow_id, "_fit_time$|_pred_time$")) %>% 
  pivot_wider(names_from = type, values_from = timing) %>% 
  ggplot(aes(fit, pred, label = wflow_id)) +
  geom_label()


