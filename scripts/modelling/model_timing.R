#create individual model workflows for timing
#create rf workflow
rf_wflow_standard <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(model_recipe_standard)

rf_wflow_dummy <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(model_recipe_dummy)

#create bagged tree model
future::plan("multisession")
bag_spec <- bag_tree(min_n = 25) %>%
  set_engine("rpart", 
             times = 25,
             control = control_bag(allow_parallel = T)) %>%
  set_mode("regression")

bag_wf_dummy <- workflow() %>%
  add_model(bag_spec) %>% 
  add_recipe(model_recipe_dummy)

#rf standard recipe
tic.clearlog()
tic()
rf_wflow_standard_fit <- rf_wflow_standard %>% 
  fit(train_data)
toc(log = T)

rf_wflow_standard_fit_time <- tic.log(format = T)

tic.clearlog()
tic()
rf_workflow_standard_res <- rf_wflow_standard_fit %>% 
  predict(slice_head(train_data, n = 1))
toc(log = T)

rf_wflow_standard_pred_time <- tic.log(format  = T)

#rf dummy recipe
tic.clearlog()
tic()
rf_wflow_dummy_fit <- rf_wflow_dummy %>% 
  fit(train_data)
toc(log = T)

rf_wflow_dummy_fit_time <- tic.log(format  = T)

tic.clearlog()
tic()
rf_workflow_dummy_res <- rf_wflow_dummy_fit %>% 
  predict(slice_head(train_data, n = 1))
toc(log = T)

rf_wflow_dummy_pred_time <- tic.log(format  = T)

#bagged tree speed
tic.clearlog()
tic()
bag_wflow_dummy_fit <- bag_wf_dummy %>% 
  fit(train_data)
toc(log = T)

bag_wflow_dummy_fit_time <- tic.log(format  = T)

tic.clearlog()
tic()
bag_wflow_dummy_res <- bag_wflow_dummy_fit %>% 
  predict(slice_head(train_data, n = 1))
toc(log = T)

bag_wflow_dummy_pred_time <- tic.log(format  = T)

timing_data <- tibble(wflow_id = c("rf_wflow_dummy_fit_time",
                    "rf_wflow_dummy_pred_time",
                    "rf_wflow_standard_fit_time",
                    "rf_wflow_standard_pred_time",
                    "bag_wflow_dummy_fit_time",
                    "bag_wflow_dummy_pred_time"
                    ),
       timing = c(rf_wflow_dummy_fit_time,
                  rf_wflow_dummy_pred_time,
                  rf_wflow_standard_fit_time,
                  rf_wflow_standard_pred_time,
                  bag_wflow_dummy_fit_time,
                  bag_wflow_dummy_pred_time)) %>% 
  mutate(timing = map_chr(timing, 1) %>% parse_number(),
         type = case_when(str_detect(wflow_id, "fit") ~ "fit",
                          str_detect(wflow_id, "pred") ~ "pred"),
         model = case_when(str_detect(wflow_id, "rf") ~ "rf",
                           str_detect(wflow_id, "bag") ~ "bag"),
         recipe = case_when(str_detect(wflow_id, "standard") ~ "standard",
                            str_detect(wflow_id, "dummy") ~ "dummy")) %>% 
  arrange(type, timing)

timing_data

timing_data %>% 
  write_csv("data/modelling/metrics/timing.csv")

timing_data %>% 
  ggplot(aes(timing, fill = model)) +
  geom_histogram() +
  facet_wrap(~type, scales = "free")


rf_wflow_standard_fit %>% 
  lobstr::obj_size()

rf_wflow_standard_fit %>% 
  butcher() %>% 
  lobstr::obj_size()

rf_wflow_standard_fit %>% 
  write_rds("data/modelling/objects/rf_wflow_standard_fit.rds")