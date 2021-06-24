#specify bagged tree model
future::plan("multicore")

bag_spec <- bag_tree(min_n = 25) %>%
  set_engine("rpart", 
             times = 25,
             control = control_bag(allow_parallel = T)) %>%
  set_mode("regression")

bag_wf <- workflow() %>%
  add_model(bag_spec) %>% 
  add_recipe(model_recipe_dummy)

#resample
folds_train <- vfold_cv(train_data, v = 10)

keep_pred <- control_resamples(save_pred = TRUE)

#fit against entire training data set
bag_fit <- bag_wf %>% 
  fit(data = train_data)

obj_size(bag_fit)

slice(test_data, 1:2) %>% 
  glimpse()

bag_fit %>% 
  predict(slice(test_data, 1:2))

#save model objects
bag_fit %>% 
  write_rds("data/modelling/objects/bag_model_fit_v.03.rds")
