library(tidyverse)
library(tidymodels)
library(baguette)
library(usemodels)
library(vip)
library(hrbrthemes)
library(skimr)
library(future)
library(lobstr)
library(butcher)
library(tictoc)

options(scipen = 999)
theme_set(theme_ipsum())

set.seed(1234)

#https://www.tmwr.org/index.html

#eda combined
assessments_valid <- read_csv("data/cleaned/big/clean_assessment_data_geocoded.csv")
parcel_geo <- read_csv("data/cleaned/big/clean_parcel_geo.csv")

skim(assessments_valid)

housing_sales <- assessments_valid %>% 
  left_join(parcel_geo, by = c("par_id" = "pin")) %>% 
  select(-sale_price) %>% 
  select(everything(), longitude, latitude, year_built) %>% 
  select(par_id, sale_price_adj, house_age_at_sale, sale_year, sale_month, lot_area, 
         finished_living_area, bedrooms, full_baths, half_baths, geo_id, 
         style_desc, grade_desc, condition_desc, ac_flag, heat_type,
         longitude, latitude, year_built) %>% 
  mutate(sale_price_adj = log10(sale_price_adj))


set.seed(1234)
# Put 3/4 of the data into the training set 
data_split <- initial_split(housing_sales, prop = 3/4, strata = sale_price_adj)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

#create recipe
model_recipe_standard <- recipe(sale_price_adj ~ .,
                                data = train_data) %>% 
  update_role(par_id, new_role = "id") %>% 
  update_role(longitude, latitude, year_built, new_role = "metadata") %>% 
  #step_log(sale_price_adj, base = 10, skip = TRUE) %>% 
  step_mutate(condition_desc = as.character(condition_desc),
              grade_desc = as.character(grade_desc),
              ac_flag = as.character(ac_flag),
              heat_type = as.character(heat_type)) %>% 
  step_mutate(heat_type = case_when(heat_type == "Central Heat" ~ heat_type,
                                    heat_type == "None" ~ heat_type,
                                    is.na(heat_type) ~ "Missing",
                                    TRUE ~ "Other")) %>% 
  step_impute_mode(condition_desc, grade_desc, ac_flag) %>%
  step_impute_median(bedrooms, full_baths, half_baths) %>%
  step_mutate(condition_desc = case_when(condition_desc %in% c("Poor", "Very Poor", "Unsound") ~ "Poor or worse",
                                         condition_desc %in% c("Very Good", "Excellent") ~ "Very Good or better",
                                         TRUE ~ condition_desc)) %>%
  step_mutate(grade_desc = case_when(grade_desc %in% c("Below Average", "Poor") ~ "Below Average or worse",
                                     grade_desc %in% c("Very Good", "Excellent", "Highest Cost") ~ "Very Good or better",
                                     TRUE ~ grade_desc)) %>%
  step_other(style_desc, threshold = .05, other = "style_other") %>%
  step_other(geo_id, threshold = 500, other = "school_other") %>%
  step_string2factor(geo_id, style_desc, grade_desc, condition_desc, ac_flag, heat_type, sale_month) %>%
  step_relevel(condition_desc, ref_level = "Average") %>% 
  step_relevel(grade_desc, ref_level = "Average") %>% 
  step_relevel(sale_month, ref_level = "Jun") %>%
  step_relevel(ac_flag, ref_level = "TRUE") %>% 
  step_relevel(heat_type, ref_level = "Central Heat")

model_recipe_standard %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  glimpse()

model_recipe_dummy <- recipe(sale_price_adj ~ .,
                             data = train_data) %>% 
  update_role(par_id, new_role = "id") %>% 
  update_role(longitude, latitude, year_built, new_role = "metadata") %>% 
  #step_log(sale_price_adj, base = 10, skip = TRUE) %>% 
  step_mutate(condition_desc = as.character(condition_desc),
              grade_desc = as.character(grade_desc),
              ac_flag = as.character(ac_flag),
              heat_type = as.character(heat_type)) %>% 
  step_mutate(heat_type = case_when(heat_type == "Central Heat" ~ heat_type,
                                    heat_type == "None" ~ heat_type,
                                    is.na(heat_type) ~ "Missing",
                                    TRUE ~ "Other")) %>% 
  step_impute_mode(condition_desc, grade_desc, ac_flag) %>%
  step_impute_median(bedrooms, full_baths, half_baths) %>%
  step_mutate(condition_desc = case_when(condition_desc %in% c("Poor", "Very Poor", "Unsound") ~ "Poor or worse",
                                         condition_desc %in% c("Very Good", "Excellent") ~ "Very Good or better",
                                         TRUE ~ condition_desc)) %>%
  step_mutate(grade_desc = case_when(grade_desc %in% c("Below Average", "Poor") ~ "Below Average or worse",
                                     grade_desc %in% c("Very Good", "Excellent", "Highest Cost") ~ "Very Good or better",
                                     TRUE ~ grade_desc)) %>%
  step_other(style_desc, threshold = .05, other = "style_other") %>%
  step_other(geo_id, threshold = 500, other = "school_other") %>%
  step_string2factor(geo_id, style_desc, grade_desc, condition_desc, ac_flag, heat_type, sale_month) %>%
  step_relevel(condition_desc, ref_level = "Average") %>% 
  step_relevel(grade_desc, ref_level = "Average") %>% 
  step_relevel(sale_month, ref_level = "Jun") %>%
  step_relevel(ac_flag, ref_level = "TRUE") %>% 
  step_relevel(heat_type, ref_level = "Central Heat") %>% 
  step_dummy(all_nominal(), -has_role(c("id", "metadata")))

model_recipe_dummy %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  glimpse()

#models

#specify lm model
lm_mod <- linear_reg() %>% 
  set_engine("lm")

#specify rf model
cores <- parallel::detectCores()
cores

rf_mod <- rand_forest() %>% 
  set_engine("ranger",
             num.threads = cores,
             importance = "impurity",
             keep.inbag = TRUE) %>% 
  set_mode("regression")

#specify bagged tree model
future::plan("multicore")
bag_spec <- bag_tree(min_n = 25) %>%
  set_engine("rpart", 
             times = 25,
             control = control_bag(allow_parallel = T)) %>%
  set_mode("regression")

#resample
folds_train <- vfold_cv(train_data, v = 10)

#workflow set
wflow_sets <- 
  workflow_set(
    preproc = list(standard = model_recipe_standard,
                   dummy = model_recipe_dummy,
                   dummy = model_recipe_dummy,
                   dummy = model_recipe_dummy), 
    models = list(random_forest = rf_mod,
                  random_forest = rf_mod,
                  linear_model = lm_mod,
                  bagged_tree = bag_spec),
    cross = FALSE)

wflow_sets

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = FALSE
  )

tic()
grid_results <-
  wflow_sets %>%
  workflow_map(
    seed = 1503,
    resamples = folds_train,
    fn = "fit_resamples",
    #grid = 5,
    control = grid_ctrl,
    verbose = T
  )
toc()
#2824.896 sec elapsed

grid_results %>% 
  distinct(wflow_id)

grid_results %>% 
  collect_metrics() %>%
  write_csv("data/modelling/metrics/train_metrics_workflowsets.csv")

grid_results %>% 
  collect_metrics() %>% 
  select(model, wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  ggplot(aes(rmse, rsq, color = wflow_id, fill = wflow_id)) +
  geom_point()

grid_results %>% 
  unnest(result) %>% 
  glimpse()

grid_results %>% 
  unnest(result) %>% 
  select(wflow_id, .metrics) %>% 
  unnest(.metrics) %>% 
  filter(.metric == "rsq") %>% 
  ggplot(aes(.estimate, fill = wflow_id)) +
  geom_histogram() +
  facet_grid(wflow_id~.metric)

grid_results %>% 
  unnest(result) %>% 
  select(wflow_id, id, .metrics) %>% 
  unnest(.metrics) %>% 
  select(wflow_id, id, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  ggplot(aes(rmse, rsq, color = wflow_id)) +
  geom_point()

