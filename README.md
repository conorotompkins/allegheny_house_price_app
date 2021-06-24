# Allegheny House Price App

This R + Shiny app uses a regression model to estimate the sale price of houses (parcels) in Allegheny County, PA.

You can view the app [here](https://conorotompkins.shinyapps.io/shiny_app/)

### Process

1. Read in raw assessment data from WPRDC
    * clean_assessments.R
2. Create crosswalk of geographic IDs
    * merge_school_districts_wards.R
3. Join assessments with geo_id crosswalk
    * geocode_parcels.R
4. EDA
    * eda/assessment_eda.Rmd
5. Compare model performance
    * compare_model_performance.R
6. Compare model timing
    * model_timing.R
7. Choose final model
    * choose_model.R
8. Fit model and predict results
9. Review model results
10. Shiny App
