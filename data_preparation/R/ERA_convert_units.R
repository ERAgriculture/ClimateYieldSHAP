library(dplyr)

ERA_convert_units <- function(data) {
  data %>%
    mutate(
      OriUnits = Units,
      
      MeanT = case_when(
        Units == "Mg/ha" ~ MeanT * 1000,
        Units == "kg/fed" ~ MeanT * 2.381,  # 1 feddan ≈ 0.42 hectares
        Units == "kg/acre" ~ MeanT * 2.471, # 1 acre ≈ 0.4047 hectares
        TRUE ~ MeanT
      ),
      across(any_of("MeanC"), ~ case_when(  # Skip MeanC if it does not exist
        Units == "Mg/ha" ~ MeanC * 1000,
        Units == "kg/fed" ~ MeanC * 2.381,  # 1 feddan ≈ 0.42 hectares
        Units == "kg/acre" ~ MeanC * 2.471, # 1 acre ≈ 0.4047 hectares
        TRUE ~ .x
      )),
      Units = case_when(
        Units == "Mg/ha" ~ "kg/ha",
        Units == "kg/fed" ~ "kg/ha",
        Units == "kg/acre" ~ "kg/ha",
        Units == "kg/ha/yr" ~ "kg/ha",
        TRUE ~ Units
      )
    )
}
