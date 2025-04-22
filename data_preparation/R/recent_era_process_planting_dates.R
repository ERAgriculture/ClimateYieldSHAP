library(dplyr)
library(lubridate)

era_process_planting_dates <- function(data) {
  # 1. Reshape the data
  reshaped_data <- data %>%
    group_by(PD.Level.Name, Site.ID, Time, B.Code) %>%
    summarize(
      planting_start = as.Date(first(PD.Date.Start[PD.Variable == "Planting"])),
      planting_end = as.Date(first(PD.Date.End[PD.Variable == "Planting"])),
      harvesting_start = as.Date(first(PD.Date.Start[PD.Variable == "Harvesting"])),
      harvesting_end = as.Date(first(PD.Date.End[PD.Variable == "Harvesting"])),
      transplanting_start = as.Date(first(PD.Date.Start[PD.Variable == "Transplanting"])),
      transplanting_end = as.Date(first(PD.Date.End[PD.Variable == "Transplanting"]))
    ) %>%
    ungroup()
  
  # 2. Impute planting dates with transplanting dates if planting dates are NA
  final_data <- reshaped_data %>%
    mutate(
      planting_start = if_else(is.na(planting_start) & !is.na(transplanting_start), 
                               transplanting_start, planting_start),
      planting_end = if_else(is.na(planting_end) & !is.na(transplanting_end), 
                             transplanting_end, planting_end)
    ) %>%
    select(-transplanting_start, -transplanting_end)  # Remove transplanting columns
  
  # 3. Merge the new columns back with the original data
  final_data <- data %>%
    left_join(final_data, by = c("PD.Level.Name", "Site.ID", "Time", "B.Code")) %>%
    distinct()
  
  return(final_data)
}

### This is an old implementation, it should be deleted
# era_process_planting_dates <- function(data, imputation_DAS_harvest) {
#   # 1. Group the data
#   grouped_data <- data %>%
#     group_by(PD.Level.Name, Site.ID, Time, B.Code)
#   
#   # 2. Create avg_planting_date
#   data_with_planting <- grouped_data %>%
#     summarize(
#       avg_planting_date = if(any(PD.Variable == "Planting")) {
#         as.Date(mean(as.numeric(as.Date(c(PD.Date.Start[PD.Variable == "Planting"], 
#                                           PD.Date.End[PD.Variable == "Planting"]))), 
#                      origin = "1970-01-01"))
#       } else {
#         as.Date(NA)
#       }
#     )
#   
#   # 3. Create avg_harvest_date
#   data_with_harvest <- grouped_data %>%
#     summarize(
#       avg_harvest_date = if(any(PD.Variable == "Harvesting")) {
#         as.Date(mean(as.numeric(as.Date(c(PD.Date.Start[PD.Variable == "Harvesting"], 
#                                           PD.Date.End[PD.Variable == "Harvesting"]))), 
#                      origin = "1970-01-01"))
#       } else {
#         as.Date(NA)
#       }
#     )
#   
#   # 4. Create avg_transplanting_date
#   data_with_transplanting <- grouped_data %>%
#     summarize(
#       avg_transplanting_date = if(any(PD.Variable == "Transplanting")) {
#         as.Date(mean(as.numeric(as.Date(c(PD.Date.Start[PD.Variable == "Transplanting"], 
#                                           PD.Date.End[PD.Variable == "Transplanting"]))), 
#                      origin = "1970-01-01"))
#       } else {
#         as.Date(NA)
#       }
#     )
#   
#   # 5. Combine all new columns
#   reshaped_data <- data_with_planting %>%
#     left_join(data_with_harvest, by = c("PD.Level.Name", "Site.ID", "Time", "B.Code")) %>%
#     left_join(data_with_transplanting, by = c("PD.Level.Name", "Site.ID", "Time", "B.Code"))
#   
#   # 6. Assign transplanting date as planting date if planting date is NA
#   reshaped_data <- reshaped_data %>%
#     mutate(avg_planting_date = if_else(is.na(avg_planting_date) & !is.na(avg_transplanting_date), 
#                                        avg_transplanting_date, avg_planting_date))
#   
#   # reshaped_data <- reshaped_data %>%
#   #   mutate(avg_harvest_date = case_when(
#   #     is.na(avg_harvest_date) & !is.na(imputation_DAS_harvest) & !is.na(avg_planting_date) ~ avg_planting_date + days(imputation_DAS_harvest),
#   #     TRUE ~ avg_harvest_date
#   #   ))
#   
#   # 8. Merge the new columns back with the original data
#   final_data <- data %>%
#     left_join(reshaped_data, by = c("PD.Level.Name", "Site.ID", "Time", "B.Code")) %>%
#     distinct()
#   
#   return(final_data)
# }