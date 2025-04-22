grouped_vfold_cv <- function(data, v = 5) {
  # Create unique identifiers for each distinct observation
  data_with_groups <- data %>%
    group_by(Site.Key, M.Year, Season, yield) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()
  
  # Use group_vfold_cv to ensure observations with same group_id stay together
  folds <- group_vfold_cv(
    data_with_groups,
    group = "group_id",
    v = v
  )
  
  return(folds)
}
