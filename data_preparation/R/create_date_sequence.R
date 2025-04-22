require("tidyr")

create_date_sequence <- function(df) {
  df$date_sequence <- Map(seq, 
                          from = as.Date(df$estimated_start_date), 
                          to = as.Date(df$estimated_end_date), 
                          by = "day")
  
  df <- unnest(df, date_sequence)
  
  return(df)
}
