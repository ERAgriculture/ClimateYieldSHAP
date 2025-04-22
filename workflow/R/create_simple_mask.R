library(dplyr)
library(rlang)

create_simple_mask <- function(data, column, value, new_column_name) {
  # Capture the column name and new column name
  col_expr <- enquo(column)
  new_col_name <- ensym(new_column_name)
  
  # Create the mask
  data %>%
    mutate(
      !!new_col_name := factor(
        ifelse(!!col_expr == value, as.character(value), "Others"),
        levels = c(as.character(value), "Others")
      )
    )
}
