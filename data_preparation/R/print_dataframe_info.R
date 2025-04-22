print_dataframe_info <- function(data_list) {
  for (df_name in names(data_list)) {
    df <- data_list[[df_name]]
    cat("\nData frame:", df_name, "\n")
    cat("Number of rows:", nrow(df), "\n")
    cat("Number of columns:", ncol(df), "\n")
    cat("Column names:\n")
    print(colnames(df))
    cat("\n")
  }
}
