remap_formula <- function(formula_string, mapping) {
  # Clean up the formula string
  formula_string <- gsub("\\s+", " ", formula_string)
  formula_string <- gsub('"', '', formula_string)
  formula_string <- trimws(formula_string)
  
  # Replace each term in the formula
  for (old_name in names(mapping)) {
    formula_string <- gsub(
      paste0("\\b", old_name, "\\b"), # word boundaries to avoid partial matches
      mapping[old_name],
      formula_string
    )
  }
  
  # Additional step to fix any remaining .Dev.Mean to _Dev
  formula_string <- gsub("\\.Dev\\.Mean\\b", "_Dev", formula_string)
  
  return(as.formula(formula_string))
}