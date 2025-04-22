#!/usr/bin/env Rscript

# Script to automatically generate requirements.txt by scanning R scripts
# for library(), require(), and other package loading functions

generate_requirements <- function(directory = ".", output_file = "requirements.txt") {
  # Get all R files in the specified directory and its subdirectories
  r_files <- list.files(
    path = directory,
    pattern = "\\.R$|\\.r$|\\.Rmd$|\\.txt$", 
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Initialize an empty vector to store package names
  packages <- c()
  
  # Patterns to match for package loading
  patterns <- c(
    "library\\(([^\\)]+)\\)",
    "require\\(([^\\)]+)\\)",
    "requireNamespace\\(([^\\)]+)\\)",
    "import\\(([^\\)]+)\\)"
  )
  
  # Process each R file
  for (file in r_files) {
    tryCatch({
      # Read file content
      content <- readLines(file)
      
      # Extract package names using regex patterns
      for (pattern in patterns) {
        matches <- gregexpr(pattern, content, perl = TRUE)
        for (i in 1:length(matches)) {
          if (matches[[i]][1] != -1) {
            match_positions <- matches[[i]]
            match_lengths <- attr(matches[[i]], "match.length")
            
            for (j in 1:length(match_positions)) {
              # Extract the full match
              full_match <- substr(content[i], match_positions[j], 
                                   match_positions[j] + match_lengths[j] - 1)
              
              # Extract the package name from inside the parentheses
              pkg_match <- gsub(pattern, "\\1", full_match, perl = TRUE)
              
              # Clean up the package name (remove quotes, whitespace)
              pkg_name <- gsub("[\"\']", "", pkg_match)
              pkg_name <- trimws(pkg_name)
              
              if (nchar(pkg_name) > 0) {
                packages <- c(packages, pkg_name)
              }
            }
          }
        }
      }
    }, error = function(e) {
      warning(paste("Error processing file:", file, "\n", e$message))
    })
  }
  
  # Clean up package names and remove duplicates
  packages <- unique(packages)
  packages <- packages[!grepl("^\\s*$", packages)]  # Remove empty strings
  
  # Remove packages that are part of base R
  base_packages <- c("base", "stats", "graphics", "grDevices", "utils", "datasets", 
                     "methods", "tools", "parallel", "compiler", "splines", 
                     "tcltk", "grid", "translations")
  packages <- setdiff(packages, base_packages)
  
  # Get installed versions of packages
  installed_packages <- installed.packages()[, c("Package", "Version")]
  
  # Create requirements output with versions when available
  requirements <- character(length(packages))
  for (i in seq_along(packages)) {
    pkg <- packages[i]
    idx <- match(pkg, installed_packages[, "Package"])
    
    if (!is.na(idx)) {
      version <- installed_packages[idx, "Version"]
      requirements[i] <- paste0(pkg, "=", version)
    } else {
      requirements[i] <- pkg
    }
  }
  
  # Sort alphabetically
  requirements <- sort(requirements)
  
  # Add R version at the top
  r_version <- paste0("# R version ", R.version$major, ".", R.version$minor)
  requirements <- c(r_version, "", "# Package dependencies", requirements)
  
  # Write to file
  writeLines(requirements, output_file)
  
  cat("Requirements file generated:", output_file, "\n")
  cat("Found", length(packages), "packages\n")
  
  return(packages)
}

# Run the function if this script is executed directly
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  dir <- if (length(args) >= 1) args[1] else "."
  outfile <- if (length(args) >= 2) args[2] else "requirements.txt"
  
  generate_requirements(dir, outfile)
}