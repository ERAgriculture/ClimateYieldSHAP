#!/usr/bin/env Rscript
# install.R - Script to install all required packages
# Usage: Rscript install.R

# Function to parse requirements file
parse_requirements <- function(file_path = "requirements.txt") {
  if (!file.exists(file_path)) {
    stop("Requirements file not found: ", file_path)
  }
  
  # Read and parse the requirements file
  lines <- readLines(file_path)
  
  # Remove comments and empty lines
  lines <- lines[!grepl("^\\s*#", lines)]
  lines <- lines[nzchar(trimws(lines))]
  
  # Parse package names and versions
  packages <- list()
  for (line in lines) {
    if (grepl("=", line)) {
      parts <- strsplit(line, "=")[[1]]
      pkg_name <- trimws(parts[1])
      pkg_version <- trimws(parts[2])
      packages[[pkg_name]] <- pkg_version
    } else {
      pkg_name <- trimws(line)
      packages[[pkg_name]] <- NULL  # NULL indicates latest version
    }
  }
  
  return(packages)
}

# Function to install a package with specific version if provided
install_package <- function(pkg_name, version = NULL) {
  # Check if package is already installed
  is_installed <- suppressWarnings(require(pkg_name, character.only = TRUE, quietly = TRUE))
  
  # If the package is already installed and no specific version is required, we're done
  if (is_installed && is.null(version)) {
    message(sprintf("Package '%s' is already installed.", pkg_name))
    return(TRUE)
  }
  
  # If package is installed but we need a specific version, check version
  if (is_installed && !is.null(version)) {
    current_version <- as.character(packageVersion(pkg_name))
    if (current_version == version) {
      message(sprintf("Package '%s' (version %s) is already installed.", pkg_name, version))
      return(TRUE)
    }
    message(sprintf("Package '%s' is installed (version %s), but version %s is required. Will reinstall.", 
                    pkg_name, current_version, version))
  }
  
  # At this point, we need to install or reinstall the package
  if (!is.null(version)) {
    # Try to install specific version
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes", repos = "https://cloud.r-project.org")
    }
    
    message(sprintf("Installing %s (version %s)...", pkg_name, version))
    tryCatch({
      remotes::install_version(pkg_name, version = version, quiet = TRUE)
      return(TRUE)
    }, error = function(e) {
      message(sprintf("Could not install specific version. Error: %s", e$message))
      message("Falling back to latest version...")
      install.packages(pkg_name, repos = "https://cloud.r-project.org")
      return(FALSE)
    })
  } else {
    # Install latest version
    message(sprintf("Installing %s (latest version)...", pkg_name))
    install.packages(pkg_name, repos = "https://cloud.r-project.org")
    return(TRUE)
  }
}

# Main installation function
main <- function() {
  # Set CRAN mirror
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  # Parse requirements
  packages <- parse_requirements()
  
  # Install each package
  results <- list()
  for (pkg_name in names(packages)) {
    version <- packages[[pkg_name]]
    
    # Special handling for tricky packages
    if (pkg_name %in% c("sf", "rnaturalearth", "rnaturalearthdata")) {
      message(sprintf("Note: '%s' may require system dependencies. See package documentation if installation fails.", pkg_name))
    }
    
    # Try to install the package
    success <- tryCatch({
      install_package(pkg_name, version)
    }, error = function(e) {
      message(sprintf("Error installing %s: %s", pkg_name, e$message))
      return(FALSE)
    })
    
    results[[pkg_name]] <- success
  }
  
  # Report results
  success_count <- sum(unlist(results))
  total_count <- length(results)
  
  message("\nInstallation summary:")
  message(sprintf("Successfully installed %d of %d packages.", success_count, total_count))
  
  # If any packages failed, report them
  failed_packages <- names(results)[!unlist(results)]
  if (length(failed_packages) > 0) {
    message("\nFailed packages:")
    message(paste(" -", failed_packages, collapse = "\n"))
    message("\nPlease install these packages manually or check system dependencies.")
  }
  
  # Check for additional system configuration
  if ("sf" %in% names(packages)) {
    message("\nNote: The 'sf' package requires system libraries for geospatial functionality.")
    message("See: https://r-spatial.github.io/sf/#installing for system-specific installation instructions.")
  }
  
  message("\nInstallation complete!")
  return(invisible(results))
}

# Run the main function
main()