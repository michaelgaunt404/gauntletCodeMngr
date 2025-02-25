#' Check Script and Function Names in a Package Directory
#'
#' This is a convenience function that helps encourage package development by
#' performing an automated check to ensure that script names match the function
#' names contained within them. It also counts the number of functions in each
#' script and returns a table listing the scripts, the functions inside each script,
#' whether the script name matches any function name, and if there are multiple
#' functions in the script.
#'
#' @param directory A character string specifying the path to the directory
#' containing the R scripts.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{multiple_functions}{Logical, indicating if there are multiple functions in the script.}
#'   \item{number_functions}{Integer, the number of functions in the script.}
#'   \item{name_match}{Logical, indicating if the script name matches any function name inside it.}
#'   \item{script_name}{Character, the name of the script without the ".R" extension.}
#'   \item{function_names}{Character, the names of the functions found inside the script.}
#' }
#'
#' @importFrom purrr map_df
#' @importFrom stringr str_detect
#' @importFrom dplyr arrange select
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load necessary libraries
#' library(purrr)
#' library(dplyr)
#' library(stringr)
#'
#' # Function to create temporary R scripts with matching function names
#' create_temp_scripts <- function(directory) {
#'   for (i in 1:3) {
#'     script_name <- file.path(directory, paste0("function", i, ".R"))
#'     function_name <- paste0("function", i)
#'     function_code <- paste0(function_name, " <- function() {\n  # Placeholder function\n}\n")
#'     writeLines(function_code, script_name)
#'   }
#' }
#'
#' # Create a temporary directory
#' temp_dir <- tempdir()
#' temp_sub_dir <- file.path(temp_dir, "temporary_directory_one")
#' dir.create(temp_sub_dir)
#'
#' # Create temporary R scripts with matching function names
#' create_temp_scripts(temp_sub_dir)
#'
#' # Check the script and function names using the provided function
#' result <- checkPkg_scrpt_fnctn_names(temp_sub_dir)
#'
#' # Print the result
#' print(result)
#' }
check_scrpt_fnctn_names <- function(directory) {
  scripts <- list.files(directory, pattern = "\\.R$", full.names = TRUE) %>%
    sort()

  # script = scripts[6]
  result_df <- scripts %>%
    purrr::map_df(function(script) {
      script_content <- readLines(script)
      script_name <- sub("\\.R$", "", basename(script))

      # Find all lines that contain function declarations
      function_lines <-
        c(
          grep("\\s*\\w+\\s*<-\\s*function\\s*\\(", script_content, value = TRUE),
          grep("\\s*\\w+\\s*=\\s*function\\s*\\(", script_content, value = TRUE)
        ) %>%
        .[!stringr::str_detect(., "^# |#'")] %>%
        .[!stringr::str_detect(., "function\\(e|err|error\\)")]

      function_names <- c()

      for (line in function_lines) {
        # Extract the function name from the line
        function_name <-
          c(
            sub("\\s*=\\s*function\\s*\\(.*", "", line),
            sub("\\s*<-\\s*function\\s*\\(.*", "", line)
          ) %>%
          .[!stringr::str_detect(., "function\\(")]
        function_name <- sub("^\\s+", "", function_name)
        function_name <- sub("\\s+$", "", function_name)
        function_names <- c(function_names, function_name) %>%
          unique()
      }

      name_match <- script_name %in% function_names
      multiple_functions <- length(function_names) > 1

      tryCatch({
        results <- data.frame(
          script_name = script_name,
          function_names = function_names,
          name_match = name_match,
          multiple_functions = multiple_functions,
          number_functions = length(function_names)
        )
        return(results)
      },  error = function(e) {
        results <- data.frame(
          script_name = script_name,
          function_names = "NULL",
          name_match = name_match,
          multiple_functions = multiple_functions,
          number_functions = length(function_names)
        )
        return(results)
      })
    })

  result_df <- result_df %>%
    dplyr::arrange(multiple_functions, name_match, script_name, function_names) %>%
    dplyr::select(multiple_functions, number_functions, name_match, script_name, function_names)

  return(result_df)
}
