#' Add Package Definitions to Functions in a Script
#'
#' This function scans a provided R script for function calls that should have package qualifiers
#' based on the packages currently loaded in the session. If it finds any unqualified function calls,
#' it replaces them with the fully qualified notation [package]::[function].
#'
#' @param script A character string specifying the path to the R script file.
#'
#' @details
#' This function uses the session information to identify which packages are loaded and the functions
#' they contain. It then reads the script, identifies non-qualified function calls, and replaces them
#' with their qualified counterparts if a match is found in the loaded packages.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_glue str_match_all str_replace_all
#' @importFrom purrr map_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(stringr)
#' library(DescTools)
#'
#' ex_script <- "mtcars %>%
#'   mutate(
#'   var_1 = Quantile(disp, probs = .6),
#'          var_2 = str_glue('{disp} {hp}'))"
#'
#' # Write the example script to a temporary file
#' temp_script <- tempfile(fileext = ".R")
#' writeLines(ex_script, temp_script)
#' add_pkg_fnctn_definitions(temp_script)
#' }
add_pkg_fnctn_definitions = function(script){
  temp_pkg_fnct_loaded = sessionInfo()[["otherPkgs"]] %>%
    names() %>%
    purrr::map_df(~{
      data.frame(pkg = .x
                 ,fnctn = stringr::str_glue("package:{.x}") %>% ls()) %>%
        dplyr::mutate(comb = stringr::str_glue("{pkg}::{fnctn}"))
    })

  #read_lines_of_script
  lines = readLines(script)
  lines_changed <- 0

  for (i in seq_along(lines)) {
    # Skip lines that start with #' or #
    if (grepl("^#'", lines[i]) || grepl("^#", lines[i])) {

      next
    }

    original_line <- lines[i]
    matches <- stringr::str_match_all(lines[i], "\\b(\\w+)\\(")[[1]]
    for (match in  matches[, 2]) {
      # Check if the function is already fully qualified
      if (grepl(paste0("::", match,"\\b" ), lines[i])) {
        next
      }

      if (match %in% temp_pkg_fnct_loaded$fnctn) {
        full_function <- temp_pkg_fnct_loaded$comb[temp_pkg_fnct_loaded$fnctn == match]
        lines[i] <- stringr::str_replace_all(lines[i], paste0("\\b", match, "\\("), paste0(full_function, "\\("))
      }
    }

    # Add inline note if the line was changed
    if (lines[i] != original_line) {
      lines[i] <- paste0(lines[i], "                      #!!!! LINE CHANGED")
      lines_changed <- lines_changed + 1
    }
  }


  if (lines_changed > 0) {
    message(stringr::str_glue("++++++++++++++++++++++++++++++++++++++++++++++++++\nTotal lines changed: {lines_changed}\n++++++++++++++++++++++++++++++++++++++++++++++++++}"))

    temp_response = lines %>%
      paste0(collapse = "\n")

    temp_response %>%
      cat()
  } else {
    message(stringr::str_glue("++++++++++++++++++++++++++++++++++++++++++++++++++\nNo functions were found to be undefined given current session info\n++++++++++++++++++++++++++++++++++++++++++++++++++"))
    temp_response = NULL
  }

  return(temp_response)
}
