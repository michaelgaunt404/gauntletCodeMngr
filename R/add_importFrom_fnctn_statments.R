#' Add @importFrom Statements Based on Script Dependencies
#'
#' This function analyzes a given script to detect functions used in the
#' `package::function` notation. It then generates `@importFrom` statements
#' for each package detected, capturing the required dependencies for the script.
#'
#' @param script A character string specifying the path to the R script file.
#'
#' @return This function generates and prints `@importFrom` statements for each
#' package detected in the script.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_glue str_detect
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ex_script <- "mtcars %>%
#'    dplyr::mutate(
#'      var_1 = DescTools::Quantile(disp, probs = .6),
#'      var_2 = str_glue('{disp} {hp}')
#'    ) %>%
#'    dplyr::arrange(disp)"
#' temp_script <- tempfile(fileext = ".R")
#' writeLines(ex_script, temp_script)
#' add_importFrom_fnctn_statments(temp_script)
#' }
add_importFrom_fnctn_statments <- function(script) {
  # Read the script lines
  script_lines <- readLines(script) %>%
    .[!stringr::str_detect(., "#'")]

  # Split script into parts by '(' and whitespace
  split_parts <- unlist(strsplit(script_lines, "\\(|\\s+"))

  # Filter parts containing '::'
  parts_with_double_colon <- split_parts[grep("::", split_parts)]

  # Extract unique package names
  packages = unique(
    gsub("::.*", "", parts_with_double_colon) %>%
      gsub(".*\\{", "", .) %>%
      gsub("[[:punct:]]", "", .)
  ) %>%
    sort() %>%
    .[.!=""]

  if (length(packages) > 0) {
    message("Packages detected:")
    import_statements <- sapply(packages, function(pkg) {
      functions <- parts_with_double_colon[grep(paste0(pkg,
                                                       "::"), parts_with_double_colon)] %>%
        gsub(stringr::str_glue(".*({pkg})"), "\\1", .) %>%
        gsub("\\(|\\)", "\\1", .) %>%
        unique() %>%
        sort()


      paste("#' @importFrom"
            ,pkg
            ,paste(gsub(paste0(pkg, "::"), "", functions), collapse = " "))
    })
    cat(import_statements, "#' @importFrom magrittr %>%", sep = "\n")
    # cat()
  }
  else {
    message("No packages were detected.")
  }
  return(import_statements)
}
