##' Ensure dontrun{} Wrapping Around @examples
#'
#' This function checks all R script files in a specified folder to ensure that
#' all `@examples` sections in Roxygen documentation are wrapped in dontrun{}.
#' It ensures proper package building and simplifies function documentation
#' development for users.
#'
#' @param folder Character. The folder containing the R scripts to process. Defaults to "R".
#' @param test_individually Logical. If TRUE, prompts the user to press a key to continue
#' after processing each file. Defaults to TRUE.
#'
#' @details The function performs the following checks on each script:
#' - Ensures the script contains only one function.
#' - Verifies that Roxygen documentation is present.
#' - Checks if an `@examples` section exists.
#' - Ensures dontrun{} is not already present around the `@examples` section.
#'
#' If all conditions are met, the function automatically adds dontrun{} around the `@examples` section
#' and relocates the `@export` tag if it appears after the `@examples` section.
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have an "R" folder with R script files:
#' add_roxygen_dontrun(folder = "R", test_individually = TRUE)
#'
#' # To process another folder:
#' add_roxygen_dontrun(folder = "path/to/your/folder", test_individually = FALSE)
#' }
add_roxygen_dontrun = function(folder = "R", test_individually = T) {
  # Check if the folder exists
  if (!dir.exists(folder)){stop("The specified folder does not exist.")}

  # Get all .R files in the folder
  r_files <- list.files(folder, pattern = "\\.R$", full.names = TRUE)
  if (length(r_files) == 0){stop("No .R files found in the folder.")}

  r_files %>%
    purrr::map(~{
      # browser()
      file = .x

      message("Processing: ", file)

      lines = readLines(file)

      chk_num_functions = (grep("function\\(", lines) %>% length()) == 1
      chk_roxygen = (grep("#'", lines) %>% length()) >5
      chk_at_example = (grep("#' @example", lines[-15]) %>% length()) == 1
      chk_dontrun_present = (grep("dontrun\\{", lines) %>% length()) == 0
      # message(filepaste(lines, collapse = "\n"))

      if(!chk_num_functions){message("Skipped file (no changes made): Too many functions detected"); return(NULL)}
      if(!chk_roxygen){message("Skipped file (no changes made): No ROXYGEN detected"); return(NULL)}
      if(!chk_at_example){message("Skipped file (no changes made): No example detected"); return(NULL)}
      if(!chk_dontrun_present){message("Skipped file (no changes made): dontrun already detected"); return(NULL)}

      # Find the @examples section
      index_export <- grep("@export", lines)
      index_example_start <- grep("@examples", lines)
      index_comment_last <- grep("#'", lines) %>% max()
      index_dontrun <- grep("#' \\dontrun", lines)
      index_at_last <- grep("#' @", lines)

      if(index_export > index_example_start) {
        message("--Relocated @export")
        lines = lines[-index_export]
        lines = append(lines, "#' @export", after = index_example_start - 1)
        index_example_start <- grep("@examples", lines)
        index_comment_last <- grep("#'", lines) %>% max()
        index_dontrun <- grep("#' \\dontrun", lines)
        index_at_last <- grep("#' @", lines) %>% max()
      }

      lines = append(lines, "#' \\dontrun{", after = index_example_start)
      lines = append(lines, "#' }", after = index_comment_last+1)

      writeLines(lines, file)

      if(test_individually){readline(prompt="Press any key to continue... ")}

      return(NULL)
    })

}
