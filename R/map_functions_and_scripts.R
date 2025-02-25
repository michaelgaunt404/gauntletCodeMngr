#' Map Functions and Scripts in an R Package
#'
#' This function analyzes R scripts within two directories: a function directory (e.g., `R` folder for an R package) and an optional code directory (e.g., `inst/scripts` folder). It maps the relationships between functions and scripts, visualizing the connections in a network diagram and providing a data frame of function calls.
#'
#' @param function_directory Character. Path to the directory containing R scripts with function definitions (e.g., `R` directory in an R package).
#' @param code_directory Character. Path to the directory containing other R scripts (e.g., `inst/scripts` directory). Default is \code{NULL}.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{vis_plot}}{An interactive network plot showing the relationships between functions and scripts.}
#'     \item{\code{df_to_from}}{A data frame mapping function-to-function and function-to-script relationships.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Scans the `function_directory` to identify R scripts and extracts all defined functions.
#' 2. Optionally scans the `code_directory` for additional scripts.
#' 3. Maps function calls within and across scripts, creating a data frame of relationships.
#' 4. Visualizes the relationships in an interactive network plot using \pkg{visNetwork}.
#'
#' The function can be used to understand code dependencies, identify potential redundancies, and visualize how functions and scripts interact within a project.
#'
#' @importFrom purrr map_df map_chr
#' @importFrom dplyr mutate select filter arrange group_by ungroup case_when row_number
#' @importFrom stringr str_detect str_remove str_trim str_extract_all
#' @importFrom visNetwork visNetwork visEdges visGroups visHierarchicalLayout visOptions visPhysics
#'
#' @note
#' This function assumes that R scripts in the `function_directory` contain properly defined functions, and that scripts in the `code_directory` may invoke these functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Create a temporary project directory
#' project_dir <- tempfile("r_project_")
#' dir.create(project_dir)
#' cat("Project directory created at:", project_dir, "\n")
#'
#' # Create subdirectories: "R" for functions and "code" for scripts
#' function_dir = file.path(project_dir, "R")
#' dir.create(function_dir)
#' code_dir = file.path(project_dir, "code")
#' dir.create(code_dir)
#'
#' # -------------------------
#' # Create custom function files in the R folder
#' # File: func1.R - returns a random number
#' func1_code <- '
#' get_random_number <- function() {
#'   # Return a random integer between 1 and 100
#'   sample(1:100, 1)
#' }
#'
#' writeLines(func1_code, con = file.path(project_dir, "R", "func1.R"))
#'
#' # File: func2.R - uses get_random_number to return double the value
#' func2_code <- '
#' # Source the first function
#' source("func1.R")
#' get_double_random <- function() {
#'   num <- get_random_number()
#'   num * 2
#' }
#'
#' writeLines(func2_code, con = file.path(project_dir, "R", "func2.R"))
#'
#'
#' # File: func3.R - uses get_random_number to return double the value
#' func3_code <- '
#' # Source the first function
#' print_hello_world <- function() {
#'   print("hello world!")
#' }
#'
#' writeLines(func3.R, con = file.path(project_dir, "R", "func3.R"))
#'
#' # -------------------------
#' # Create an R script in the code folder that uses the custom functions
#' # File: run_script.R - sources func2.R and prints the result
#' script_code <- '
#' # Source the custom functions from the R directory
#' source("../R/func2.R")
#'
#' # Use the function and print the result
#' result <- get_double_random()
#' cat("Double random number:", result, "\n")
#'
#' writeLines(script_code, con = file.path(project_dir, "code", "run_script.R"))
#'
#' cat("Temporary R project structure has been set up.\n")
#'
#' map_functions_and_scripts(function_directory = function_dir, code_directory = code_dir)
#'
#'
#' }
map_functions_and_scripts = function(function_directory, code_directory = NULL) {
  # List and sort function scripts
  function_scripts <- list.files(function_directory, pattern = "\\.R$", full.names = TRUE) %>%
    sort()

  # Parse function scripts to find defined functions
  result_df = function_scripts %>% purrr::map_df(function(script) {
    script_content <- readLines(script)
    script_name <- sub("\\.R$", "", basename(script))
    function_lines <- c(
      grep("\\s*\\w+\\s*<-\\s*function\\s*\\(", script_content, value = TRUE),
      grep("\\s*\\w+\\s*=\\s*function\\s*\\(", script_content, value = TRUE)
    ) %>% .[!stringr::str_detect(., "^# |#'")] %>% .[!stringr::str_detect(., "function\\(e|err|error\\)")]

    function_names <- function_lines %>%
      purrr::map_chr(~ gsub("\\s*(<-|=)\\s*function\\(.*", "", .)) %>%
      stringr::str_trim() %>%
      unique()

    data.frame(
      file_name = script_name,
      function_names = paste(function_names, collapse = ", "),
      number_functions = length(function_names),
      stringsAsFactors = FALSE
    )
  })

  index_functions = list.files(function_directory, pattern = "\\.R$", full.names = TRUE)

  if (!is.null(code_directory)){
    index_scripts = list.files(code_directory, pattern = "\\.R$", full.names = TRUE)

    code_scripts = c(index_functions
                     ,index_scripts)

  }

  code_scripts = c(index_functions)


  code_function_calls <- code_scripts %>%
    # .[5] %>%
    purrr::map_df(function(script) {
      # browser()
      script_content <- readLines(script)
      calls = grep("\\w+\\s*\\(", script_content, value = TRUE) %>%
        stringr::str_extract_all("\\b\\w+\\b") %>%
        unlist() %>%
        unique() %>%
        sort()

      if(is.null(calls)){
        calls_df = data.frame(
          from = NA_character_
          ,to = basename(script)
        )
      } else {
        calls_df = data.frame(
          from = calls
          ,to = basename(script)
        )

      }
      calls_df = calls_df %>%
        dplyr::mutate(to = stringr::str_remove(to, "\\.R"))

      return(calls_df)
    }) %>%
    dplyr::filter((from %in% result_df$file_name) |
                    (from == to)) %>%
    dplyr::arrange(to, from) %>%
    dplyr::mutate(from = dplyr::case_when(from == to~NA_character_, T~from))

  nodes = data.frame(
    label = unique(c(code_function_calls$from, code_function_calls$to)) %>%
      sort(), stringsAsFactors = FALSE) %>%
    dplyr::mutate(group = dplyr::case_when(
      label %in% stringr::str_remove(basename(index_scripts), "\\.R")~"scripts"
      ,T~"functions"
    )) %>%
    unique() %>%
    dplyr::mutate(id = dplyr::row_number()-1)

  links = code_function_calls %>%
    dplyr::mutate(
      from = match(from, nodes$label) - 1,
      to = match(to, nodes$label) - 1
    ) %>%
    dplyr::select(from, to) %>%
    dplyr::arrange(to, from)

  temp_vis = visNetwork::visNetwork(nodes, links, height = "800px", width = "100%") %>%
    visNetwork::visEdges(arrows = "to", length = 100
                         ,physics = F) %>%
    visNetwork::visGroups(groupname = "functions", color = "lightblue", shape = "triangle",
                          shadow = list(enabled = F)) %>%
    visNetwork::visGroups(groupname = "scripts", color = "orange", shape = "square") %>%
    visNetwork::visHierarchicalLayout(
      direction = "LR"
      ,levelSeparation = 300
      ,blockShifting = T
      ,edgeMinimization = T
      ,parentCentralization = T
      ,sortMethod = "directed") %>%
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 2), nodesIdSelection = F) %>%
    visNetwork::visPhysics(stabilization = F)

  output_object = list(
    vis_plot = temp_vis
    ,df_to_from = code_function_calls
  )

  return(output_object)
}
