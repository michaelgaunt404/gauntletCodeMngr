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
map_functions_and_scripts =  function(function_directory, code_directory = NULL) {
    #-- Step 1: Parse all functions defined in function_directory
    function_scripts <- list.files(function_directory, pattern = "\\.R$", full.names = TRUE)

    # Identify custom functions
    function_defs <- purrr::map_df(function_scripts, function(script) {
      lines <- readLines(script, warn = FALSE)
      script_name <- tools::file_path_sans_ext(basename(script))

      func_lines <- grep("\\b\\w+\\s*(<-|=)\\s*function\\s*\\(", lines, value = TRUE)
      func_lines <- func_lines[!stringr::str_detect(func_lines, "^#|#'")]

      func_names <- func_lines %>%
        purrr::map_chr(~ gsub("\\s*(<-|=)\\s*function\\(.*", "", .)) %>%
        stringr::str_trim() %>%
        unique()

      tibble::tibble(file = script_name, function_name = func_names)
    })

    custom_functions <- unique(function_defs$function_name)

    #-- Step 2: Identify function-to-function calls (internal)
    df_function_to_function <- purrr::map_df(function_scripts, function(script_path) {
      lines <- readLines(script_path, warn = FALSE)
      parent <- tools::file_path_sans_ext(basename(script_path))

      matches <- grep("\\w+\\s*\\(", lines, value = TRUE) %>%
        stringr::str_extract_all("\\b\\w+\\b") %>%
        unlist() %>%
        intersect(custom_functions) %>%
        unique()

      if (length(matches) == 0) return(NULL)

      tibble::tibble(from = matches, to = parent, type = "in_function")
    })

    #-- Step 3: Optionally parse usage in an external code_directory
    df_function_to_script <- NULL
    if (!is.null(code_directory)) {
      code_scripts <- list.files(code_directory, pattern = "\\.R$", full.names = TRUE)

      df_function_to_script <- purrr::map_df(code_scripts, function(script_path) {
        lines <- readLines(script_path, warn = FALSE)
        script_name <- tools::file_path_sans_ext(basename(script_path))

        matches <- grep("\\w+\\s*\\(", lines, value = TRUE) %>%
          stringr::str_extract_all("\\b\\w+\\b") %>%
          unlist() %>%
          intersect(custom_functions) %>%
          unique()

        if (length(matches) == 0) return(NULL)

        tibble::tibble(from = matches, to = script_name, type = "used_in_script")
      })
    }

    #-- Step 4: Combine edge tables and prevent self-edges visually
    edges <- dplyr::bind_rows(df_function_to_function, df_function_to_script) %>%
      dplyr::mutate(from = dplyr::if_else(from == to, NA_character_, from)) %>%
      dplyr::distinct(from, to, type) %>%
      group_by(from) %>%
      mutate(from_conn = n()) %>%
      group_by(to) %>%
      mutate(to_conn = n()) %>%
      ungroup() %>%
      select(from, from_conn, to, to_conn, type)

    #-- Step 5: Construct nodes
    all_labels <- unique(c(edges$from, edges$to)) %>% na.omit()
    nodes <- tibble::tibble(label = all_labels) %>%
      dplyr::mutate(
        group = dplyr::case_when(
          label %in% function_defs$function_name ~ "function",
          label %in% function_defs$file ~ "function_file",
          TRUE ~ "script"
        ),
        id = dplyr::row_number() - 1
      )

    label_to_id <- setNames(nodes$id, nodes$label)

    links <- edges %>%
      dplyr::mutate(
        from_id = label_to_id[from],
        to_id = label_to_id[to]
      ) %>%
      dplyr::select(from = from_id, to = to_id, type)

    #-- Step 6: Visualize using visNetwork
    vis_plot <- visNetwork::visNetwork(nodes, links, height = "800px", width = "100%") %>%
      visNetwork::visEdges(arrows = "to", physics = T) %>%
      visNetwork::visGroups(groupname = "function", color = "lightblue", shape = "dot") %>%
      visNetwork::visGroups(groupname = "function_file", color = "lightgreen", shape = "box") %>%
      visNetwork::visGroups(groupname = "script", color = "orange", shape = "square") %>%
      visNetwork::visHierarchicalLayout(direction = "LR", levelSeparation = 250) %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

    return(list(
      vis_plot = vis_plot,
      edges = edges,
      nodes = nodes
    ))
  }
