#' Generate a Tree Representation of a Directory Structure
#'
#' `generate_tree_structure()` provides a basic textual representation of a file directory,
#' allowing users to view the layout of directories and files in a structured format.
#'
#' @param dir Character. The directory path to start the tree structure. Defaults to the current directory `"."`.
#' @param prefix Character. A prefix string used for formatting subdirectory levels. Defaults to an empty string `""`.
#' @param dirs_to_fully_list Character vector. A vector of directory names that should be fully listed without sampling.
#'        Defaults to `c("R", "code", "data", "docs", "_targets", "analysis")`.
#' @param sample_n Integer or NULL. The number of files to randomly sample per subdirectory if the directory is not
#'        in `dirs_to_fully_list`. If `NULL`, all files are listed. Defaults to `1`.
#'
#' @return A character string representing the hierarchical structure of the directory.
#'
#' @details This function scans a directory and recursively lists its files and subdirectories in a
#' structured tree format. Directories listed in `dirs_to_fully_list` are displayed completely,
#' while others may have files randomly sampled if `sample_n` is specified.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a tree structure for the current directory
#' generate_tree_structure()
#'
#' # Generate a tree structure for a specific directory with no sampling
#' generate_tree_structure(dir = "path/to/directory", sample_n = NULL)
#'
#' # Generate a tree structure and sample up to 2 files per directory
#' generate_tree_structure(dir = "path/to/directory", sample_n = 2)
#' }
generate_dir_tree_structure <- function(
    dir = ".", prefix = "",
    dirs_to_fully_list = c("R", "code", "data", "docs", "_targets", "analysis"),
    sample_n = 1) {
  items <- list.files(dir, full.names = TRUE)

  # Separate files and directories
  dirs <- items[file.info(items)$isdir]
  files <- setdiff(items, dirs)

  # Check if this directory should be fully listed
  dir_name <- basename(dir)
  full_listing <- is.null(dirs_to_fully_list) || dir_name %in% dirs_to_fully_list

  # If sample_n is not NULL and the directory is not marked for full listing,
  # then sample the files if more than sample_n exist
  if (!is.null(sample_n) && !full_listing && length(files) > sample_n) {
    files <- sample(files, sample_n)
    files <- c(files, "<...>")  # Indicate that more files exist
  }

  structure <- c()

  # Process files first
  for (i in seq_along(files)) {
    file_name <- basename(files[i])
    connector <- ifelse(i == length(files) && length(dirs) == 0, "└── ", "├── ")
    structure <- c(structure, paste0(prefix, connector, file_name))
  }

  # Process directories recursively
  for (i in seq_along(dirs)) {
    sub_dir_name <- basename(dirs[i])
    connector <- ifelse(i == length(dirs), "└── ", "├── ")
    structure <- c(structure, paste0(prefix, connector, sub_dir_name, "/"))

    # Adjust prefix for recursive call
    sub_prefix <- ifelse(i == length(dirs), "    ", "│   ")
    sub_structure <- generate_tree_structure(dirs[i], paste0(prefix, sub_prefix),
                                             dirs_to_fully_list, sample_n)
    structure <- c(structure, sub_structure)
  }

  structure <- paste0(structure, collapse = "\n")
  # message(structure)

  return(structure)
}



