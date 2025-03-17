#' Extract Required and Suggested Packages from NAMESPACE
#'
#' This function reads a `NAMESPACE` file from an R package directory, identifies all
#' `importFrom` and `import` statements, and returns a categorized list of
#' package dependencies, recommending which should be added to `Imports` or `Suggests`.
#'
#' @param namespace_path Character. Path to the `NAMESPACE` file. Defaults to `"NAMESPACE"` in the current directory.
#' @param suggest_threshold Integer. If a package appears in fewer than this number of occurrences,
#'        it is categorized as `Suggests`. Defaults to `1`.
#'
#' @return A list with two character vectors: `"Imports"` (required dependencies) and `"Suggests"` (optional dependencies).
#'
#' @importFrom stringr str_extract_all str_remove_all
#' @importFrom utils readLines
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run in a package directory where NAMESPACE exists
#' analyze_namespace_dependencies("path/to/package/NAMESPACE")
#' }
extract_namespace_dependencies <- function(namespace_path = "NAMESPACE", suggest_threshold = 1) {

  if (!file.exists(namespace_path)) {
    stop("NAMESPACE file not found at: ", namespace_path)
  }

  # Read NAMESPACE file
  namespace_lines <- readLines(namespace_path)

  # Extract package names from importFrom(pkg, function) and import(pkg)
  imports <- unlist(stringr::str_extract_all(namespace_lines, "(?<=importFrom\\()[^,]+"))
  full_imports <- unlist(stringr::str_extract_all(namespace_lines, "(?<=import\\()[^)]+"))

  # Clean up whitespace
  imports <- stringr::str_remove_all(imports, "\\s+")
  full_imports <- stringr::str_remove_all(full_imports, "\\s+")

  # Combine all package names
  all_packages <- c(imports, full_imports)

  # Count occurrences
  package_counts <- table(all_packages)

  # Categorize packages
  required_packages <- names(package_counts[package_counts > suggest_threshold])
  suggested_packages <- names(package_counts[package_counts <= suggest_threshold])

  return(list(
    Imports = required_packages,
    Suggests = suggested_packages
  ))
}
