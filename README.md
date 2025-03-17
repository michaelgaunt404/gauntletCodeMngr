# gauntletCodeMngr

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The `gauntletCodeMngr` package is a collection of utility functions designed to help **organize, manage, and document R codebases**, whether for package development or standalone projects.

This package simplifies **Roxygen documentation**, **function mapping**, **dependency tracking**, and **directory structuring**, making it an essential toolkit for maintaining clean, well-documented, and structured R projects.

#### Intent of this package:

The goal of `gauntletCodeMngr` is to **reduce manual work and enforce structure** in R projects. It automates common tasks related to **function documentation, dependency management, and project organization**, making codebases more maintainable and easier to navigate.

#### What you can do with this package:

:heavy_check_mark: **Automatically add `@importFrom` statements** based on script dependencies\
:heavy_check_mark: **Convert unqualified function calls** to fully qualified `package::function()` format\
:heavy_check_mark: **Ensure Roxygen `@examples` sections** are properly wrapped in `dontrun{}`\
:heavy_check_mark: **Check if script names match function names** to maintain consistency\
:heavy_check_mark: **Extract dependencies from a `NAMESPACE` file** and categorize them into `Imports` or `Suggests`\
:heavy_check_mark: **Generate a directory tree representation** of an R project\
:heavy_check_mark: **Map function calls across scripts** to visualize dependencies

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools") 
devtools::install_github("your-github-username/gauntletCodeMngr")

library(gauntletCodeMngr)

# Example: Add @importFrom statements to an R script
add_importFrom_fnctn_statments("path/to/script.R")

# Example: Convert function calls to fully qualified package::function() format
add_pkg_fnctn_definitions("path/to/script.R")

# Example: Ensure dontrun{} is wrapped around @examples in all R scripts
add_roxygen_dontrun(folder = "R")

# Example: Check if script names match function names
check_scrpt_fnctn_names("R")

# Example: Extract Imports and Suggests from a NAMESPACE file
extract_namespace_dependencies("NAMESPACE")

# Example: Generate a tree structure of the project directory
generate_dir_tree_structure("path/to/project")

# Example: Map function relationships across scripts
map_functions_and_scripts(function_directory = "R", code_directory = "code")
```

### **Key Improvements:**

1.  **More accurate description of package purpose:**
    -   Focused on **documentation automation**, **dependency management**, and **project organization**.
2.  **Clearly categorized functions:**
    -   **Documentation automation:** `add_importFrom_fnctn_statments()`, `add_pkg_fnctn_definitions()`, `add_roxygen_dontrun()`
    -   **Project consistency checks:** `check_scrpt_fnctn_names()`, `extract_namespace_dependencies()`
    -   **Directory and function mapping tools:** `generate_dir_tree_structure()`, `map_functions_and_scripts()`
3.  **Installation and usage examples:**
    -   Quick-start guide with core functions.

Let me know if you’d like any refinements! 🚀
