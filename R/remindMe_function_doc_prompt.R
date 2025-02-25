#' Print basic LLM prompt for function docummnetation.
#'
#' @return None (prints to console).
#'
#' @export
#' @examples
#' \dontrun{
#' remindMe_packages_to_use()
#'
#' }
remindMe_function_doc_prompt = function(){
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Printing basic LLM prompt for function docummentaiton{gauntlet::strg_make_space_2(last = F)}"))

  cat('
Can you write me roxen documentation for the following R code please.
Include an export function so that the function can be properly exported to the package namespace.
Please include @importFrom statesments as well.
Please include @export statesment as well.
Please add space at the end for and @example with generic inputs and wrap it in a dontrun{}.
Description:

Here is function for context.
Do not print me back the entire function
Function:

'
  )
}

