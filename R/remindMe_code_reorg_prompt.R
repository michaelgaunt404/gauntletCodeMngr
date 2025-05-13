#' Print basic LLM prompt for to reorganize code.
#'
#' @return None (prints to console).
#'
#' @export
#' @examples
#' \dontrun{
#' remindMe_packages_to_use()
#'
#' }
remindMe_code_reorg_prompt = function(){
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Printing basic LLM prompt for function docummentaiton{gauntlet::strg_make_space_2(last = F)}"))

  cat('
Can you please take the code that provide to you and return it to me using the template below.
It needs to be reorganized and annotated using the template styling.
Please change section headers as well with appropriate short desc/names.

!! This is the template that I would like you to follow:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# DESC: This is script [[insert brief readme here]]
#
# AUTHOR: Name, email@wsp.com
#
# STATUS: Working (Working/Deprecated)
#
# README: [[insert brief readme here]]
# README: --[[insert brief readme here]]
# README: --[[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define all packages required to run script here

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define path(s) here in the in this section

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#source or define functions required to run script here

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#source all data required to run scrip here

#main header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1.1================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 1.2================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

!! End of the template.

!! This is the code:

[[PLACE CODE HERE]]


!! This is end of the code:

!! An a description of the code:

[[PLACE CODE DESCRIPTION HERE]]


'
  )
}

