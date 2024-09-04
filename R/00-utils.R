message_on_prompt <- function(...){
  invisible(readline(cat(prompt = paste(...))))
}

silently_run <- function(...){
  return(suppressWarnings(suppressMessages(try(...,silent = TRUE))))
}

#' @import dplyr
#' @export
parceval <- function(...){
  eval(parse(text = str_squish(...) %>% str_remove_all("\\\r")))
}

#' Read all Excel sheets using readxl::read_excel recursively
#'
#' The Excel file is read and the content is placed in a list of tibbles, with each
#' sheet in a separate element in the list. If the Excel file has only one sheet,
#' the output is a single tibble.
#'
#' @param filename A character string of the path of the Excel file.
#' @param sheets A vector containing only the sheets to be read.
#'
#' @return A list of tibbles corresponding to the sheets read, or a single tibble
#' if the number of sheets is one.
#'
#' @examples
#' \dontrun{
#' # Example 1: read all sheet of a data dictionary
#'
#' read_excel_allsheets("dd_TOKYO_format_maelstrom_tagged.xlsx")
#'
#' }
#'
#' @import readxl dplyr tibble rlang
#' @export
read_excel_allsheets <- function(filename, sheets = "") {

  if(toString(sheets) == ""){
    sheets_name <- excel_sheets(filename)
  }else{
    sheets_name <- excel_sheets(filename) %>% as_tibble %>% filter(value %in% c(sheets)) %>% pull(value)
    if(length(sheets_name) != length(sheets)){
      message("{",sheets[!(sheets %in% sheets_name)] %>% toString, "} sheet name(s) not found in the excel file")}}

  if(is_empty(sheets_name)){message("The sheet name(s) you provided do not exist")}else{
    x <- lapply(sheets_name,
                function(X) read_excel(
                  path      = filename,
                  sheet     = X,
                  guess_max = suppressWarnings(read_excel(filename,sheet = X) %>% nrow)))
    names(x) <- sheets_name
    if(length(x) == 1){return(x[[1]])}else{return(x)}
  }
}

#' Write all Excel sheets using xlsx::write.xlsx recursively
#'
#' The R objects are read and the content is placed in separated sheets.
#' This function is inspired by the function proposed in
#' https://statmethods.wordpress.com/2014/06/19/quickly-export-multiple-r-objects-to-an-excel-workbook/
#'
#' @param filename A character string of the path of the Excel file.
#' @param ... R objects, coma separated.
#'
#'
#' @examples
#' \dontrun{
#' # Example 1: read all sheet of a data dictionary
#'
#' write_excel_allsheets(filename = "DEMO/my_save/studies.xlsx", list(study_TOKYO, study_MELBOURNE_1))
#'
#' }
#'
#' @import writexl fs stringr
#' @export
write_excel_allsheets <- function (filename, list){

  objnames <- list %>% names
  fargs <- as.list(match.call(expand.dots = TRUE))

  if(is.null(objnames)) {
    objnames <-
      as.character(fargs[3]) %>%
      str_remove(.,"^list\\(") %>%
      str_remove(.,"\\)$") %>%
      str_split(", ") %>% unlist
    names(list) <- objnames}

  dir_create(dirname(filename))
  write_xlsx(x = list, path = filename)

  }

#' Read a csv file using readr::read_csv and avoid errors
#'
#' The csv file is read twice to detect the number of lines to use in attributing
#' the column type ('guess_max' parameter of read_csv). This avoids common errors
#' when reading csv files.
#'
#' @param csv_name A character string of the path of the csv file.
#'
#' @return a tibble corresponding to the csv read.
#'
#' @examples
#' \dontrun{
#' # Example 1: read all sheet of a data dictionary
#'
#' read_csv_any_formats("study_TOKYO.csv")
#'
#' }
#'
#' @import readr dplyr
#' @export
read_csv_any_formats <- function(csv_name){
  guess_max <- suppressMessages(suppressWarnings(read_csv(csv_name, progress = FALSE))) %>% nrow
  csv <- read_delim(file = csv_name, guess_max = guess_max)
  return(csv)
}

#' Call the help center for full documentation
#'
#' This function is a direct call of the documentation in the repository hosting the
#' package. The user accesses the description of the lastest version of the package, the
#' vignettes, and the list of the functions.
#'
#' @examples
#' \dontrun{
#' # Example 1: call the help center!
#'
#' harmonizR_help()
#'
#' }
#'
#' @import utils
#' @export
harmonizR_help <- function(){

  browseURL("https://maelstrom-research.github.io/harmonizRLegacy-documentation")

}

#' Download Maelstrom templates.
#'
#' This helper function downloads all templates from an Opal environment and writes them in a specified directory
#'
#' @param opal Opal login attributes (asked on prompt if empty)
#' @param to A character string of a path where the files will be placed to in Opal
#' (if not specified, the working directory is the default)
#'
#' @examples
#' \dontrun{
#' # Example 1: Download all templates amd write them in a specified directory
#'
#'o <- opal.login(username = 'administrator',
#'                password = 'password',
#'                url = 'https://opal-demo.maelstrom-research.org/')
#'2
#' mlstr_download_templates( opal = o , to = "DEMO")
#'
#' }
#'
#' @import utils opalr
#' @export
mlstr_download_templates <- function(
  opal = opal.login(
    url = getPass::getPass("Enter the url: "),
    username = getPass::getPass("Enter the username: "),
    password = getPass::getPass("Enter the password: ")),
  from,
  to = paste0(getwd())){

  opal_files_pull(opal,from = "/home/administrator/mlstr_templates",to)

  }

