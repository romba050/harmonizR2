#' Create an index of files in a folder
#'
#' Creates a tibble listing files in a specified folder (recursively) with file
#' path name and other useful metadata. This index can be used to quickly find
#' files in the environment. The index also generates script to read files as
#' R objects into the environment. Names for R objects are generated automatically
#' from file names (R objects are not created at this step but the command line is
#' generated and stored in the column to_eval, ready to be evaluated and generate
#' R objects).
#'
#' The user must make sure their files are in the folder to be indexed.
#'
#' @param folder A character string identifying the folder to index. If not specified,
#' the current folder is the default
#' @param pattern A character string defining a pattern to sub-select within folder.
#' Can be useful for excluding certain folders from indexing (matching by regex is supported).
#' @param negate If TRUE, return non-matching elements.
#'
#' @return A tibble with folder_path, file_path, file_name, extension, file_type
#' columns and a last column to_eval which is R code in a character vector to read
#' the file into the environment.
#'
#' @examples
#' \dontrun{
#' # use case 1: create index of files in one folder
#' index_DEMO <- file_index_create(folder = "TEST_demo")
#'
#' # use case 2: create index of files in one folder excluding "study_specific_datasets" subfolder
#' index_DEMO <-
#'   file_index_create(folder = "TEST_demo", pattern = "study_specific_datasets", negate = TRUE)
#'
#' }
#'
#' @import tidyr dplyr tools rlang stringr utils fs
#' @export
file_index_create <- function(folder = getwd(), pattern = "^", negate = FALSE){

  message(
    "Your files contained in your R environnement are currently being indexed. Please wait...\n")

  index <- tibble(
    folder_path = file_path_as_absolute(folder),
    file_path  = NA_character_,
    file_name  = NA_character_,
    extension = NA_character_,
    file_type = NA_character_,
    to_eval = NA_character_)

  all_files_list <-
    list.files(folder, full.names = TRUE, recursive = TRUE) %>%
    str_subset(.,pattern = pattern,negate = negate)

  if(is_empty(all_files_list)){
    message("Your folder is empty or do not exists.")
  }else{

    for(i in all_files_list){
      index <-
        index %>%
        add_row(
          file_path = file_path_as_absolute(i)) %>%
        fill(folder_path,.direction = "down")}

    index <-
      index %>% filter(!is.na(file_path)) %>%
      mutate(
        file_name = paste0(basename(file_path)),
        extension = file_ext(file_path),
        extension = ifelse(nchar(extension) == 0,file_name,extension),
        to_eval = case_when(
          extension == "RData"     ~ paste0("load('",file_path,"',envir = .GlobalEnv)"),
          extension == "spss"      ~ paste0("haven::read_spss('",file_path,"')"),
          extension == "sav"       ~ paste0("haven::read_spss('",file_path,"')"),
          extension == "dta"       ~ paste0("haven::read_dta('",file_path,"')"),
          extension == "sas7bdat"  ~ paste0("haven::read_sas('",file_path,"')"),
          extension == "xlsx"      ~ paste0("read_excel_allsheets('",file_path,"')"),
          extension == "csv"       ~ paste0("suppressMessages(read_csv_any_formats('",file_path,"'))"),
          extension == "R"         ~ paste0("source('",file_path,"')"),
          extension == "html"      ~ paste0("utils::browseURL('",file_path,"')"),
          TRUE                     ~ paste0("usethis::edit_file('",file_path,"')")),
        file_type = case_when(
          extension %in%c("R","r")                            ~ "R script",
          extension %in%c("Rmd","md")                         ~ "Markdown file",
          extension == "html"                                 ~ "html page",
          extension == "RData"                                ~ "RData file",
          extension == "sav"                                  ~ "excel-like file",
          extension == "dta"                                  ~ "excel-like file",
          extension == "sas7bdat"                             ~ "excel-like file",
          extension == "xlsx"                                 ~ "excel-like file",
          extension == "csv"                                  ~ "excel-like file",
          extension %in%c("png","jpg", "JPG", "jpeg", "JPEG") ~ "image",

          TRUE                     ~ "other file")) %>%
      distinct

    # visualization of the dir tree
    temporary_folder <- path_temp() %>% basename
    dir_create(temporary_folder)
    folder_tp <- str_remove(string = index$file_path,pattern = dirname(index$folder_path))
    for(i in folder_tp){
      dir_create(paste0(temporary_folder,"/",dirname(i)))
      file_create(paste0(temporary_folder,"/",i))}
    dir_tree(paste0(temporary_folder))
    dir_delete(temporary_folder)
    message("\n\nYour files have been indexed.\n")

    return(index)

  }
}

#' Search an index of files
#'
#' Searches in mlstr_file_index R object (tibble) based on pattern and other query
#' options and provides a table where all the files in a specified folder and
#' corresponding to the query are listed (recursively). If no index tibble is
#' provided, the function creates one from the working directory.
#'
#' @param index The index (tibble) of a folder with file locations and metadata,
#' either previously generated by file_index_create() or created from folder.
#' @param file_path A character string specifying a file path to search by.
#' Can be the full string or substring (matching by regex is supported)
#' @param file_name A character string a file name to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param extension A character string a file extention to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param file_type A character string a file type to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param .fs_tree If TRUE, return the file tree of the query.
#'
#' @return A tibble with indexed information for files matching the query.
#'
#' @examples
#' \dontrun{
#' # use case 1: create index of files in one folder
#' index_DEMO <- file_index_create(folder = "TEST_demo")
#'
#' # use case 3: subset index matching a specific file_path query
#' file_index_search(index = index_DEMO, file_path = "data_processing_elements")
#'
#' # use case 4: subset index matching a specific file_type query
#' file_index_search(index = index_DEMO, file_name = "study_MELBOURNE_1")
#'
#' }
#'
#' @import dplyr rlang fs
#' @export
file_index_search <- function(index = tibble(), file_path = "^", file_name = "^", extension = "^", file_type = "^", .fs_tree = TRUE){

  index      <- if(is_empty(index)) {
    file_index_create(folder = getwd())
    }else{index}
  temp.file_path  <- file_path
  temp.file_name  <- file_name
  temp.extension  <- extension
  temp.file_type  <- file_type

  index <-
    index %>%
    filter(str_detect(string = file_path,  pattern = temp.file_path)) %>%
    filter(str_detect(string = file_name,  pattern = temp.file_name))  %>%
    filter(str_detect(string = file_type,  pattern = temp.file_type))  %>%
    filter(str_detect(string = extension,  pattern = temp.extension))

  if(.fs_tree == TRUE){
    # visualization of the dir tree
    temporary_folder <- path_temp() %>% basename
    dir_create(temporary_folder)
    folder_tp <- str_remove(string = index$file_path,pattern = dirname(index$folder_path))
    for(i in folder_tp){
      dir_create(paste0(temporary_folder,"/",dirname(i)))
      file_create(paste0(temporary_folder,"/",i))}
    dir_tree(paste0(temporary_folder))
    dir_delete(temporary_folder)
  }

  return(index)

}

#' Read, source and open objects from an index of files
#'
#' Reads all files from an mlstr_file_index tibble as R objects to generate in the
#' environment or R scripts to be sourced. Any other file types will be opened in
#' browser (html files) or in environnement. If no index tibble is provided, the
#' function creates one from the working directory.
#'
#' @param index The index (tibble) of a folder with file locations and metadata,
#' either previously generated by file_index_create() or created from folder.
#' @param file_path A character string specifying a file path to search by.
#' Can be the full string or substring (matching by regex is supported)
#' @param file_name A character string a file name to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param extension A character string a file extention to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param file_type A character string a file type to search by.
#' Can be the full string or substring (matching by regex is supported).
#' @param assign If TRUE, the name is automatically assigned from the name
#' of the object read.
#' @param .envir The environment to use. parent.frame() by default
#'
#' Can be the full string or substring (matching by regex is supported).
#'
#' @return R objects generated in the environment or R scripts. R object names are
#' created automatically from their file names. Otherwise return messages indicating
#' what objects were created, or files opened, and if any troubles occured
#'
#' @examples
#' \dontrun{
#' # use case 1: create index of files in one folder
#' index_DEMO <- file_index_create(folder = "TEST_demo")
#'
#' # use case 5: read a file indexed matching a specific file_name query (assign = FALSE)
#' file_index_read(index = index_DEMO, file_name = "study_MELBOURNE_1",assign = FALSE)
#'
#' # use case 6: read and download files indexed matching a specific file_name query (assign = TRUE)
#' file_index_read(index = index_DEMO ,file_name = "study_MELBOURNE_1",assign = TRUE)
#' }
#'
#' @import dplyr
#' @export
file_index_read <- function(
  index = tibble(),
  file_path = "^",
  file_name = "^",
  extension = "^",
  file_type = "^",
  assign = TRUE,
  .envir = parent.frame()){

  index <-
    file_index_search(
      index = index,
      file_path = file_path,
      file_name = file_name,
      extension = extension,
      file_type = file_type,
      .fs_tree = FALSE) %>%
    filter(!is.na(to_eval))

  if(nrow(index) > 0){

    for(i in 1:nrow(index)){

      if(str_detect(index$to_eval[i], "^source\\('|^utils::browseURL\\('|^usethis::edit_file\\('|^load\\('")){
        index$to_eval[i] %>% parceval()
        message("the file: ",basename(index$file_path[i])," has been sourced, loaded or opened in your environnement")

     }else{

       if(assign == TRUE){

        try({assign(
          x = file_path_sans_ext(index$file_name[i]),
          value = index$to_eval[i] %>% parceval(),
          envir = .envir)
       message("the R object: ",index$file_name[i]," has been created")})

       }else{
         return(index$to_eval[i] %>% parceval())
      }
    }
  }

    }else{message("the file you try to read does not exists in the index")}
}
