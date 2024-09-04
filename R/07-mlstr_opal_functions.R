#' Create a project in an Opal environment
#'
#' These Opal functions are wrappers of functions present in opalr. For further
#' information, please refer to Opal documentation. https://www.obiba.org/opalr/index.html
#'
#' The user must be allowed to interact with their Opal
#'
#' @param opal Opal login attributes (asked on prompt if empty)
#' @param project A character string to name the project in Opal
#' @param tag A character string to provide a tag for the Opal project
#' @param ... Additional parameters. See opal.project_create()
#'
#' @return A project in an Opal environment. If the project already exists, it won't
#' it will remain as it is, and no new project is created. The errors provided are
#' associated to the handler or the read/write permissions to Opal.
#'
#' The user must have adequate credentials to interact with their Opal environment.
#'
#' @examples
#' \dontrun{
#'
#' # use case 1: create a project in Opal
#' opal_project_create(project = "DEMO" ,tag = "DEMO")
#'
#' }
#'
#' @import dplyr fs opalr getPass
#' @export
opal_project_create <- function(
  opal = opal.login(
    url = getPass::getPass("Enter the url: "),
    username = getPass::getPass("Enter the username: "),
    password = getPass::getPass("Enter the password: ")), project, tag = NULL,...){


  if(!is.null(tag)){tag <- as.list(tag)}
  for(i in project){

    if(opal.project_exists(opal = opal, project = basename(i))){
      message("The project ",i," already exists in Opal and will not be created here.")

    }else{

      opal.project_create(opal = opal, project = basename(i), database = TRUE, tags = tag,...)
      message("The project ",i," has been created in Opal")
    }
  }
}

#' Upload files to an Opal environment
#'
#' These Opal functions are wrappers of functions present in opalr. For further
#' information, please refer to Opal documentation. https://www.obiba.org/opalr/index.html
#'
#' The user must be allowed to interact with their Opal
#'
#' @param opal Opal login attributes (asked on prompt if empty)
#' @param from A character string of a path where the files will be taken from in R
#' @param to A character string of a path where the files will be placed to in Opal
#'
#' @return Folder(s) containing files coming from the user R environment in Opal.
#' The path to Opal needs to be pasted with Opal absolute path.
#'
#' @examples
#' \dontrun{
#'
#' # use case 1: place all files in a project ("home/project/") or a user ("home/administrator/")
#' opal_files_push(
#'   opal = o,
#'   from = "DEMO",
#'   to = "home/project/DEMO")
#'
#' }
#'
#' @import dplyr fs opalr getPass
#' @export
opal_files_push <- function(
  opal = opal.login(
    url = getPass::getPass("Enter the url: "),
    username = getPass::getPass("Enter the username: "),
    password = getPass::getPass("Enter the password: ")), from, to){

  opal.file_upload(opal = opal,source = from, destination = to)
  message("Your file(s) has(ve) been succesfully uploaded to Opal")
}

#' Upload a tibble from the R environment into an Opal environment as tables in an Opal project
#'
#' These Opal functions are wrappers of functions present in opalr. For further
#' information, please refer to Opal documentation. https://www.obiba.org/opalr/index.html
#'
#' The user must be allowed to interact with their Opal
#'
#' @param opal Opal login attributes (asked on prompt if empty)
#' @param project A character string to name the project in Opal
#' @param table tibble of the dataset
#' @param data_dict tibble of the data dictionary
#'
#' @return A table in Opal.
#'
#' @examples
#' \dontrun{
#'
#'
#' # use case: send to Opal tables with the data dictionary (not mandatory)
#' opal_tables_push(
#'   opal = o,
#'   project = "DEMO",
#'   table = study_PARIS,
#'   data_dict = dd_PARIS_format_maelstrom)
#'
#' }
#'
#' @import dplyr fs opalr getPass
#' @export
opal_tables_push <- function(
  opal = opal.login(
    url = getPass::getPass("Enter the url: "),
    username = getPass::getPass("Enter the username: "),
    password = getPass::getPass("Enter the password: ")),
  project,
  table,
  data_dict = NULL){

  temp.table_name <- if(class(table)[1] == "character") {table}else{as.character(substitute(table)) }
  tibble <- if(class(table)[1] == "character") { parceval(table) }else{ table }

  if(!is.null(data_dict)){
    tibble <-
      dictionary.apply(
        tibble = tibble,
        variables = data_dict$Variables,
        categories = data_dict$Categories)}

  if(!opal.table_exists(opal, project,temp.table_name)){
    opal.table_create(
      opal       = opal,
      project    = project,
      table      = temp.table_name)}

    opal.table_save(
      opal       = opal,
      tibble     = tibble,
      project    = project,
      table      = temp.table_name,
      overwrite = TRUE,
      force = TRUE,
      id.name = tibble %>% select(1) %>% names)

  message("\nThe table:",temp.table_name, " has been successfuly uploaded to Opal")
}


#' Download files from an Opal environment into the R environment
#'
#' These Opal functions are wrappers of functions present in opalr. For further
#' information, please refer to Opal documentation. https://www.obiba.org/opalr/index.html
#'
#' The user must be allowed to interact with their Opal
#'
#' @param opal Opal login attributes (asked on prompt if this field is empty)
#' @param from A character string of a path where the files will be taken from in R
#' @param to A character string of a path where the files will be placed to in Opal
#'
#' @return Folder(s) containing files coming from Opal in user R environment.
#'
#' @examples
#' \dontrun{
#'
#' # use case 1: download all files from a project folder ("home/project/") or a user's folder ("home/administrator/").
#' opal_files_pull(
#'   opal = o,
#'   from = "/home/administrator/DEMO/data_processing_elements",
#'   to = "DEMO")
#'
#' # use case 2: download specific file from an Opal folder and rename it.
#' opal_files_pull(
#'   opal = o,
#'   from = "/home/administrator/DEMO/dataschema/DEMO_dataschema.xlsx",
#'   to = "DEMO/dataschema/MY_dataschema.xlsx")
#'
#' }
#'
#' @import dplyr fs opalr getPass tools utils stringr
#' @export
opal_files_pull <- function(

  opal = opal.login(
    url = getPass::getPass("Enter the url: "),
    username = getPass::getPass("Enter the username: "),
    password = getPass::getPass("Enter the password: ")),
  from,
  to = paste0(getwd(),"/opal_files")){

  # if from = ".../.../foo"     and to = ".../.../fuu"      <- zip the folder, dl and unzip
  # if from = ".../.../foo.ext" and to = ".../.../fuu"      <- to <- to + "/" + basename(from), dl
  # if from = ".../.../foo.ext" and to = ".../.../fuu.ext"  <- dl

  to <- ifelse(file_ext(from) == file_ext(to), to, paste0(to,"/",basename(from)))
  to <- ifelse(file_ext(to) == "",paste0(to,".zip"),to)
  to <- str_replace(to,"/.zip",".zip")

  if(from == ""){
    message("\nYou must provide an Opal files path\n")}else{

      opal.file_download(
        opal = opal,
        source = from,
        destination = to)

      if(file_ext(to) == "zip"){
        unzip(zipfile = to, exdir = file_path_sans_ext(to), overwrite = TRUE,junkpaths = FALSE)
        file.remove(paste0(to))
        message(
"The files have been added to your environment in the folder ",file_path_sans_ext(to),".\n")
      }else{
message("The file ",basename(to)," have been added to your environment in the folder ",dirname(to),".\n")
    }
    }
}

#' Download tables and data dictionaries from an Opal environment into the R environment
#'
#' These Opal functions are wrappers of functions present in opalr. For further
#' information, please refer to Opal documentation. https://www.obiba.org/opalr/index.html
#'
#' The user must be allowed to interact with their Opal
#'
#' @param opal Opal login attributes (asked on prompt if empty)
#' @param project A character string to name the project in Opal
#' @param table A character string of the name the table
#' @param .envir The environment to use. parent.frame() by default
#'
#' @return R objets (tibbles and list of tibbles) representing tables and their
#' respective data dictionary.
#'
#' @examples
#' \dontrun{
#'
#' # use case 1: download a table and its data dictionary associated
#' opal_tables_pull(opal = o,project = "DEMO", table = "study_PARIS")
#'
#' # use case 2: download all tables and their data dictionaries associated
#' opal_tables_pull(opal = o,project = "DEMO")
#'
#' }
#'
#' @import dplyr fs opalr getPass
#' @export
opal_tables_pull <- function(

  opal = opal.login(
    url = getPass::getPass("Enter the url: "),
    username = getPass::getPass("Enter the username: "),
    password = getPass::getPass("Enter the password: ")),
  project, table = c(),
  .envir = parent.frame()){

  project <- if(project == ""){
    message("\nYou must provide an Opal project\n")}else{

      if(is.null(table)){
        table <- opal.tables(opal = opal,datasource = project) %>% tibble() %>% pull(name)
      }

      message_on_prompt(
        table %>% toString() %>% str_replace_all(",","\n") %>% cat,
        "\n\nfrom",project,"will be download in your environment\n",
        "Press [enter] to continue")

      for(i in table){
        message("Loading of table ",i)
        assign(
          x = paste0(i),
          value = opal.table_get(
            opal = opal,
            project = project,
            table = i),
          envir = .envir)
      }

      for(i in table){
        message("Loading of data dictionary data_dict_",i)
        assign(
          x     = paste0("data_dict_",i),
          value = lapply(
            X       = opal.table_dictionary_get(
              opal    = opal,
              project = project,
              table   = i),
            FUN = function(x) as_tibble(x)),
          envir = .envir)
      }

  message("The tables have been added to your environment.\n")

    }
}


