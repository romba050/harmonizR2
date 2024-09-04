#' Create folders for a harmonization project
#'
#' Creates folders for a harmonization project with a structure following the Maelstrom
#' Research harmonization standards. This is an optional feature that can be used
#' as a template for a harmonization project. The project can have versions or not.
#' A new version can be created by copying from an existing version.
#'
#' @param project A character string specifying the project name.
#' @param version A character string specifying the project version.
#' @param copy_from A character string specifying the existing project version to copy from.
#'
#' @return A project folder with eight subfolders and a README.Rmd file for documentation.
#' If a version is specified, a subfolder for each version is created in the project
#' folder, and the version folder contains the eight project subfolders and README file.
#' If the project already exists, it will not be overwritten, but any missing subfolders
#' will be created.
#'
#' @examples
#' \dontrun{
#' # use case 1: create folder without versioning
#' harmo_env_create(project = "TEST_1")
#'
#' # use case 2: create folder with a versioning
#' harmo_env_create(
#'   project = "TEST_2",
#'   version = "version_1")
#'
#' # use case 3: copy a folder in the same project
#' # (e.g., the folder “version 2” will be created as a copy of the folder “version_1”)
#'
#' harmo_env_create(
#'   project = "TEST_2",
#'   version = "version_2" ,
#'   copy_from = "version_1")
#' }
#'
#' @import dplyr fs stringr
#' @export
harmo_env_create <- function(project, version = NA_character_, copy_from =  NA_character_){

  temp.project_list <- basename(list.dirs(recursive = FALSE))
  temp.version_list <- c("no_version_provided", "",basename(list.dirs(paste0(project),recursive = FALSE)))

  test_project <- if(str_sub(project,1,1) %in% c("0","1","2","3","4","5","6","7","8","9","_")){paste0("X",project)}else{project}
  if(is.na(version)) {version <- "no_version_provided"}
  test_version <- if(str_sub(version,1,1) %in% c("0","1","2","3","4","5","6","7","8","9","_")){paste0("X",version)}else{version}

  if(project %in% temp.project_list & version %in% temp.version_list){
    message(
"This project/version combination already exists.\n",
"Please provide a different name")
  }else{
     if(make.names(test_project) != test_project){
    message(
"This project name is unauthorized (space or non alpha numeric characters).\n",
"Please provide a different name")
  }else{
     if(make.names(test_version) != test_version){
   message(
"This version name already exists or is unauthorized (space or non alpha numeric characters).\n",
"Please provide a different name")
  }else{
    temp.version <- version


  if(temp.version == "no_version_provided") {temp.version <- ""}

  if(!is.na(copy_from) & copy_from %in% temp.version_list){

    if(copy_from == ""){
      dir_copy(
        path = paste0(project,"/"),
        new_path = paste0(project,"_temp/",temp.version))

      dir_delete(path = paste0(project,"/"))

      dir_copy(
        path = paste0(project,"_temp/"),
        new_path = paste0(project,"/"))

      dir_delete(path = paste0(project,"_temp/"))

      message("\nThe whole content of the project ",project, " has been placed in the version ",temp.version," in your environment\n")

      dir_tree(paste0(project,"/",temp.version))

    }else{
    dir_copy(
      path = paste0(project,"/",copy_from),
      new_path = paste0(project,"/",temp.version))
      message("\nThe version ",temp.version, " has been created from ",copy_from," in your environment\n")

    dir_tree(paste0(project,"/",temp.version))
    }

  }else{

  dir_create(paste0(project,"/",temp.version,""))
  dir_create(paste0(project,"/",temp.version,"/study_specific_datasets"))
  dir_create(paste0(project,"/",temp.version,"/dataschema"))
  dir_create(paste0(project,"/",temp.version,"/data_processing_elements"))
  dir_create(paste0(project,"/",temp.version,"/data_processing_elements/additional_scripts"))
  dir_create(paste0(project,"/",temp.version,"/harmonized_datasets"))
  dir_create(paste0(project,"/",temp.version,"/reports"))
  dir_create(paste0(project,"/",temp.version,"/other_docs"))
  file_create(paste0(project,"/",temp.version,"/README.Rmd"))

  message(paste0("Creation of the folder '",temp.version," in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/study_specific_datasets' in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/dataschema' in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/data_processing_elements' in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/data_processing_elements/additional_scripts' in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/harmonized_datasets' in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/reports' in ", basename(project)))
  message(paste0("Creation of the folder '",temp.version,"/other_docs' in ", basename(project)))
  message(paste0("Creation of the file '",temp.version,"/READ_me.Rmd' in ", basename(project)))

  dir_tree(paste0(project,"/",temp.version))

  if(!is.na(copy_from)){
    message("\nThe version ",copy_from, " you want to copy your version from does not exists.\n",
            "The version ",temp.version, " has been created as a new one.\n")
  }
 }
    }}}
}


