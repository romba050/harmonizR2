#' Generate harmonized dataset(s) and annotated data processing elements file
#'
#' Reads DataSchema and data processing elements R objects to generate harmonized dataset(s)
#' and annotated data processing elements file with harmonization status and errors.
#' The function uses DataSchema and data processing elements specifications
#' to process input study-specific variables into study harmonized variables.
#' Documentation of each data processing action is generated in the console to support
#' the identification of errors and correction of the data processing elements file
#' and objects, as needed. An annotated data processing elements file is also produced
#' providing harmonization status (complete/impossible) for each DataSchema variable
#' in each study-specific dataset, which can be used to create a summary of harmonization
#' potential of DataSchema variables across study-specific datasets.
#'
#' Must provide data processing elements, DataSchema, and study-specific datasets
#' in the environment.
#' The DataSchema is the core list of variables to be generate using study-specific
#' variable and must be in Maelstrom Research data dictionary format (with "Variables" and
#' "Categories” tibbles and standard descriptors for each). The data processing
#' elements file contains the harmonization potential and data processing instructions
#' for the harmonization project to generate harmonized datasets, in the Maelstrom
#' Research format.
#' If the harmonization process contains errors, the tibbles and csv files will not
#' be generated to avoid any version confusion. The user can correct their data
#' processing element file and re-run the process.
#' If the user identified errors and want to correct them later, they can specify
#' 'undetermined' in the column 'harmo_rule' of their problematic data processing
#' element(s). Such data processing element will not be processed.
#'
#' @param dataschema A character string or list of two tibbles (Variables and Categories)
#' identifying the DataSchema.
#' @param data_proc_elem A character string or tibble R object identifying the data
#' processing elements file.
#' @param output_harmo_folder A character string specifying the folder path where
#' the output harmonized dataset(s) and annotated data processing elements files
#' will be saved.
#' @param name_report The name of the tibble to be generated of the annotated data
#' processing elements.
#' @param .envir The environment to use. parent.frame() by default
#'
#' @return A tibble and a csv file for each study-specific dataset that has been
#' harmonized saved in the specified output folder.
#'
#' @examples
#' \dontrun{
#' # Read the files
#' index_DEMO <- file_index_create(folder = "DEMO")
#' file_index_read(index_DEMO, file_path = "study_specific_datasets")
#' file_index_read(index_DEMO, file_path = "dataschema")
#' file_index_read(index_DEMO, file_path = "data_processing_elements")
#'
#' # Example 1: all the process of harmonization with data processing elements with errors
#' harmo_process(
#'   dataschema = DEMO_dataschema,
#'   data_proc_elem = `DEMO_data_processing_elements - with error`,
#'   output_harmo_folder = "DEMO/harmonized_datasets")
#'
#' # The user can use the function harmo_summary() to access the errors. In the
#' # environment, the user will find a report attached to harmonization.
#' harmo_summary(annotated_data_proc_elem)
#' rm(annotated_data_proc_elem)
#'
#' # After correction, re-run all the process of harmonization with the corrected
#' # data processing elements.
#' harmo_process(
#'   dataschema = DEMO_dataschema,
#'   data_proc_elem = `DEMO_data_processing_elements - final`,
#'   name_report = "final_report",
#'   output_harmo_folder = "DEMO/harmonized_datasets")
#' }
#'
#' @import dplyr readr xlsx fs
#' @export
harmo_process <- function(
  dataschema,
  data_proc_elem,
  output_harmo_folder = "harmonized_datasets",
  name_report = "annotated_data_proc_elem",
  .envir = parent.frame()){

  message_on_prompt(
"Please make sure that the study-specific dataset(s) exist in your R environment,
and your data processing elements too.\n",
    "Press [enter] to continue")

  if(class(data_proc_elem)[1] == "list") {
      test_data_proc_elem = 0
    for(i in 1:length(data_proc_elem)){
      test_data_proc_elem <- ifelse(
       all(c("index",
             "dataschema_table",
             "dataschema_variable",
             "study","study_table",
             "study_variables",
             "rule_category",
             "harmo_rule") %in%
        (data_proc_elem[[i]] %>% names)),i,test_data_proc_elem)
    }
      data_proc_elem <- data_proc_elem[[test_data_proc_elem]]
  }

  try({

    temp.process_rule <- rename_process_rule_columns(data_proc_elem)
    temp.input_tables <- as.list(temp.process_rule %>% select(input_table) %>% unique %>% pull)

    for(i in (1:length(temp.input_tables))){
      names(temp.input_tables)[[i]] <- temp.input_tables[[i]]

      temp.input_tables_test <- try(get(x = temp.input_tables[[i]], envir = .envir),silent = TRUE)

      if(class(temp.input_tables_test)[1] == "try-error"){
        temp.input_tables[[i]] <- tibble()
      }else{
        temp.input_tables[[i]] <- get(x = temp.input_tables[[i]], envir = .envir)
      }
    }

    temp.harmonized_tables <-
      harmo_parse_process_rule(
        process_rule = temp.process_rule,
        input_table_list = temp.input_tables,
        .envir = .envir)

    temp.process_rule <-
      temp.harmonized_tables$process_rule %>%
      rename(
        dataschema_table        = output_table ,
        dataschema_variable     = output_variable,
        study_table             = input_table,
        study_variables         = input_variable,
        harmo_rule              = script) %>%
      select(-type_of) %>%
      mutate(
        dataschema_table        = str_remove(dataschema_table,"temp.output_tables\\$"),
        study_table             = str_remove(study_table,"temp.input_tables\\$"),
        process_script          = str_remove(process_script,"temp.input_tables\\$"),
        process_script          = str_remove(process_script,"temp.output_tables\\$"))

    temp.harmonized_tables$process_rule <- NULL

    # # if pooled dataset has been created
    # if(!is.na(pooled_harmo_table_name)){
    #   temp.pooled_harmo_table <- bind_rows(temp.harmonized_tables)
    #   temp.harmonized_tables[length(temp.harmonized_tables)+1] <- list(temp.pooled_harmo_table)
    #   names(temp.harmonized_tables)[[length(temp.harmonized_tables)]] <- pooled_harmo_table_name
    # }

    # place the report in the environment
    assign(
      x = name_report,
      value = temp.process_rule,
      envir = .envir)
    message("\nThe tibble: ",name_report," has been created in your environment.\n\n")
    harmo_summary(temp.process_rule)

    if(temp.process_rule %>% filter(str_detect(process_status, "Error")) %>% nrow() > 0){

message(
"\n
--------------------------------------------------------------------------------\n
Your harmonization process contains errors. The tibbles and csv files have not
been generated to avoid any version confusion. Correct your data processing element
file and re-run the process.

[Suggestion]: If you identified errors and want to correct them later, you can specify
'undetermined' in the column 'harmo_rule' of your problematic data processing element(s).
Such data processing element will not be processed.\n")

    }else{

     message(
"\n
- CREATION OF HARMONIZED DATASETS: ---------------------------------------------\n")
      message(
"- Creation of study-specific harmonized datasets (tibbles): --------------------\n")

      for(i in (1:length(temp.harmonized_tables))){
        assign(
          x = names(temp.harmonized_tables[i]),
          value = temp.harmonized_tables[[i]],
          envir = .envir)
        message("The tibble: ",names(temp.harmonized_tables[i])," has been created.")

      }


    message(
"\n
- Creation of study-specific harmonized datasets (csv): ------------------------\n")

    # write CSVs one by one
    output_harmo_folder <- paste0(output_harmo_folder,"/") %>% str_replace_all(.,"//","/")
    dir_create(output_harmo_folder)
    temp.iter_name <- 1

    for(i in temp.harmonized_tables){
      temp.name <- names(temp.harmonized_tables) %>% .[temp.iter_name]

      write_csv(
        x = i %>% as.data.frame,
        file = paste0(output_harmo_folder,temp.name,".csv"),
        na = "")

      message("The file: ",basename(paste0(output_harmo_folder,temp.name,".csv"))," has been created")

      temp.iter_name <- temp.iter_name + 1
    }

    message(
"\n
- Creation of study-specific harmonized data dictionaries (tibble list): ------\n")

    # write CSVs one by one
    temp.iter_name <- 1

    for(i in temp.harmonized_tables){
      temp.name <- names(temp.harmonized_tables) %>% .[temp.iter_name]

      specific_data_proc_elem <- temp.process_rule %>%
        filter(dataschema_table == temp.name) %>%
        select(table = dataschema_table,
               name = dataschema_variable,
               rule_category,
               process_status)

      data_dict <- list()
      data_dict$Variables <-
        dataschema$Variables %>%
        select(-matches("^table$")) %>%
        left_join(specific_data_proc_elem, by = "name") %>%
        select_at(.vars = c(dataschema$Variables %>% names,specific_data_proc_elem %>% names))

      data_dict$Categories <-
        dataschema$Categories %>%
        select(-matches("^table$")) %>%
        left_join(specific_data_proc_elem %>% select(table, variable = name), by = "variable") %>%
        select_at(.vars = c(dataschema$Categories %>% names))

      assign(
        x = paste0("data_dict_",temp.name),
        value = data_dict,
        envir = .envir)
      message("The tibble list: dd_",temp.name," has been created.")
      temp.iter_name <- temp.iter_name + 1
    }


    message(
"\n
- Creation of study-specific harmonized data dictionaries (xlsx): --------------\n")

    # write CSVs one by one
    temp.iter_name <- 1

    for(i in temp.harmonized_tables){
      temp.name <- names(temp.harmonized_tables) %>% .[temp.iter_name]

      specific_data_proc_elem <- temp.process_rule %>%
        filter(dataschema_table == temp.name) %>%
        select(table = dataschema_table,
               name = dataschema_variable,
               rule_category,
               process_status)

      data_dict <- list()
      data_dict$Variables <-
        dataschema$Variables %>%
        select(-matches("^table$")) %>%
        left_join(specific_data_proc_elem, by = "name") %>%
        select_at(.vars = c(dataschema$Variables %>% names,specific_data_proc_elem %>% names))

      data_dict$Categories <-
        dataschema$Categories %>%
        select(-matches("^table$")) %>%
        left_join(specific_data_proc_elem %>% select(table, variable = name), by = "variable") %>%
        select_at(.vars = c(dataschema$Categories %>% names))

      write.xlsx2(
        x = data_dict$Variables,
        file = paste0(output_harmo_folder,"data_dict_",temp.name,".xlsx"),
        row.names = TRUE, showNA = FALSE,append = FALSE,
        sheetName="Variables")

      # write.xlsx2(
      #   x = data_dict$Categories,
      #   file = paste0(output_harmo_folder,"data_dict_",temp.name,".xlsx"),
      #   row.names = TRUE, showNA = FALSE,append = TRUE,
      #   sheetName="Categories")

      message("The file: data_dict_",basename(paste0(output_harmo_folder,temp.name,".xlsx"))," has been created")

      temp.iter_name <- temp.iter_name + 1
    }


    message("\n\nYour files have been created in the folder ", output_harmo_folder)
    message("\n\nYour harmonization is done. Please check if everything worked correctly.\n")
#
#         write.xlsx2(
#           x = temp.process_rule %>% as.data.frame,
#           file = paste0(output_harmo_folder,name_report,".xlsx"),
#           row.names = TRUE, showNA = FALSE,append = FALSE,
#           sheetName="harmo")
#         message("the file: ",basename(paste0(output_harmo_folder,name_report,".xlsx"))," has been created")
#
#
#


    }

  })
}

#' @import dplyr
harmo_process_id_creation <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = paste0("paste0('",study,"','_',",input_variable,")"),
      to_eval_test          =   paste0(

        input_table," %>% select(",input_variable,") %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,")"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>% parceval)
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_direct_mapping <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = input_variable,
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_operation <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    # temp.process_rule %>% slice(4) %>%
    mutate(
      replacement      = script %>% str_squish(),
      replacement      = replacement %>% str_replace_all(.,"\\$",paste0(input_table,"$")),
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }


}

#' @import stringr dplyr
harmo_process_recode <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    # temp.process_rule %>% slice(11) %>%
    mutate(
      replacement      = script,
      replacement      = gsub("recode\\("," , '",replacement),
      replacement      = gsub("ELSE","else ",replacement),
      replacement      = gsub(")","')",replacement),
      replacement      = paste0("car::recode(\n      var = ",input_table,"$", input_variable,replacement),
      replacement      = str_replace_all(replacement,"fun::",""),
      replacement      = str_replace_all(replacement,",",",\n      recodes = "),
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }
}

#' @import dplyr stringr
harmo_process_case_when <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    # temp.process_rule %>% slice(14) %>%
    mutate(
      replacement      = script %>% str_squish(),
      replacement      = str_replace_all(replacement,"case_when\\(","case_when(\n     "),
      replacement      = str_replace_all(replacement,";",",\n     "),
      replacement      = str_replace_all(replacement,"ELSE","TRUE"),
      replacement      = str_replace_all(replacement,"\\$",paste0(input_table,"$")),
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_paste <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = script,
      replacement      = str_squish(replacement),
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_add_variable <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    # cleaning_rules_v3_adult_copy %>% slice(1) %>%
    mutate(
      input_variable   = input_variable,
      replacement      = "left",
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_merge_variable <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      var_to_add   = harmo_process_add_variable(process_rule_slice %>% mutate(script = "left")),
      var_to_merge = harmo_process_case_when(process_rule_slice %>% mutate(input_table = output_table)),
      var_to_merge = var_to_merge %>% gsub('^.+?%>%(.*)', "\\1",.),
      replacement = paste0(var_to_add," %>%",var_to_merge),
      replacement = str_replace_all(replacement,"\\.old",".x"),
      replacement = str_replace_all(replacement,"\\.new",".y"),
      to_eval_test = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))
  return(temp.process_script_to_eval$to_eval_test)
}

#' @import dplyr
harmo_process_rename <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      input_variable   = input_variable,
      replacement      = script,
      to_eval_test     = paste0(
        output_table," %>% \n",
        "  rename(",output_variable," = ",input_variable,")"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>% parceval)
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }
}

#' @import dplyr stringr
harmo_process_other <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = script,
      to_eval_test = replacement %>% str_squish
      )

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval)
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_function <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      input_variable   = input_variable %>% str_replace_all(.,";",",") %>% str_squish,
      replacement      = script,

      replacement = str_replace_all(script,"study_table",input_table),
      replacement = str_remove_all(replacement,"\\$"),
      to_eval_test = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }

}

#' @import dplyr stringr
harmo_process_impossible <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = NA,
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }
}

#' @import dplyr
harmo_process_undetermined <- function(process_rule_slice, oneline = FALSE){

  if(oneline == TRUE){process_rule_slice <- rename_process_rule_columns(process_rule_slice)}

  temp.process_script_to_eval <-
    process_rule_slice %>%
    mutate(
      replacement      = NA,
      to_eval_test     = paste0(
        output_table," %>% left_join(\n",
        "  ",input_table," %>% \n",
        "  mutate(\n",
        "  ",output_variable," = as.",type_of,"(",replacement,")) %>% \n",
        "  select(1,",output_variable,"))"))

  if(oneline == TRUE){
    return(temp.process_script_to_eval$to_eval_test %>%
             str_split("left_join\\(") %>% .[[1]] %>% .[[2]] %>%
             str_remove("\\)$") %>% parceval  )
  }else{
    return(temp.process_script_to_eval$to_eval_test)
  }
}

#' @import dplyr lubridate
harmo_parse_process_rule <- function(process_rule , input_table_list, .envir = parent.frame()){

  # cover valueType as.Date
  as.date <- as.Date

  temp.input_tables <- input_table_list
  temp.name_input_tables <- names(temp.input_tables)

  for(i in 1:length(temp.name_input_tables)){
    assign(x     = temp.name_input_tables[i] ,
           value = temp.input_tables[[i]],
           envir = .envir)
  }

  temp.output_tables <- vector(mode = "list", length(process_rule$output_table %>% unique))
  names(temp.output_tables) <- process_rule$output_table %>% unique
  temp.df <- tibble()
  temp.output_tables <- lapply(temp.output_tables, FUN = function(x) bind_rows(x,temp.df))
  temp.name_output_tables <- names(temp.output_tables)

  for(i in 1:length(temp.name_output_tables)){
    try({
      temp.output_tables[[i]] <- parceval(temp.name_output_tables[i])}, silent = TRUE)
  }

  for(i in 1:length(temp.name_output_tables)){
    assign(x     = temp.name_output_tables[i] ,
           value = temp.output_tables[[i]],
           envir = .envir)}

  temp.process_rule <-
    process_rule %>%
    mutate(
      to_eval_test =
        paste0("try(harmo_process_", rule_category,"(temp.process_rule %>% slice(",row_number(),")), silent = TRUE)" ),
      process_script        = NA_character_,
      process_status        = NA_character_)


  for (i in 1:nrow(temp.process_rule)) {
    temp.process_rule$process_script[i] <- eval(parse(text = temp.process_rule$to_eval_test[i]))
  }

message(
"- DATA PROCESSING ELEMENTS: ---------------------------------------------------\n")

  for (i in 1:nrow(temp.process_rule)) {
    # print(temp.process_rule$output_table[i] %>% parceval)
    message("processing: ",i,"/",nrow(temp.process_rule))
    test <- suppressMessages(try(eval(parse(text = temp.process_rule$process_script[i])), silent = TRUE))

    if(class(test) %>% .[[1]] == "try-error"){
      temp.process_rule$process_status[i] <- test[1]
      cat(temp.process_rule$process_script[i],"\n")

    }else{

      assign(x     = temp.process_rule$output_table[i] ,
             value = test,
             envir = .envir)

      temp.process_rule$process_status[i] <-
        ifelse(temp.process_rule$rule_category[i] %in% c("impossible","undetermined"),temp.process_rule$rule_category[i],"complete")
    }
  }

  for(i in 1:length(temp.name_output_tables)){
    try({
      temp.output_tables[[i]] <- parceval(temp.name_output_tables[i])}, silent = TRUE)
  }


  for(i in 1:length(temp.output_tables)){
    temp.output_tables[[i]] <- temp.output_tables[[i]] %>% select(-1)
  }

  temp.output_tables$process_rule <- temp.process_rule

  return(temp.output_tables)
}


#' Generate summary of data processing elements using annotated data processing elements R object
#'
#' Reads annotated data processing elements to list processes, errors, and an
#' overview of each rule of harmonization. The user can take this summary to help
#' in the correction of their data processing elements file and re run the
#' process of harmonization.
#'
#' @param report A tibble of the annotated data processing elements.
#'
#' @examples
#' \dontrun{
#' # Read the files
#' file_index_read(index_DEMO, file_path = "study_specific_datasets")
#'
#' # Example 1: all the process of harmonization with data processing elements with errors
#' file_index_read(index_DEMO, file_name = "Harmo_rule_with_error")
#' harmo_process(
#'   dataschema = Harmo_dataschema,
#'   data_proc_elem = Harmo_rule_with_error,
#'   output_harmo_folder = "DEMO/harmonized_datasets")
#'
#' # Example 2: use the function harmo_summary to access the errors. In the environment,
#' # the user will find a report attached to harmonization process.
#' harmo_summary(annotated_data_proc_elem)
#' }
#'
#' @import utils dplyr
#' @export
harmo_summary <- function(report){

  # list of primary error in the data processing elements. print the list of error + the index

  report_log <-
    report %>%
    select(index, process_script, process_status, rule_category) %>%
    filter(str_detect(process_status,"Error")) %>%
    mutate(
      var = paste0(index," :" ,process_script))

  if(nrow(report_log) > 0){
  message(
"\n
- ERROR STATUS DETAILS: --------------------------------------------------------\n
Here is the list of the errors encountered in the process of harmonization:")
  for(i in 1:nrow(report_log)){
    message(
"--------------------------------------------------------------------------------")
    message(report_log$var[i])
    message(report_log$process_status[i])
    }
  }
  message(
"- STATUS SUMMARY: --------------------------------------------------------------\n")
  message(paste(capture.output({print(
    report_log %>% count(rule_category) %>%
      full_join(.,report %>% count(rule_category),by = "rule_category") %>%
      mutate(
        n.x = replace_na(n.x,0),
        success = (100 - round(n.x/n.y,2)*100)) %>%
      arrange(success) %>%
      mutate(success = paste0(success," %")) %>%
      rename(total_nb_errors = n.x, total_nb_rules = n.y) %>% mutate_all(as.character)

  )}), collapse = "\n"))

}

#' @import utils dplyr
rename_process_rule_columns <- function(data_proc_elem){
  try({
    temp.process_rule <- data_proc_elem %>%
      rename(
        output_table    = dataschema_table,
        output_variable = dataschema_variable,
        input_table     = study_table,
        input_variable  = study_variables,
        script          = harmo_rule) %>%
      mutate(
        rule_category =
          ifelse(input_variable == "undetermined" | script == "undetermined","undetermined",rule_category),
        type_of = str_replace_all(valueType,"text","character"),
        type_of = str_replace_all(type_of,"decimal","numeric")
      )
  })
  return(temp.process_rule)
}
