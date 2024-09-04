#' Create a data dictionary in the Maelstrom Research format from a dataset
#'
#' Creates a data dictionary in the Maelstrom Research formats (with "Variables" and "Categories"
#' in separate tibbles and standard columns in each) from any dataset in tibble format.
#' If the input dataset has no associated metadata, a data dictionary with a minimal
#' of information is created from the column (variable) names to create the data
#' dictionary structure required for harmonizRLegacy (all columns except variable names
#' will be blank).
#'
#' Must provide an input data dictionary in tibble format, with or without metadata
#' in addition to column names.
#'
#' @param tbl_name A character string or tibble R object identifying the input dataset.
#' @param project A character string identifying the associated project (if any).
#' @param categories A character vector in a specific structure identifying the
#' columns that contain categorical variables (if any) and the associated category names.
#' c("variableA{name1 = label1 ; name2 = label2}, variableB") means that the variableA
#' has two labels and names associated, and variableB has all of its values as categories.
#' @param lang A character string identifying the lang of the data dictionary (default 'en')
#'
#' @return A list of two tibbles: Variables and Categories. Variables has all the
#' variable names extracted from the dataset column names as rows and the Maelstrom
#' Research descriptors standards of variables as columns. Categories has all the category
#' names extracted from the “categories” argument as rows and the Maelstrom Research
#' descriptors standard of categories as columns.
#'
#' @examples
#' \dontrun{
#' # use case 1: create a data dictionary from any dataset
#' data_dict_create(iris)
#'
#' # use case 2: create a data dictionary with project and categorical variables
#' data_dict_create(
#'   tbl_name = iris,
#'   project = "iris",
#'   categories = c("Sepal.Length{-888 = none ; -7 = PNA} , Species"))
#' }
#'
#' @import dplyr rlang tidyr
#' @export
data_dict_create <- function(tbl_name, project = NA_character_, categories = c(), lang = "en"){

  tbl_name <- if(class(tbl_name)[1] == "character") {tbl_name}else{as.character(substitute(tbl_name)) }
  tbl <- if(class(tbl_name)[1] == "character") { parceval(tbl_name) }else{ tbl_name }

  temp.project <- project

  index       = rep(NA, ncol(tbl))
  project     = rep(NA, ncol(tbl))
  table       = rep(NA, ncol(tbl))
  name        = rep(NA, ncol(tbl))
  label       = rep(NA, ncol(tbl))
  unit        = rep(NA, ncol(tbl))
  category    = rep(NA, ncol(tbl))
  cat_lab     = rep(NA, ncol(tbl))
  value_class = rep(NA, ncol(tbl))
  value_type  = rep(NA, ncol(tbl))
  missing     = rep(0 , ncol(tbl))

  for(i in 1:ncol(tbl)){
    try({index[i]       <- i}, silent = TRUE)
    try({project[i]     <- temp.project}, silent = TRUE)
    try({table[i]       <- tbl_name}, silent = TRUE)
    try({name[i]        <- tbl[i] %>% names}, silent = TRUE)
    try({label[i]       <- tbl[i] %>% pull(.) %>% attr("label",exact = TRUE)}, silent = TRUE)
    try({cat_lab[i]     <- tbl[i] %>% pull(.) %>% attr("labels",exact = TRUE) %>% names %>% paste(.,collapse = "|")}, silent = TRUE)
    try({category[i]    <- tbl[i] %>% pull(.) %>% attr("labels",exact = TRUE) %>% paste(.,collapse = "|")}, silent = TRUE)
    try({value_class[i] <- tbl[i] %>% pull(.) %>% class %>% .[1]}, silent = TRUE)
    try({value_type[i]  <- tbl[i] %>% pull(.) %>% typeof}, silent = TRUE)}

  # lst_att <- tibble(col = as.character())
  #
  # for(i in 1:ncol(tbl)){
  #
  #   att <- attributes(x = tbl[i] %>% pull)
  #
  #   lst_att %>%
  #     mutate()
  #
  #   for(j in att %>% names){
  #
  #     btt <- tbl[i] %>% pull(.) %>% attr("class",exact = TRUE) %>% paste(collapse = "|*|")
  #
  #   }
  # }

  tbl_numerical = tibble()
  tbl_int = tibble()
  tbl_dbl = tibble()
  var_int = c()

  tbl_numerical = tbl %>% summarise_all(typeof) %>% gather %>% filter(value %in% c("double","integer")) %>% pull(key)
  try({tbl_int <-
    suppressWarnings(tbl %>%
                       select(tbl_numerical) %>%
                       mutate_all(as.integer) %>%
                       summarise_all(sum, na.rm = TRUE) %>%
                       gather %>%
                       rename(var_int = key, as_int = value))}, silent = TRUE)
  try({tbl_dbl <- tbl %>% select(tbl_numerical) %>% mutate_all(as.numeric) %>% summarise_all(sum, na.rm = TRUE) %>% gather %>% rename(var_num = key, as_num = value)}, silent = TRUE)
  try({var_int <- bind_cols(tbl_int,tbl_dbl) %>% filter(as_int == as_num) %>% pull(var_int)}, silent = TRUE)

  data_dict_long <-
    tibble(index,project, table, name, label, unit, cat_lab, category, missing, value_class, value_type) %>%
    mutate_all(., ~as.character(.)) %>%
    mutate_all(., ~na_if(.,"")) %>%
    mutate(
      valueType = case_when(
        value_class == 'haven_labelled' & value_type == 'double'  & (!name %in% var_int)   ~ 'decimal',
        value_class == 'haven_labelled' & value_type == 'double'  & (name %in% var_int)    ~ 'integer',
        value_class == 'haven_labelled' & value_type == 'character'                        ~ 'text',
        value_class == 'integer'        & value_type == 'integer' & (name %in% var_int)    ~ 'integer',
        value_class == 'Date'                                                              ~ 'date',
        value_class == 'POSIXct'                                                           ~ 'text',
        value_class == 'numeric'                                                           ~ 'decimal',
        TRUE                                                                               ~ "text"
      ))

  data_dict <- list()
  data_dict$Variables <- data_dict_long %>%
    select(
      index,
      project,
      table,
      name,
      `label:lang` = label,
      unit,
      valueType) %>% mutate_all(as.character)

  # manual modifications
  data_dict$Variables$`label:lang`[is.na(data_dict$Variables$`label:lang`)] <- "missing information"

  data_dict$Categories <-
    data_dict_long %>%
    select(project, table, variable = name, name = category, `label:lang` = cat_lab, missing, value_class) %>%
    filter(value_class == "haven_labelled") %>%
    separate_rows("name","label:lang", sep = "\\|") %>%
    mutate_at(.vars = c("name","label:lang"), .funs = ~str_squish(.)) %>%
    mutate_at(.vars = c("name"), .funs = ~na_if(.,"NA")) %>%
    select(-value_class) %>%
    add_row(
      table = NA,
      variable = NA,
      name = NA,
      `label:lang` = NA,
      missing = NA) %>% mutate_all(as.character)

  if(!is_empty(categories)){

    temp.categories <-
      silently_run(
        categories %>% as_tibble %>%
          separate_rows(value, sep = ",") %>%
          mutate_all(str_squish) %>%
          separate(col = value, into = c("variable","categories"),sep = "\\{") %>%
          mutate(categories = str_remove(categories,"\\}$")) %>%
          mutate(
            categories_all =
              ifelse(is.na(categories), eval(parse(text = paste0(as.character(substitute(tbl))," %>% pull(",variable,") %>% unique %>% toString"))),NA),
            categories_all = str_replace_all(categories_all,","," = missing information ; "),
            categories_all = ifelse(!is.na(categories_all), paste0(categories_all," = missing information"),categories_all),
            categories = ifelse(is.na(categories), categories_all,categories),
            missing = 0,
            project = temp.project,
            table   = tbl_name) %>%
          select(-categories_all) %>%
          separate_rows(categories, sep = ";") %>%
          separate(col = categories, into = c("name","label:lang"),sep = "\\=") %>%
          mutate_all(str_squish) %>%
          mutate_all(~na_if(.,"NA")) %>%
          filter(!is.na(name)) %>%
          mutate(`label:lang` = ifelse(`label:lang`== "missing information", name,`label:lang`)) %>%
          mutate(name = str_replace(name,"^$","missing information"))) %>%
      mutate_all(as.character)
    #
    data_dict$Categories <-
      data_dict$Categories %>% bind_rows(.,temp.categories) %>%
      filter(!is.na(variable))
  }

  data_dict$Variables  <- data_dict$Variables %>%
    rename_at(.vars = "label:lang",.funs = ~paste0("label:",lang))
  data_dict$Categories <- data_dict$Categories %>%
    rename_at(.vars = "label:lang",.funs = ~paste0("label:",lang)) %>%
    mutate(missing = missing %>% as.numeric())

  return(data_dict)

}


#' Apply all the valueTypes of a data dictionary to a dataset
#'
#' Uses a data dictionary in the Maelstrom Research formats (with "Variables" and "Categories"
#' in separate tibbles and standard columns in each) to apply their valueType to a dataset in tibble format.
#' If no data dictionary is provided, the function will automatically evaluate the most restrictive valueType
#' for each variable in the dataset and apply it.
#'
#' Must provide an input dataset in tibble format
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param data_dict tibble of the data dictionary (automatically generated if not provided)
#'
#' @return The dataset with the data dictionary valueType
#' applied to each variable
#'
#' @examples
#' \dontrun{
#' # use case 1: Apply valueType without specifying a data dictionary
#' apply_valueType(dataset = study_TOKYO)
#'
#' # use case 2: Apply valueType using a specified data dictionary
#' apply_valueType(dataset = study_TOKYO , data_dict = dd_TOKYO_format_maelstrom_tagged)
#'}
#'
#' @import dplyr tidyr lubridate
#' @export
apply_valueType <- function(dataset, data_dict = data_dict_create(dataset)){

  # i = "dob"

  vars <- data_dict$Variables %>% pull(name) %>% intersect(dataset %>% names)
  data_dict$Variables <- data_dict$Variables %>% filter(name %in% vars)

  valueType_numeric <-
    data_dict$Variables %>% select(name, valueType) %>%
    filter(valueType %in% c("decimal")) %>%
    pull(name)

  valueType_integer <-
    data_dict$Variables %>% select(name, valueType) %>%
    filter(valueType %in% c("integer","boolean","binary")) %>%
    pull(name)

  valueType_date <-
    data_dict$Variables %>% select(name, valueType) %>%
    filter(valueType %in% c("date","datetime","time")) %>%
    pull(name)

  valueType_character <-
    data_dict$Variables %>% select(name, valueType) %>%
    filter(valueType %in% c("text")) %>%
    pull(name)

  dataset <-
    dataset %>%
    mutate_at(.vars = valueType_numeric,.funs   = ~ as.numeric(.)) %>%
    mutate_at(.vars = valueType_integer,.funs   = ~ as.integer(.)) %>%
    mutate_at(.vars = valueType_character,.funs = ~ as.character(.))

  dataset <- dataset %>%
    select(-valueType_date) %>%
    add_column(dataset %>% select(valueType_date) %>% apply_date_format()) %>%
    select(dataset %>% names)

  return(dataset)
}

#' @import dplyr lubridate
guess_date_format <- function(dataset, col = NULL){

  if(is.null(col)) col <- dataset %>% names
  dataset <- dataset %>% select(all_of(col))

  test <- tibble(
    name_var = as.character(),
    `Date format` = as.character(),
    `% values formated` = as.numeric())

  for(i in dataset %>% names){

    test <- bind_rows(test,
      dataset %>%
        select(var = all_of(i)) %>%
        filter(!is.na(var)) %>%
        rowwise() %>%
        mutate(
          dmy = dmy(var, quiet = TRUE),
          dym = dym(var, quiet = TRUE),
          ymd = ymd(var, quiet = TRUE),
          ydm = ydm(var, quiet = TRUE),
          mdy = mdy(var, quiet = TRUE),
          myd = myd(var, quiet = TRUE)) %>%
        ungroup %>%
        summarise(across(c(dmy,dym,ymd,ydm,mdy,myd), ~ n_distinct(., na.rm = TRUE))) %>%
        pivot_longer(cols = everything(),names_to = "Date format",values_to = "nb_values") %>%
        mutate(
          name_var = i,
          `% values formated` = round(100*(nb_values / (dataset %>% select(var = all_of(i)) %>% filter(!is.na(var)) %>% nrow)),2),
          `% values formated` = ifelse(is.na(`% values formated`),0,`% values formated`),
          `Date format` = case_when(
            `% values formated` == 0   ~ "No match",
            `% values formated` == 100 ~ paste0("Exact match : ", `Date format`),
            TRUE                       ~ paste0("Closest match : ", `Date format`)),
        )  %>%
        arrange(-nb_values) %>%
        slice(1) %>%
        select(-nb_values)
      )
  }
  return(test)
}


#' @import dplyr lubridate
apply_date_format <- function(dataset, col = NULL){

  if(is.null(col)) col <- dataset %>% names
  dataset <- dataset %>% select(all_of(col))

  date_format_guess <-
    guess_date_format(dataset, col) %>%
    filter(`% values formated` == 100)

  if(date_format_guess %>% nrow() == 0){

    message("No exact match has been found in any column. The dataset has not been modified")
    return(dataset)

  }else{

    for(i in date_format_guess$name_var){

      selected_format <-
        date_format_guess %>%
        filter(name_var == i) %>%
        pull(`Date format`) %>%
        str_remove("Exact match : ")

      dataset <-
        dataset %>%
        rowwise() %>%
        mutate(across(i,
          ~ case_when(
            selected_format == "dmy" ~ dmy(., quiet = TRUE),
            selected_format == "dym" ~ dym(., quiet = TRUE),
            selected_format == "ymd" ~ ymd(., quiet = TRUE),
            selected_format == "ydm" ~ ydm(., quiet = TRUE),
            selected_format == "mdy" ~ mdy(., quiet = TRUE),
            selected_format == "myd" ~ myd(., quiet = TRUE),
          ))) %>%
        ungroup
    }
    return(dataset)
  }
}


#' @import dplyr lubridate
as_any_date <- function(x, ...){


  # value = study_TOKYO$dob[1]
  if(is.Date(x)) return(TRUE)


  for(i in dataset %>% names){

    test <- bind_rows(test,
                      dataset %>%
                        select(var = all_of(i)) %>%
                        filter(!is.na(var)) %>%
                        rowwise() %>%
                        mutate(
                          dmy = dmy(var, quiet = TRUE),
                          dym = dym(var, quiet = TRUE),
                          ymd = ymd(var, quiet = TRUE),
                          ydm = ydm(var, quiet = TRUE),
                          mdy = mdy(var, quiet = TRUE),
                          myd = myd(var, quiet = TRUE)) %>%
                        ungroup %>%
                        summarise(across(c(dmy,dym,ymd,ydm,mdy,myd), ~ n_distinct(., na.rm = TRUE))) %>%
                        pivot_longer(cols = everything(),names_to = "Date format",values_to = "nb_values") %>%
                        mutate(
                          name_var = i,
                          `% values formated` = round(100*(nb_values / (dataset %>% select(var = all_of(i)) %>% filter(!is.na(var)) %>% nrow)),2),
                          `% values formated` = ifelse(is.na(`% values formated`),0,`% values formated`),
                          `Date format` = case_when(
                            `% values formated` == 0   ~ "No match",
                            `% values formated` == 100 ~ paste0("Exact match : ", `Date format`),
                            TRUE                       ~ paste0("Closest match : ", `Date format`)),
                        )  %>%
                        arrange(-nb_values) %>%
                        slice(1) %>%
                        select(-nb_values)
    )
  }
  return(test)

}


#' @import dplyr lubridate
is_any_date <- function(x, ...){


  # value = study_TOKYO$dob[1]
  if(is.Date(x)) return(TRUE)


  for(i in dataset %>% names){

    test <- bind_rows(test,
                      dataset %>%
                        select(var = all_of(i)) %>%
                        filter(!is.na(var)) %>%
                        rowwise() %>%
                        mutate(
                          dmy = dmy(var, quiet = TRUE),
                          dym = dym(var, quiet = TRUE),
                          ymd = ymd(var, quiet = TRUE),
                          ydm = ydm(var, quiet = TRUE),
                          mdy = mdy(var, quiet = TRUE),
                          myd = myd(var, quiet = TRUE)) %>%
                        ungroup %>%
                        summarise(across(c(dmy,dym,ymd,ydm,mdy,myd), ~ n_distinct(., na.rm = TRUE))) %>%
                        pivot_longer(cols = everything(),names_to = "Date format",values_to = "nb_values") %>%
                        mutate(
                          name_var = i,
                          `% values formated` = round(100*(nb_values / (dataset %>% select(var = all_of(i)) %>% filter(!is.na(var)) %>% nrow)),2),
                          `% values formated` = ifelse(is.na(`% values formated`),0,`% values formated`),
                          `Date format` = case_when(
                            `% values formated` == 0   ~ "No match",
                            `% values formated` == 100 ~ paste0("Exact match : ", `Date format`),
                            TRUE                       ~ paste0("Closest match : ", `Date format`)),
                        )  %>%
                        arrange(-nb_values) %>%
                        slice(1) %>%
                        select(-nb_values)
    )
  }
  return(test)

}


