# packages used
# tidyverse obviously
# read_xlsx

library("tidyverse")
library("stridngr")
library("readxl")

# check if all files are .xlsx
Check_Files1a <- function(path_name = "tests/input/valid"){
    
  if( file.exists(path_name) == FALSE){
    message("Name of folder is inaccurate")} else{}
  
  all_files <- list.files(path = path_name)
  xlsx_files <- list.files(path = path_name, 
                           pattern = ".xlsx")
  
  if( length(all_files) == length(xlsx_files)){
    message(paste("There are", length(xlsx_files),"xlsx files", 
                  "in the relevant folder."))
  } else {
    message(paste("There are", length(xlsx_files),"xlsx files", 
                  "and", length(all_files)-length(xlsx_files), "other files",
                  "in the relevant folder."))
    message(paste("All non-xlsx files are listed below:"))
    message(paste(all_files[(all_files %in% xlsx_files) == FALSE], 
                          collapse = "\n"))
            
  }

}

# check if all .xlsx files can be imported 
Check_Files1b <- function(path_name = "tests/input/valid"){
  
  xlsx_files <- list.files(path = path_name, 
                           pattern = ".xlsx")
  
  tbl <- tibble(
    filename = xlsx_files, 
    import_success = NA)
  
  for (j in 1:length(xlsx_files)){  
    
    try(
      d <- readxl::read_xlsx(
        path = paste0(path_name, "/", xlsx_files[j])) |> 
        suppressMessages(), 
      silent = TRUE)
    
    try(
      tbl$import_success[j] <- exists(x = "d"),
      silent = TRUE)
    
    if (exists(x = "d")) {
      remove(d) 
      } else {}
    
  }
  
  cannot_import <- subset(tbl, import_success == FALSE)
  
  if (all(tbl$import_success) == TRUE){
    message("All .xlsx files can be imported")
  } else {
    message("The following .xlsx files cannot be imported:")
    message(paste(cannot_import$filename, collapse = "\n"))
    
  }
  
  importable_xlsx_files <- tbl |> 
    subset(import_success == TRUE) |> 
    select(filename)
  importable_xlsx_filenames <-  importable_xlsx_files$filename
  
  assign("importable_xlsx_filenames", 
         importable_xlsx_filenames, 
         envir = globalenv()) 
}

# check if all .xlsx files have an instruction and submission sheet
Check_Files2 <- function(path_name = "tests/input/valid", 
                         assign_valid_sheets = TRUE, 
                         importable_xlsx_files = importable_xlsx_filenames){

  xlsx_files <- list.files(path = path_name, 
                           pattern = ".xlsx")
  
  importable_list <- xlsx_files %in% importable_xlsx_files
  
tbl0 <- tibble(
  row = 1:length(xlsx_files),
  excel_filename = xlsx_files)

tbl <- tbl0 |> 
  mutate(full_filename = paste0(path_name,"/",tbl0$excel_filename), 
         can_be_imported = importable_list) |> 
  mutate(
    instruction_sheet = NA, 
    submission_sheet = NA) 

# tbl of files to check the sheets of
tbl2 <- tbl |> 
  subset(can_be_imported == TRUE)

# for loop going through the importable files and checking if appropriate sheets exist
for (j in 1:nrow(tbl2) ){
  
  tbl2$instruction_sheet[j] = "Instructions" %in% readxl::excel_sheets(tbl2$full_filename[j]) 
  tbl2$submission_sheet[j] = "SubmissionTemplate" %in% readxl::excel_sheets(tbl2$full_filename[j])
}

# define the files with missing sheets
no_instructions <- tbl2 |> 
  subset(instruction_sheet == FALSE) |> 
  select(excel_filename)
no_submissions <- tbl2 |> 
  subset(submission_sheet == FALSE) |> 
  select(excel_filename)

# print out messages based on which sheets are present or missing 
if (all(tbl2$instruction_sheet) == TRUE & all(tbl2$submission_sheet) == TRUE){
  message("All checked .xlsx files have appropriately named instruction and submission sheets") 
} else{
  
  if (all(tbl2$instruction_sheet) == TRUE) {
    message("All checked .xlsx files have an instruction sheet")
  } else if (all(tbl2$instruction_sheet) == FALSE) {
    message("The following checked .xlsx files are missing an instruction sheet:")
    message(paste(no_instructions$excel_filename, collapse = "\n"))
  } else {"Some other error"}
  
  message(" ")
  
  if (all(tbl2$submission_sheet) == TRUE) {
    message("All checked. xlsx files have a submission sheet")
  } else if (all(tbl2$submission_sheet) == FALSE) {
    message("The following checked .xlsx files are missing a submission sheet:")
    message(paste(no_submissions$excel_filename, collapse = "\n"))
  } else {"Some other error"}
  
}
 
# rejoin the tbl of checkable files (with info on sheet presence) to uncheckable files
tbl3 <- left_join(
  x = 
    # need to drop the blank values from the cols on whether sheets exist
    tbl |> 
      select(row, excel_filename, full_filename, can_be_imported), 
  y = tbl2)  |> 
  suppressMessages()
if (assign_valid_sheets == TRUE){ assign("valid_submissions", tbl3, envir = globalenv()) 
  } else{ return(tbl3)}
}

# check is all legit files have legitimate names and periods
# also check for duplicates
Check_Files3 <- function(valid_submission_names = valid_submissions ,
                         path_name = "tests/input/valid",
                         assign_valid_sheets = TRUE, 
                         lookup_name = "legitimate_values.xlsx"){
  
  # lookup of possible values
  legitimate_values <- readxl::read_xlsx(lookup_name) |> 
    mutate(Period = zoo::as.yearmon(Month))
  
  # actual xlsx files to be checked
  xlsx_files <- subset(
    valid_submission_names ,
    instruction_sheet == TRUE & submission_sheet == TRUE)$excel_filename
  
  # create basic table which can record whether basic details are valid
  # only covers actual xlsx files which to be checked
  check_instr <- tibble(
    excel_filename = xlsx_files, 
    valid_place = NA, 
    valid_period = NA, 
    place = NA, 
    period = zoo::as.yearmon(NA))
  
  # for loop which updates the table of legit files with whether name and period is valid
  for (j in 1:length(xlsx_files) ){
    
    # extract sheet 1 for name reasons
    sheet1 <- readxl::read_xlsx(
      path = paste0(path_name, "/",xlsx_files[j]), 
      sheet = "Instructions", 
      skip = 0) |> 
      suppressMessages()
    
    # add in place and period
    d2 <- tibble(
      Place = sheet1[5,2]$...2, 
      Period = sheet1[6,2]$...2) |> 
      mutate(Period = zoo::as.yearmon(
        x = as.Date(x = as.numeric(Period), 
                    origin = "1899-12-30")
      ))
    
    check_instr$valid_place[j] = d2$Place %in% subset(legitimate_values, is.na(Project_name) == FALSE)$Project_name 
    check_instr$valid_period[j] = d2$Period %in% subset(legitimate_values, is.na(Period) == FALSE)$Period
    
    check_instr$place[j] = d2$Place 
    check_instr$period[j] = zoo::as.yearmon(d2$Period)
    
  }
  
  valid_submission_names <- left_join(
    x = valid_submission_names, 
    y = check_instr, 
    by = "excel_filename") |> 
    suppressMessages() 
  
  # identify duplicates in place and period
  dup_index <- duplicated(check_instr[, c("place", "period")]) |
               duplicated(check_instr[, c("place", "period")], fromLast = TRUE)
  duplicates <- check_instr[dup_index, 1]
  
  # print off messages about whether and which files are duplictaes
  if(nrow(duplicates) == 0) {
    message("None of the checked .xlsx files are duplicates based on reported place and time period")
  } else {
    message("The following checked .xlsx files are duplicates based on reported place and time period")
    message(paste(duplicates$excel_filename, collapse = "\n"))
  }
  message("")
  
  # add in column to the valid subm table on whether a file is a duplicate
  valid_submission_names <- valid_submission_names |> 
    mutate(unique = (excel_filename %in% duplicates$excel_filename) == FALSE)
  
  
  check_instr <- check_instr |> 
    select(-place, -period)
  
  invalid_placenames <- check_instr |> 
    subset(valid_place == FALSE) |> 
    select(excel_filename)
  invalid_period <- check_instr |> 
    subset(valid_period == FALSE) |> 
    select(excel_filename)
  
  if (all(check_instr$valid_place) == TRUE & 
      all(check_instr$valid_period) == TRUE ){
    message("All checked .xlsx files have valid place names and time periods") 
  } else{
    
    if (all(check_instr$valid_place) == TRUE) {
      message("All checked .xlsx files have valid place names")
    } else if (all(check_instr$valid_place) == FALSE) {
      message("The following checked .xlsx files have invalid place names:")
      message(paste(invalid_placenames$excel_filename, collapse = "\n"))
    } else {"Some other error"}
    
    message(" ")
    
    if (all(check_instr$valid_period) == TRUE) {
      message("All checked .xlsx files have valid time periods")
    } else if (all(check_instr$valid_period) == FALSE) {
      message("The following checked .xlsx files have invalid time periods:")
      message(paste(invalid_period$excel_filename, collapse = "\n"))
    } else {"Some other error"}
    
  }
  
  if (assign_valid_sheets == TRUE){ assign("valid_submissions", valid_submission_names, envir = globalenv()) 
  } else{return(valid_submission_names)}
}

# check all column names are right
# check all metrics are right
Check_Files4 <- function(
    valid_submission_names = valid_submissions ,
    path_name = "tests/input/valid",
    assign_valid_sheets = TRUE ,
    lookup_name = "legitimate_values.xlsx"){
  
  # long lists of legit values ----
  legit_colnames <- c("Metric Id","Metric Type","Metric Details","...4", 
                      "18-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Not known...13", 
                      "White","Black","Asian","Mixed" , "Other", "Not known...19", 
                      "1","2","3", "4","5", "Not Known"  )
  legit_metrics <- c(   "Number of new consultant-led outpatient appointments for patients in the cohort"                                                
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of new consultant-led outpatient appointments per 1,000 patients in the cohort"                                         
                        , "Number of follow up consultant-led outpatient appointments for patients in the cohort"                                          
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of follow up consultant-led outpatient appointments per 1,000 patients in the cohort"                                   
                        , "Number of  consultant-led outpatient procedures  for patients in the cohort"                                                    
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of  consultant-led outpatient procedures per 1,000 patients in the cohort"                                             
                        , "Number of specific acute non-elective spells in the period with a length of stay of zero days for patients in the cohort"       
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of specific acute non-elective spells in the period with a length of stay of zero days per 1,000 patients in the cohort" 
                        , "Number of specific acute non-elective spells in the period with a length of stay of one or more days for patients in the cohort"
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of specific acute non-elective spells in the period with a length of one or more days per 1,000 patients in the cohort"  
                        , "Number of inpatient bed days"                                                                                                   
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of inpatient bed days per 1,000 patients in the cohort"                                                                  
                        , "Number of category 1 ambulance conveyances for patients in the cohort"                                                          
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of category 1 ambulance conveyances per 1,000 patients in the cohort"                                                    
                        , "Number of category 2 ambulance conveyances for patients in the cohort"                                                          
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of category 2 ambulance conveyances per 1,000 patients in the cohort"                                                    
                        , "Number of category 3 ambulance conveyances for patients in the cohort"                                                          
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of category 3 ambulance conveyances per 1,000 patients in the cohort"                                                    
                        , "Number of category 4 ambulance conveyances for patients in the cohort"                                                          
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of category 4 ambulance conveyances per 1,000 patients in the cohort"                                                    
                        , "Number of A&E attendances at a type 1 department in the cohort"                                                                 
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of A&E attendances at a type 1 department per 1,000 patients in the cohort"                                             
                        , "Number of A&E attendances at an other type department in the cohort"                                                            
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of A&E attendances at an other type department per 1,000 patients in the cohort"                                        
                        , "Number of appointments in general    practice and Primary Care Networks"                                                           
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of appointments in general practice and Primary Care Networks per 1,000 patients in the cohort"                          
                        , "Number of Community Care Contacts attended for patients in the cohort"                                                          
                        , "Number of patients in the cohort"                                                                                               
                        , "Number of Community Care Contacts attended per 1,000 patients in the cohort"                                                    
                        , "Number of patients on the NNHIP caseload" )
    legit_metrics <- stringr::str_squish(legit_metrics)
  
  # back to actual wrangling work -----
  
  # actual xlsx files to be checked
  xlsx_files <- subset(
    valid_submission_names,
    can_be_imported == TRUE & 
      instruction_sheet == TRUE & 
      submission_sheet == TRUE)$excel_filename
  
  # create basic table which can record whether basic details are valid
  # only covers actual xlsx files which to be checked
  check_subm <- tibble(
    excel_filename = xlsx_files, 
    valid_columns = NA, 
    valid_metrics = NA)
  
  # for loop which updates the table of legit files with whether name and period is valid
  for (j in 1:length(xlsx_files) ){
    
    # extract the actual dataset
    d1 <- readxl::read_excel(
      path = paste0(path_name, "/",xlsx_files[j]), 
      sheet = "SubmissionTemplate" , 
      skip = 4) |> 
      mutate(`Metric Details` = stringr::str_squish(`Metric Details`)) |> 
      suppressMessages() 
    
    valid_colnames <- all(colnames(d1) == legit_colnames) |> 
      suppressWarnings()
    valid_metrics1 <- all(d1$`Metric Details` == legit_metrics) |> 
      suppressWarnings()
    
    check_subm$valid_columns[j] = valid_colnames
    check_subm$valid_metrics[j] = valid_metrics1
    
  }
  
  # append info on columns and metrics to the master table
  valid_submission_names <- left_join(
    x = valid_submission_names, 
    y = check_subm, 
    by = "excel_filename") |> 
    suppressMessages() 
  
  # create object for wrong columns ----
  wrong_cols <- subset(check_subm, valid_columns == FALSE)$excel_filename
  
  col_errors.master <- tibble(
    excel_filename = character(), 
    error_type = character(), 
    column = character())
  
  for (j in 1:length(wrong_cols)){
    
    # extract the actual dataset
    d1 <- readxl::read_excel(
      path = paste0(path_name, "/",wrong_cols[j]), 
      sheet = "SubmissionTemplate" , 
      skip = 4) |> 
      suppressMessages()
    
    extra_columns <- colnames(d1)[(colnames(d1) %in% legit_colnames)==FALSE]
    missing_columns <- legit_colnames[(legit_colnames %in% colnames(d1))==FALSE]
    
    col_errors <- tibble(
      excel_filename = wrong_cols[j], 
      error_type = c(rep(x = "extra columns", 
                         times = length(extra_columns)), 
                     rep(x = "missing column", 
                         times = length(missing_columns))), 
      column = c(extra_columns, missing_columns))
    
    col_errors.master <- rbind(col_errors.master, col_errors)
  }
  
  if (nrow(col_errors.master)>0){
    assign("column_errors", col_errors.master, envir = globalenv())
  } else {}
  
  # create object for wrong metric ----
  wrong_metr <-  subset(check_subm, valid_metrics == FALSE)$excel_filename
  
  metr_errors.master <- tibble(
    excel_filename = character(), 
    error_type = character(), 
    column = character())
  
  for (j in 1:length(wrong_metr)){
    
    # extract the actual dataset
    d1 <- readxl::read_excel(
      path = paste0(path_name, "/",wrong_metr[j]), 
      sheet = "SubmissionTemplate" , 
      skip = 4) |> 
      mutate(`Metric Details` = stringr::str_squish(`Metric Details`)) |> 
      suppressMessages()
    
    extra_metrics <- d1$`Metric Details`[(d1$`Metric Details` %in% legit_metrics)==FALSE]
    missing_metrics <- legit_metrics[(legit_metrics %in% d1$`Metric Details`)==FALSE]
    
    metr_errors <- tibble(
      excel_filename = wrong_metr[j], 
      error_type = c(rep(x = "extra metric", 
                         times = length(extra_metrics)), 
                     rep(x = "missing metric", 
                         times = length(missing_metrics))), 
      column = c(extra_metrics, missing_metrics))
    
    metr_errors.master <- rbind(metr_errors.master, metr_errors)
  }
  
  if (nrow(metr_errors.master)>0){
    assign("metric_errors", metr_errors.master, envir = globalenv())
  } else {}
  
  # final message output ----
  invalid_metric_names <- subset(valid_submission_names, valid_metrics == FALSE)$excel_filename
  invalid_column_names <- subset(valid_submission_names, valid_columns == FALSE)$excel_filename
  
  if ( all(valid_submission_names$valid_columns) == TRUE & 
       all(valid_submission_names$valid_metrics) == TRUE) {
    message("All checked .xlsx files have valid column names and metrics")
  } else {
    
    if (all(valid_submission_names$valid_columns) == TRUE){
      "All checked .xlsx files have valid columns"
    } else {
      message("The following checked .xlsx files have invalid demographic groups:")
      message(paste(invalid_column_names, 
                    collapse = "\n"))}
    
    message("")
    
    if (all(valid_submission_names$valid_metrics) == TRUE){
      "All checked .xlsx files have valid metrics"
    } else {
      message("The following checked .xlsx files have invalid metrics:")
      message(paste(invalid_metric_names, collapse = "\n"))}
    
    message("")
    message("For further details see the created objects")
    
  }
  
  assign("valid_submissions", valid_submission_names, envir = globalenv())
  
}

# import all the data into three datasets (ethnicity, age, deprivation)
Import_Data <- function(path_name = "tests/input/valid", 
                        valid_submission_names = valid_submissions, 
                        acceptable_valid_places = c(TRUE), 
                        acceptable_valid_periods = c(TRUE), 
                        acceptable_valid_unique = c(TRUE), 
                        acceptable_valid_cols = c(TRUE), 
                        acceptable_valid_metrics = c(TRUE) ){
  
  # report a message telling people not to deviate from the default import rules
  if (all(acceptable_valid_places, acceptable_valid_periods, acceptable_valid_unique, acceptable_valid_cols, acceptable_valid_metrics) == FALSE
  ){message("Importing data with potentially invalid structure or features. This may lead to unexpected or mistaken results.")
    message("")} else (message(""))
  
  # define which files to import
  xlsx_files <- subset(
    valid_submission_names,
    can_be_imported == TRUE &
    instruction_sheet == TRUE & submission_sheet == TRUE & 
      
    valid_place %in% acceptable_valid_places & 
    valid_period %in% acceptable_valid_periods & 
    unique %in% acceptable_valid_unique &
      
    valid_columns %in% acceptable_valid_cols & 
    valid_metrics %in% acceptable_valid_metrics)$excel_filename
    
  
  # setup blank tibbles ----
  full_ages <- tibble(
    Metric = character(), 
    Place = character(), 
    Period = character(), 
    Age_Group = character(), 
    Count = numeric(), 
    Eligible_Cohort_Size = numeric() )
  full_ethnic <- tibble(
    Metric = character(), 
    Place = character(), 
    Period = character(), 
    Ethn_Group = character(), 
    Count = numeric(), 
    Eligible_Cohort_Size = numeric() )
  full_depr <- tibble(
    Metric = character(), 
    Place = character(), 
    Period = character(), 
    Depr_Group = character(), 
    Count = numeric(), 
    Eligible_Cohort_Size = numeric() )
  
  # for loop starts here ----
  
  for (j in 1:length(xlsx_files)){
    
    # basics for all types ----
    # extract the actual dataset
    d1 <- readxl::read_excel(
      path = paste0(path_name, "/",xlsx_files[j]), 
      sheet = "SubmissionTemplate" , 
      skip = 4) |> 
      suppressMessages()
    
    # extract sheet 1 for name reasons
    sheet1 <- readxl::read_xlsx(
      path = paste0(path_name, "/",xlsx_files[j]), 
      sheet = "Instructions", 
      skip = 0) |> 
      suppressMessages()
    
    # add in place and period
    d2 <- d1 |> 
      mutate(Place = sheet1[5,2]$...2, 
             Period = sheet1[6,2]$...2) |> 
      mutate(Period = zoo::as.yearmon(
        x = as.Date(
                x = as.numeric(Period), 
                origin = "1899-12-30")
        ))
    
    # just some formatting
    d3 <- d2 |> 
      # fill in all the NAs
      mutate(`Metric Type` = ifelse(test = is.na(`Metric Type`), 
                                    yes = "Outcome", 
                                    no = `Metric Type`)) |>
      fill(`Metric Id`, .direction = "down") |>
      # renaming variables to names i personally like more
      rename(Metric_Type = `Metric Type`, 
             Metric = `Metric Details`,
             Metric_ID = `Metric Id`,
             Total = ...4, 
             Unknown_Age = `Not known...13`, 
             Unknown_Ethnicity = `Not known...19`, 
             Unknown_Deprivation = `Not Known`)
    
    # extract just the relevant outcomes
    d5 <- d3 |> 
      subset(grepl(pattern = "per 1,000 patients in the cohort", 
                   x = d3$Metric) == FALSE & 
               grepl(pattern = "Number of patients in the cohort", 
                     x = d3$Metric) == FALSE  )
    
    # Age_Group ----
    
    # pivot longer on the actual outcomes
    Age_Group_outcomes <- d5[,c(3:13, 26, 27)]  |> 
      pivot_longer(
        cols = c(2:11), 
        names_to = "Age_Group",
        values_to = "Count", 
        values_transform = list(Count = as.numeric)) |> 
      suppressWarnings()
    
    # get just the cohort patients
    Age_Group_patients <- d3 |> 
      subset(Metric == "Number of patients in the cohort") |> 
      select(-Metric_ID) |> 
      unique() 
    Age_Group_patients2 <- Age_Group_patients[,c(2:12, 25, 26)] |> 
      pivot_longer(col = c(2:11), 
                   names_to = "Age_Group",
                   values_to = "Eligible_Cohort_Size", 
                   values_transform = list(Eligible_Cohort_Size = as.numeric)) |> 
      suppressWarnings() |> 
      select(-Metric)
    
    # join the relevant parts together
    Age_Group <- left_join(
      x = Age_Group_outcomes, 
      y = Age_Group_patients2) |> 
      suppressMessages()
    
    # join the age data for this place to all the others
    full_ages <- rbind(full_ages, Age_Group)
    
    # remove the unnecesary dfs
    rm(list=setdiff(ls(), c("d3", "d5", "path_name", 
                            "xlsx_files", "j", "b", 
                            "full_ages", "full_ethnic", "full_depr")))
    
    # Ethn_Group ----
    
    # pivot longer on the actual outcomes
    Ethn_Group_outcomes <- d5[,c(3,4, 14:19, 26, 27)]  |> 
      pivot_longer(
        cols = c(2:8), 
        names_to = "Ethn_Group",
        values_to = "Count", 
        values_transform = list(Count = as.numeric)) |> 
      suppressWarnings()
    
    
    # get just the cohort patients
    Ethn_Group_patients <- d3 |> 
      subset(Metric == "Number of patients in the cohort") |> 
      mutate(Metric_ID = NA) |> 
      unique() 
    Ethn_Group_patients2 <- Ethn_Group_patients[,c(3,4, 14:19, 26, 27)]  |> 
      pivot_longer(
        cols = c(2:8), 
        names_to = "Ethn_Group",
        values_to = "Eligible_Cohort_Size", 
        values_transform = list(Eligible_Cohort_Size = as.numeric)) |> 
      suppressWarnings() |> 
      select(-Metric)
    
    # join the relevant parts together
    Ethn_Group <- left_join(
      x = Ethn_Group_outcomes, 
      y = Ethn_Group_patients2) |> 
      suppressMessages()
    
    # join the age data for this place to all the others
    full_ethnic <- rbind(full_ethnic, Ethn_Group)
    
    # remove the unnecesary dfs
    rm(list=setdiff(ls(), c("d3", "d5", "path_name", 
                            "xlsx_files", "j", "b", 
                            "full_ages", "full_ethnic", "full_depr")))
    
    # Depr_Group ----
    
    # pivot longer on the actual outcomes
    Depr_Group_outcomes <- d5[,c(3,4, 20:25, 26, 27)]  |> 
      pivot_longer(
        cols = c(2:8), 
        names_to = "Depr_Group",
        values_to = "Count", 
        values_transform = list(Count = as.numeric)) |> 
      suppressWarnings()
    
    # get just the cohort patients
    Depr_Group_patients <- d3 |> 
      subset(Metric == "Number of patients in the cohort") |> 
      mutate(Metric_ID = NA) |> 
      unique() 
    Depr_Group_patients2 <- Depr_Group_patients[,c(3,4, 20:25, 26, 27)]  |> 
      pivot_longer(
        cols = c(2:8), 
        names_to = "Depr_Group",
        values_to = "Eligible_Cohort_Size", 
        values_transform = list(Eligible_Cohort_Size = as.numeric)) |> 
      suppressWarnings() |> 
      select(-Metric)
    
    # join the relevant parts together
    Depr_Group <- left_join(
      x = Depr_Group_outcomes, 
      y = Depr_Group_patients2) |> 
      suppressMessages()
    
    # join the age data for this place to all the others
    full_depr <- rbind(full_depr, Depr_Group)
    
    # remove the unnecesary dfs
    rm(list=setdiff(ls(), c("d3", "d5", "path_name", 
                            "xlsx_files", "j", "b", 
                            "full_ages", "full_ethnic", "full_depr")))
    # print completed message ----
    
    message(xlsx_files[j], " completed!")
  }
  
  # assign the outputs to the global environment
  assign(x = "age_breakdown", 
         value = full_ages, 
         envir = globalenv())
  assign(x = "ethnicity_breakdown", 
         value = full_ethnic, 
         envir = globalenv())
  assign(x = "deprivation_breakdown", 
         value = full_depr, 
         envir = globalenv())
  

}

# check for NA values
Check_Data1 <- function(age_breakdown_aggr = age_breakdown,
                        deprivation_breakdown_aggr = deprivation_breakdown,
                        ethnicity_breakdown_aggr = ethnicity_breakdown,
                        print_all_NAs = TRUE,
                        valid_submission_names = valid_submissions){
  
  # arguments
  
  
  
  age_NAs <- age_breakdown_aggr |> 
    subset(is.na(Count) | is.na(Eligible_Cohort_Size))
  
  depr_NAs <- deprivation_breakdown_aggr |> 
    subset(is.na(Count) | is.na(Eligible_Cohort_Size))
  
  ethn_NAs <- ethnicity_breakdown_aggr|> 
    subset(is.na(Count) | is.na(Eligible_Cohort_Size))
  
  any_NAs <- rbind(
    select(age_NAs, Place, Period), 
    select(ethn_NAs, Place, Period),
    select(depr_NAs, Place, Period)) |> 
    unique() |> 
    transmute(place = Place, 
              period = Period)
  
  na_names <- left_join(
    x = any_NAs, 
    y = valid_submission_names) |> 
    suppressMessages() |> 
    select(excel_filename)
  
  if( nrow(age_NAs) == 0 & 
      nrow(depr_NAs) == 0 & 
      nrow(ethn_NAs) == 0 ){
    message("There are no missing values or NAs in any of the imported datasets")
  } else if(print_all_NAs == TRUE){
    message("There are missing values or NAs in the following imported datasets:")
    message(paste(na_names$excel_filename, collapse = "\n"))
    message("")
    message(paste0("In total there are ", 
                   nrow(age_NAs) + nrow(depr_NAs) + nrow(ethn_NAs), 
                   " missing values or NAs across all the imported datasets"))
    message("See created objects for further details")
    
    assign("age_NAs", age_NAs, envir = globalenv())
    assign("ethnicity_NAs", ethn_NAs, envir = globalenv())
    assign("deprivation_NAs", depr_NAs, envir = globalenv())
  } else {
    message("There are missing values or NAs in some of the imported datasets:")
    message("")
    message(paste0("In total there are ", 
                   nrow(age_NAs) + nrow(depr_NAs) + nrow(ethn_NAs), 
                   " missing values or NAs across all the imported datasets"))
    message("See created objects for further details")
    
    assign("age_NAs", age_NAs, envir = globalenv())
    assign("ethnicity_NAs", ethn_NAs, envir = globalenv())
    assign("deprivation_NAs", depr_NAs, envir = globalenv())
  }
  

  
}


# put it all together
Run_Full_Process <- function(file_location = "tests/input/valid"){
  
  
  
  a <- Sys.time()
  
  message("--- Checking file format is .xlsx ---")
  Check_Files1a(path_name = file_location)
  message("")
  message("--- Checking .xlsx files can be imported ---")
  Check_Files1b(path_name = file_location)
  message("")
  message("--- Checking .xlsx files have necessary sheets ---")
  Check_Files2(path_name = file_location)
  message("")
  message("--- Checking .xlsx files cover valid places and time periods ---")
  Check_Files3(path_name = file_location)
  message("")
  message("--- Checking .xlsx files include valid demographics and metrics ---")
  Check_Files4(path_name = file_location)
  message("")
  
  message("--- Importing data for valid submissions ---")
  Import_Data(path_name = file_location)
  message("")
  
  message("--- Checking imported data for NAs ----")
  Check_Data1() 
  message("")
  
  b <- Sys.time()
  elapsed = as.numeric(b-a)
  message(paste("Full process took ", 
                round(elapsed/60, 1), 
                " minutes"))
  
}
