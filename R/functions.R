# packages used
# tidyverse obviously
# read_xlsx


# check if all files are .xlsx
Check_Files1a <- function(){
    
  all_files <- list.files(path = "submissions")
  xlsx_files <- list.files(path = "submissions", 
                           pattern = ".xlsx")
  
  if( length(all_files) == length(xlsx_files)){
    message(paste("There are", length(xlsx_files),"xlsx files", 
                  "in the submissions folder."))
  } else {
    message(paste("There are", length(xlsx_files),"xlsx files", 
                  "and", length(all_files)-length(xlsx_files), "other files",
                  "in the submission folder."))
    message(paste("All non-xlsx files are listed below:"))
    message(all_files[(all_files %in% xlsx_files) == FALSE])
  }
  
}

# check if all .xlsx files can be imported 
Check_Files1b <- function(){
  xlsx_files <- list.files(path = "submissions", 
                           pattern = ".xlsx")
  
  tbl <- tibble(
    filename = xlsx_files, 
    import_success = NA)
  
  for (j in 1:length(xlsx_files)){  
    
    try(
      d <- readxl::read_xlsx(
        path = paste0("submissions/", xlsx_files[j])) |> 
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
    message(cannot_import$filename)
    
  }
}

# check if all .xlsx files have an instruction and submission sheet
Check_Files2 <- function(){

xlsx_files <- list.files(path = "submissions", 
                         pattern = ".xlsx")

tbl <- tibble(
  row = 1:1:length(xlsx_files),
  excel_filename = xlsx_files)

tbl <- tbl |> 
  mutate(
    full_filename = paste0("submissions/",tbl$excel_filename),
    instruction_sheet = NA, 
    submission_sheet = NA)

for (j in 1:nrow(tbl) ){
  
  tbl$instruction_sheet[j] = "Instructions" %in% readxl::excel_sheets(tbl$full_filename[j]) 
  tbl$submission_sheet[j] = "SubmissionTemplate" %in% readxl::excel_sheets(tbl$full_filename[j])
}

no_instructions <- tbl |> 
  subset(instruction_sheet == FALSE) |> 
  select(excel_filename)
no_submissions <- tbl |> 
  subset(submission_sheet == FALSE) |> 
  select(excel_filename)

if (all(tbl$instruction_sheet) == TRUE & all(tbl$submission_sheet) == TRUE){
  message("All .xlsx files have appropriately named sheets") 
} else{
  
  if (all(tbl$instruction_sheet) == TRUE) {
    message("No .xlsx files are missing instruction sheets")
  } else if (all(tbl$instruction_sheet) == FALSE) {
    message("The following .xlsx files are missing instruction sheets:")
    message(no_instructions)
  } else {"Some other error"}
  
  message(" ")
  
  if (all(tbl$submission_sheet) == TRUE) {
    message("No .xlsx files are missing submission sheets")
  } else if (all(tbl$submission_sheet) == FALSE) {
    message("The following .xlsx files are missing submission sheets:")
    message(no_submissions)
  } else {"Some other error"}
  
}
 
}

# import all the data into three datasets (ethnicity, age, deprivation)
Import_Data <- function(){
  
  b <- Sys.time()
  xlsx_files <- list.files(path = "submissions", 
                           pattern = ".xlsx")
  
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
      path = paste0("submissions/",xlsx_files[j]), 
      sheet = "SubmissionTemplate" , 
      skip = 4) |> 
      suppressMessages()
    
    # extract sheet 1 for name reasons
    sheet1 <- readxl::read_xlsx(
      path = paste0("submissions/",xlsx_files[j]), 
      sheet = "Instructions", 
      skip = 0) |> 
      suppressMessages()
    
    # add in place and period
    d2 <- d1 |> 
      mutate(Place = sheet1[5,2]$...2, 
             Period = sheet1[6,2]$...2)
    
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
    rm(list=setdiff(ls(), c("d3", "d5", 
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
    rm(list=setdiff(ls(), c("d3", "d5", 
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
    rm(list=setdiff(ls(), c("d3", "d5", 
                            "xlsx_files", "j", "b", 
                            "full_ages", "full_ethnic", "full_depr")))
    # print completed message
    
    message(xlsx_files[j], " completed!")
  }
  
  a <- Sys.time()
  message("Total import took ", 
          round(x = as.numeric(a-b)/60, 
                digits = 2), 
          " minutes")
  
}

# still need to check the missing data
# still need to do some reformatting, i.e. period in date format
# check the numbers even add up

Check_Files1a()
Check_Files1b()
Check_Files2()
Import_Data()