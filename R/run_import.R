# 1 initial setup ----

# clean the environment
rm(list = ls())

# packages required
require("tidyverse")
require("stridngr")
require("readxl")

# load in the bespoke functions
source("functions.R") 

# 2 run the process step by step ----

# define file location
file_location = "tests/input/valid"

# check validity of submission basics
Check_Files1a(path_name = file_location)
Check_Files1b(path_name = file_location)
Check_Files2(path_name = file_location)
Check_Files3(path_name = file_location)
Check_Files4(path_name = file_location)

# import data
# to import data with invalid values for a variable, change the argument to c(TRUE, FALSE)
# i.e. to import files with invalid metrics, acceptable_valid_metrics = c(TRUE, FALSE)
Import_Data(path_name = file_location)

# check for NAs and missing data
Check_Data1()

# 3 alternatively the process can be run all at once ----
# Run_Full_Process(file_location = "tests/input/valid")



  
  
  
  
  
  
  
  
  
  
  
  
  

    