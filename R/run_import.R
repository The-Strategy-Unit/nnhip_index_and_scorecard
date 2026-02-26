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

# run each function in order
Check_Files1a(path_name = file_location)
Check_Files1b(path_name = file_location)
Check_Files2(path_name = file_location)
Check_Files3(path_name = file_location)
Check_Files4(path_name = file_location)
Import_Data(path_name = file_location)
Check_Data1()

# 3 alternatively the process can be run all at once ----
# Run_Full_Process(file_location = "tests/input/valid")



  
  
  
  
  
  
  
  
  
  
  
  
  

    