source("data_setup.R")
print("1/5 Data set-up completed")
rm(list=ls())

source("main_script.R")
print("2/5 Main analysis completed")
rm(list=ls())

source("placebo_script.R")
print("3/5 Placebo analysis completed (Rashomon analysis next which will take a while)")
rm(list=ls())

source("rashomon_script.R")
print("4/5 Rashomon analysis completed")
rm(list=ls())

system('python python_forest.py')
print("5/5 Python XAI analysis completed")

