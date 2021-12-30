################################################################################
###  Title: AUTO-ANNOTATION OF CELLTYPE
###
###  Midterm Project: Statistics 454/556 - University of Victoria
### 
###  Contributors: Dayten J. Sheffar  &  Leno S. Rocha
#
#
#
#
################################################################################
################################### READ ME ####################################
################################################################################
#
#
#  SUBMITTED TO COMPUTE CANADA; CHECK INCLUDED SHELLSCRIPT IF DESIRED.
#  RESOURCES: 64 CPUS-PER-TASK  &&  8GB MEMORY-PER-CPU
#  
#  METHOD: 100 Cross Validations of 10-folds. 
#  MODELS: 5 Elastic Nets; LASSO; LDA; CLTree.
#
#  Estimated Runtime: 20.1 HOURS ------- SUGGESTED RUNTIME: 23:59 HOURS
#
#  Libraries Used:                                        
#  "MASS"    "class"   "readr"   "ggpubr"   "foreach"     "bigstatsr"
#  "tree"    "caret"   "xtable"  "glmnet"   "reshape2"    "data.table"        
#  "rpart"   "stats"   "dplyr"   "ggplot2"  "gridExtra"   "doParallel"                 
#                                                         "RColorBrewer"  
#
#  PLEASE READ THIS PART!
#  This file is the main script to run, it will draw upon the other scripts 
#  found in the submitted ZIP file along with the shell script main_script.sh
#  which is to be run on Compute Canada. You're welcome to change the parameters
#  found in the shell script to suit your needs. The shell script successfully
#  ran the experiment, producing all data frames, tables, and figures either 
#  used in the presentation or internally in these scripts. The SLURM output
#  is also included for your reference. 
#
#
#  REDUNDANT: PUT ALL UNZIPPED FILES IN ONE WORKING DIRECTORY/FOLDER.
#             SUBMIT: sbatch ctype.sh FROM THIS DIRECTORY ON COMPUTE CANADA.
#             CSVs AND PLOTS WILL EXPORT TO THIS WORKING DIRECTORY.
#             THEY ARE CALLED UPON IN OTHER SCRIPTS -- VERY IMPORTANT!!!
#
#             THIS PROJECT IS MODULARIZED.
#             THIS PROJECT RUNS IN PARALLEL WITH SEVERAL NESTED FOREACH LOOPS.
#             YOU ONLY NEED TO CALL THE SHELL SCRIPT.
#            '1_main_script' IS THE ONLY SCRIPT INITIALIZED BY THE SHELL SCRIPT
#
#             REGARDING SHELL SCRIPT: CHANGE THE PATH TO R/LIBRARY AS NEEDED!
#         
#
################################################################################


time <- proc.time()[[3]]
print('1.1 - Job Underway') #1
set.seed(0) #Also called wherever needed in other scripts.

source('2_install_libraries.R') #2
source('3_load_data.R') #3
source('4_feat_select.R') #4
print(paste('    - Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))


#DONT RUN THIS STUFF NOW
if(0){
source('5_EDA_df_sml.R') #5
print(paste('    - Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))


time_models <- proc.time()[[3]]
source('6_model_fitting.R') #6
print(paste('    - Model Fitting Time:',(proc.time()[[3]]-time_models)/60,' minutes'))
print(paste('    - Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))

source('7_post_fit.R') #7
print(paste('    - Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))

print('1.2 - Job Complte!')
print(paste('    - Total Time:',(proc.time()[[3]]-time)/60,' minutes'))
}
### use shell script to submit LATER 1_main_script


