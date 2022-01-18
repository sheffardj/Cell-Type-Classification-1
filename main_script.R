#  Title: AUTO-ANNOTATION OF CELLTYPE
#
#  Midterm Project: Statistics 454/556 - University of Victoria
# 
#  Contributors: Dayten J. Sheffar & Leno S. Rocha


#### READ ME ####
#
#  SUBMITTED TO COMPUTE CANADA ++ CHECK SHELLSCRIPT
#
#  Estimated Runtime: ________
#
#  Libraries Used: 
#  "MASS"   "class"   "caret"   "glmnet"   "tree"   "dplyr"   "data.table" 
#  "reshape2"   "ggplot2"   "RColorBrewer"   "gridExtra"   "foreach"   "readr"
#  "doParallel"   "bigstatsr"   "rpart"   "stats"  "ggpubr"   "xtable"      
#  
#
#  READ THIS PART!
#  This file is the main script to run, it will draw upon the other scripts 
#  found in the submitted ZIP file along with the shell script main_script.sh
#  which is to be run on Compute Canada. You're welcome to change the parameters
#  found in the shell script to suit your needs but the shell script sucessfully
#  ran the experiment, producing all data frames, tables, and figures either 
#  used in the presentation or internally in these scripts.



time <- proc.time()[[3]]
print('1. job underway')
set.seed(0)

source('install_libraries.R')
source('load_data.R')
source('feat_select.R')
print(paste('Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))

source('EDA_df_sml.R') ## UNCOMMENT!!!
print(paste('Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))


time_models <- proc.time()[[3]]
source('model_fitting.R')
print(paste('Model Fitting Time:',(proc.time()[[3]]-time_models)/60,' minutes'))
print(paste('Total Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))

source('post_fit.R')
print(paste('Elapsed Time:',(proc.time()[[3]]-time)/60,' minutes'))

print('12. job complete!')




