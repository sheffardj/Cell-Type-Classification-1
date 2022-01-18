print('installing libraries')

repo = 'https://cloud.r-project.org/'

if (!require(MASS)) install.packages('MASS', repos = repo)
if (!require(class)) install.packages('class', repos = repo)
if (!require(caret)) install.packages('caret', repos = repo)
if (!require(glmnet)) install.packages('glmnet', repos = repo)
if (!require(tree)) install.packages('tree', repos = repo)
if (!require(dplyr)) install.packages('dplyr', repos = repo)
if (!require(reshape2)) install.packages('reshape2', repos = repo)
if (!require(ggplot2)) install.packages('ggplot2', repos = repo)
if (!require(RColorBrewer)) install.packages('RColorBrewer', repos = repo)
if (!require(gridExtra)) install.packages('gridExtra', repos = repo)
if (!require(foreach)) install.packages('foreach', repos = repo)
if (!require(doParallel)) install.packages('doParallel', repos = repo)
if (!require(readr)) install.packages('readr', repos = repo)
if (!require(bigstatsr)) install.packages('bigstatsr', repos = repo)
if (!require(rpart)) install.packages('rpart', repos = repo)
if (!require(stats)) install.packages('stats', repos = repo) 
if (!require(ggpubr)) install.packages('ggpubr', repos = repo) 
if (!require(xtable)) install.packages('xtable', repos = repo)
if (!require(data.table)) install.packages('data.table', repos = repo)




