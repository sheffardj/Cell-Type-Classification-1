print('3.  - Loading Data')
set.seed(0) # just making sure seed is set over and over and over

library(readr) # I also load libraries as required per , in case.
library(xtable)
library(data.table)

dat <- readRDS('project556.rds')
count <- t(as.data.frame(dat$count))
condt <- as.matrix(dat$condt)    
ctype <- as.matrix(dat$celltype) 

rm(dat)
invisible(gc())

df <- data.frame(cbind(condt, count, ctype))

rm(count)
invisible(gc())

df <- df[, sapply(df, function(col) length(unique(col))) > 1]  #remove cst cols
colnames(df)[c(1,length(df[1,]))] <- c('condt','ctype') #need to rename

fwrite(df,'df.csv')

rm(df)
invisible(gc())

#head(df[,c(1:5,(num_feats-4):num_feats)])  ##CHECK
