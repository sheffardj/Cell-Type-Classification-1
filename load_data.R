print('2. loading data')
set.seed(0)

library(readr)
library(xtable)
library(data.table)

githubURL <- "https://github.com/lenosr/project556/blob/main/project556.rds?raw=true"
dat <- readRDS(url(githubURL, method="libcurl"))
count <- t(as.data.frame(dat$count))
condt <- as.matrix(dat$condt)    
ctype <- as.matrix(dat$celltype) 

condt_enc <- match(condt, c(unique(condt))) - 1  #encode illness/control
ctype_enc <- match(ctype, c(unique(ctype))) - 1  #ctype encoding -> 0:4

fwrite(as.list(condt_enc), 'condt_enc.csv')
fwrite(as.list(ctype_enc), 'ctype_enc.csv')

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
