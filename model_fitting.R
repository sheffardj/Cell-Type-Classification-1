print('7. Model Fitting Underway')
set.seed(0)

library(MASS)
library(class)
library(glmnet)
library(tree)
library(dplyr)
library(foreach)
library(doParallel)
library(bigstatsr)
library(rpart)
library(caret)


n_CVs = 100
nfold = 10
n_models = 8 #elastic net alpha = 0.1, 0.3, 0.5, 0.7, 0.9, LASSO, LDA, TREE


# read in feature reduced data frame
df_sml <- fread('df_sml.csv',header=T) %>% as.data.frame()

num_rows <- dim(df_sml)[1]
num_cols <- dim(df_sml)[2]

idx <- matrix(NA, nrow = n_CVs, ncol = dim(df_sml)[1]) #make n_CVs fold-indexes
for(ii in 1:n_CVs){idx[ii,] <- createFolds(df_sml$ctype, k = nfold, list=F)}
fwrite(as.data.frame(idx),"idx.csv") #store for later


# functions to retrieve fold and cross validation indices inside 'foreach'
y.train <- function(fold, cv) {
  match(df_sml[idx[cv, ] != fold, num_cols], c(unique(df_sml$ctype))) - 1
}
y.test  <- function(fold, cv) {
  match(df_sml[idx[cv, ] == fold, num_cols], c(unique(df_sml$ctype))) - 1
}
x.train <- function(fold, cv) {
  as.matrix(df_sml[idx[cv, ] != fold,-num_cols])
}
x.test  <- function(fold, cv) {
  as.matrix(df_sml[idx[cv, ] == fold,-num_cols])
}

# required imports to 'foreach'
load_packs <- c('foreach', 'glmnet','MASS','class','rpart','stats')

preds <- FBM(ncol = n_models, nrow = n_CVs * dim(df_sml)[1], init = NA)

#==============================================================================#
#### PARALLEL LOOP ####
print('7.1) Fitting 5 Elastic Nets and LASSO')


alpha_seq <- c(seq(0.1,0.9,0.2),1)

#lasso on glmnet noncv took 2.69mins
#lasso on cv.glmnet took 5.54 minutes with parallel = False
#lasso on cv.glmnet took 5.36 minutes with parallel = True  ### negligible

time_test <- proc.time()[[3]]
registerDoParallel(detectCores())
t<-foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:% 
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    foreach(alpha = alpha_seq) %do% { #alpha_seq
      
      fit <- cv.glmnet(
        x.train(fold, cv), #check parallel = T
        y.train(fold, cv),
        alpha = 1,
        family = 'multinomial',
        parallel = TRUE
      )
      
      
      preds[list_row_set, match(alpha, alpha_seq)] <-
        as.numeric(predict(fit,
                           x.test(fold, cv),
                           s = 'lambda.1se',
                           type = 'class'
        ))    
      
      
    }
    
    preds[list_row_set, 7] <-
      as.numeric(predict(
        lda(y.train(fold, cv) ~ .,
            data = as.data.frame(x.train(fold, cv)))
        ,
        as.data.frame(cbind(x.test(fold, cv),
                            y.test(fold, cv))),
        CV = T
      )$class) - 1
    
    #============##### TREE #####=============#
    
    CLtree <- rpart(
      y.train(fold, cv) ~ .,
      data = as.data.frame(x.train(fold, cv)),
      method = "class")
    
    preds[list_row_set, 8] <-
      as.numeric(predict(prune(CLtree,
                               cp = cp.select(CLtree)),
                         as.data.frame(x.test(fold, cv)),
                         "class")) - 1
  }

print(paste0((proc.time()[[3]]-time_test)/60,'minutes')) #
stopImplicitCluster()

rm(t)
invisible(gc())

#==============================================================================#

preds[,7:8]<-preds[,7:8]-1 # CLASSES 1:5 -> 0:4
preds <- preds[] %>% as.data.frame()
colnames(preds) <- c(paste0('a=0.',c(1,3,5,7,9)),'a=1.0','LDA','TREE')

fwrite(preds,'cv_model_predictions.csv')
rm(preds)

print('Completed Model Fitting!')





#==============================================================================#
#==============================================================================#
