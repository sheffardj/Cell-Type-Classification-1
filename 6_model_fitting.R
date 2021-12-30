print('6.1 - Model Fitting Underway')
set.seed(0)

n_CVs = 10
nfold = 5
n_models = 8 #elastic net alpha = 0.1, 0.3, 0.5, 0.7, 0.9, LASSO, LDA, TREE
print(paste0('    - performing ',nfold,'-fold Cross Validation ', n_CVs, ' times'))

# read in feature reduced data frame
df_sml <- fread('df_sml.csv',header=T) %>% as.data.frame()

num_rows <- dim(df_sml)[1]
num_cols <- dim(df_sml)[2]

idx <- matrix(NA, nrow = n_CVs, ncol = dim(df_sml)[1]) #make n_CVs fold-indexes
for(ii in 1:n_CVs){idx[ii,] <- createFolds(df_sml$ctype, k = nfold, list=F)}
#fwrite(as.data.frame(idx),"idx.csv") #store for later


#==============================================================================#
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
preds2 <- FBM(ncol = 4, nrow = n_CVs * dim(df_sml)[1], init = NA)


### ELNET, LASSO, LDA, CL.TREE
if(0){
#==============================================================================#
#### LASSO AND ELASTIC NET ####

print('6.2 - Fitting 5 Elastic Nets, LASSO, LDA & TREE')

alpha_seq <- c(seq(0.1,0.9,0.2),1)

time_test <- proc.time()[[3]]
registerDoParallel(detectCores())
t<-foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:% 
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    #============##### LASSO/EL.NET #####=============#
    
    foreach(alpha = alpha_seq) %do% { #alpha_seq #runs in sequential (for loop)
      
      fit <- cv.glmnet(
        x.train(fold, cv), #check parallel = T
        y.train(fold, cv),
        alpha = alpha,
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
  }

print(paste0('    - ',(proc.time()[[3]]-time_test)/60,' minutes')) #


rm(t)
invisible(gc())

#==============================================================================#
#### LINEAR DISCRIMINANT ANALYSIS ####

t<-foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:% 
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    #============##### LDA #####=============#
    
    preds[list_row_set, 7] <-
      as.numeric(predict(
        lda(y.train(fold, cv) ~ .,
            data = as.data.frame(x.train(fold, cv)))
        ,
        as.data.frame(cbind(x.test(fold, cv),
                            y.test(fold, cv))),
        CV = T
      )$class) - 1
  }

print(paste0('    - ',(proc.time()[[3]]-time_test)/60,' minutes')) #


rm(t)
invisible(gc())

#==============================================================================#
#### CLASSIFICATION TREE ####

cp.select <- function(big.tree) {
  min.x <- which.min(big.tree$cptable[, 4]) #column 4 is xerror
  for(i in 1:nrow(big.tree$cptable)) {
    if(big.tree$cptable[i, 4] < big.tree$cptable[min.x, 4] + big.tree$cptable[min.x, 5]) 
      return(big.tree$cptable[i, 1]) #column 5: xstd, column 1: cp 
  }
}


t<-foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:% 
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
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

print(paste0('    - ',(proc.time()[[3]]-time_test)/60,' minutes')) #
stopImplicitCluster()

rm(t)
invisible(gc())
} 

#==============================================================================#

foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:%  #NESTED
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #dopar is PARALLEL fold 1,5,2,6,7,3...
    
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    #============##### XGBoost #####=============# Leno
    #fitting to do here
    #
    #
    ####
    
    preds2[list_row_set, 1] <- ## predict fit model here...
  }


#==============================================================================#
registerDoParallel(detectCores())
foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:%  #NESTED
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #dopar is PARALLEL fold 1,5,2,6,7,3...
    
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    
    #============##### Random Forest #####=============# 
    #fitting to do here
    #
    #
    ####
    
    preds2[list_row_set, 2] <- ## predict fit model here...
      
  }
stopImplicitCluster()


#==============================================================================#

foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:%  #NESTED
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #dopar is PARALLEL fold 1,5,2,6,7,3...
    
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    
    
    #============##### NN #####=============#
    #fitting to do here
    #
    #
    ####
    
    preds2[list_row_set, 3] <- ## predict fit model here...
  }


#==============================================================================#

foreach(cv = 1:n_CVs, .combine = 'c', .packages = load_packs) %:%  #NESTED
  foreach(fold = 1:nfold, .combine = 'c') %dopar% {  #dopar is PARALLEL fold 1,5,2,6,7,3...
    
    
    cv_row_set <- 1:num_rows + (cv - 1) * num_rows #i.e. 1:4954 for cv==1
    list_row_set <- cv_row_set[idx[cv,] == fold]   #get fold indices per cv
    
    
    
    #============##### MTPS Stacking #####=============# Dayten
    #fitting to do here
    #
    #
    ####
    
    preds2[list_row_set, 4] <- ## predict fit model here...
  }



#==============================================================================#
#preds[,7:8]<-preds[,7:8]-1 # CLASSES 1:5 -> 0:4
#preds <- preds[] %>% as.data.frame()
#colnames(preds) <- c(paste0('a=0.',c(1,3,5,7,9)),'a=1.0','LDA','TREE')

fwrite(preds,'cv_model_predictions.csv')
rm(preds)

fwrite(preds2,'cv_model_predictions2.csv')
rm(preds2)


print('    - Completed Model Fitting!')



