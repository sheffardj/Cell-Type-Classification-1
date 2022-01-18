print('3. feature selection underway')
set.seed(0)

library(bigstatsr)
library(foreach)
library(doParallel)
library(caret)
library(stats)


#####  KRUSKAL WALLIS TEST (BETWEEN NUMERICAL PREDICTORS AND OUTCOME) ####
df <- fread('df.csv') %>% as.data.frame()   #retrieve data
num_cols <- dim(df)[2]

# Filebacked Matrix for foreach loop, consider only gene expressions (17308)
p_vals <- FBM(nrow = (num_cols - 2), ncol = 1, init = NA) 

registerDoParallel(detectCores())
t <- foreach(i = 2:(num_cols - 1), .combine = 'c') %dopar% {
  dat_total = cbind(df[, i], df$ctype) #order doesn't matter
  p_vals[i - 1, 1] <-
    kruskal.test(df$ctype ~ df[, i], data = dat_total)$p.value
}
stopImplicitCluster()
rm(t)
invisible(gc())

p_vals <- p_vals[] #restore matrix from FBM
print(paste('Range of P-Values:',min(p_vals),'to',max(p_vals)))

png("hist_p_vals.png",width=4, height = 6,units='in',res=300)
par(mfrow=c(1,3),mar=c(4.1,4.1,2.1,0.5))
hist(p_vals,breaks=50,main='All P-Values',xlab='P-Values')
hist(p_vals[p_vals<0.00005],breaks=50,xlab='P-Values',ylab='',main='P-Vals < 5e-05')
plot(2:17309,sort(p_vals),type='l',main='Sorted P-values',xlab='index')
dev.off()


#==============================================================================#
#### CORRELATION FOR FEATURE REMOVAL ####
print('4. Finding correlation')

number_of_p_values_to_select = length(p_vals[p_vals < 0.005]) #1645 values
best_ps <- match(sort(p_vals)[1:number_of_p_values_to_select],p_vals) #find p_vals indices ; pval<0.005
df2 <- as.data.frame(df[,2:(dim(df)[2]-1)]) #just consider 'count' data so col 1 ~ p_val id 1

rm(p_vals)
invisible(gc())

best_ps_of_count <- cor(df2[,best_ps]) #subset count with best_ps

# Next, remove correlations outside -0.25:0.25 
# the middle quartile of possible correlations: [-1,1]
corrs_to_remove <- findCorrelation(best_ps_of_count,cutoff=0.25, exact=TRUE) #leaves 1065. #removing 580
df3 <- df2[, best_ps] #subset df2 for 
df3 <- df3[,-corrs_to_remove]  #now remove highly correlated columns

rm(df2) #cleanup memory allocation
rm(best_ps)
rm(best_ps_of_count)
rm(corrs_to_remove)
invisible(gc())


#### FEATURE REDUCED MATRIX ####
print('5. Reduce df to df_sml; a feature reduced dataframe')
df_sml <-  as.data.frame(cbind(condt = df$condt, df3, ctype = df$ctype))

rm(df3)
rm(df)
invisible(gc())

reduced_mat_corr <- cor(df_sml[,2:(dim(df_sml)[2]-1)])
print(paste('Range of correlations:',
            min((reduced_mat_corr)), 'to',
            max((reduced_mat_corr[reduced_mat_corr!=1]))))
print(paste('Reduced Dimensions:',dim(df_sml)[1],'rows x',dim(df_sml)[2],'cols'))

rm(reduced_mat_corr)
invisible(gc())

df_sml[,1] <- match(df_sml[,1], c(unique(df_sml$condt)))-1

fwrite(df_sml,'df_sml.csv')


#==============================================================================#
#### consider for feature selection : multinomial logistic regression ####


