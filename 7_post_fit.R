set.seed(0) #cause why not

library(data.table)
library(doParallel)
library(foreach)
library(bigstatsr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(xtable)
library(dplyr)

df_sml <- fread('df_sml.csv',header=T) %>% as.data.frame()
#preds  <- fread('cv_model_predictions.csv',header=T) %>% as.data.frame()

### vvv really this is "preds2"
preds  <- fread('cv_model_predictions2.csv',header=T) %>% as.data.frame()

num_rows <- dim(df_sml)[1]
num_cols <- dim(df_sml)[2]


#### Prelims & Load required data ####
n_CVs = dim(preds)[1]/4954 ### retrieve n_CVs specified elsewhere
nfold = 10                 ### constant through project
n_models = 8               ### also constant alpha = 0.1, 0.3, ..., 0.9, 1(LASSO) 
                           ###   as well as LDA and TREE (pruned)


#### Calculating Criteria Scores ####
print('7.1 - Calculating Criteria Scores')

acc_models <- matrix(NA, ncol = n_models, nrow = n_CVs)
pre_models <- acc_models
rec_models <- acc_models
f11_models <- acc_models

acc_models <- as_FBM(acc_models)
pre_models <- as_FBM(pre_models)
rec_models <- as_FBM(rec_models)
f11_models <- as_FBM(f11_models)


registerDoParallel(detectCores())
tmp <-
  foreach(cv_set = 1:n_CVs, .combine = 'c', .packages = c("foreach")) %:% 
    foreach(model = 1:n_models, .combine = 'c') %dopar% {
      
      cv_row_set <- 1:num_rows + (cv_set - 1) * num_rows
      a <- table(df_sml$ctype, preds[cv_row_set, model])
      
      micro.pre <- diag(a) / colSums(a)
      micro.rec <- diag(a) / rowSums(a)
      b <- as.data.frame(cbind(unique(df_sml$ctype), micro.pre, micro.rec))
      b[,2:3] <- apply(b[,2:3], 2, as.numeric)
      
      acc_models[cv_set, model] <- sum(diag(a)) / sum(a)
      
      pre_models[cv_set, model] <- sum(as.numeric(b[, 2])) / 5
      
      rec_models[cv_set, model] <- sum(as.numeric(b[, 3])) / 5
      
      f11 <- 2 * (b[, 2] * b[, 3]) / (b[, 2] + b[, 3])
      
      f11_models[cv_set, model] <- sum(f11)/length(f11) 
      
    }
stopImplicitCluster()

rm(tmp, preds)
invisible(gc())

scores <- cbind(acc_models[], 
                 pre_models[],
                 rec_models[],
                 f11_models[]) %>% as.data.frame()

rownames(scores) <- c(paste0('CV',1:n_CVs))
colnames(scores) <- c(paste0(c(paste0('a=0.',c(1,3,5,7,9)),'a=1.0','LDA','TREE'),'.a'),
                      paste0(c(paste0('a=0.',c(1,3,5,7,9)),'a=1.0','LDA','TREE'),'.p'),
                      paste0(c(paste0('a=0.',c(1,3,5,7,9)),'a=1.0','LDA','TREE'),'.r'),
                      paste0(c(paste0('a=0.',c(1,3,5,7,9)),'a=1.0','LDA','TREE'),'.f'))

write.csv(scores,'scores.csv')

#scores[,c(seq(7,32,8), seq(8,32,8))] <- scores[,c(seq(7,32,8), seq(8,32,8))] - 0.1
#==============================================================================#
print('7.2 - Format Data for ggplot')
ptm_plots <- proc.time()[[3]]


ord <- c(seq(1,32,8),seq(2,32,8),seq(3,32,8),seq(4,32,8),
         seq(5,32,8),seq(6,32,8),seq(7,32,8),seq(8,32,8)) #reorder by model, not criteria

melted <- reshape2::melt(scores[,ord]) #melt for plotting

model_id <- c(rep('a=0.1', 4*n_CVs),
              rep('a=0.3', 4*n_CVs), #4 critera with 'n_CVs' observations each
              rep('a=0.5', 4*n_CVs),
              rep('a=0.7', 4*n_CVs),
              rep('a=0.9', 4*n_CVs),
              rep('a=1.0', 4*n_CVs),
              rep('LDA',   4*n_CVs),
              rep('TREE',  4*n_CVs))

criteria_id <- c(rep('a', n_CVs),         
                 rep('p', n_CVs),
                 rep('r', n_CVs),
                 rep('f1', n_CVs))

melted <- cbind(melted,model_id)
melted <- cbind(melted, criteria_id)
melted_tree <- melted[model_id == 'TREE',] #STORE TREE
melted <- melted[model_id != 'TREE',]      # THEN REMOVE TREE!!
melted$model_id2 <- melted$model_id
elnet_no <- length(melted$model_id[melted$model_id %in% c(paste0('a=0.',seq(1,9,2)))] )
melted$model_id2[1:elnet_no] <- "El.Net"
melted$model_id2[melted$model_id2 == 'a=1.0'] <- "LASSO"
#==============================================================================#
#### Individual Plots ####

acc <- melted[melted$criteria_id=='a',]
pre <- melted[melted$criteria_id=='p',]
rec <- melted[melted$criteria_id=='r',]
f1s <- melted[melted$criteria_id=='f1',]

#===================================================#
## PLOTS: ACCURACY AND F1-SCORE ##

print('7.3 - Arranging Plots')

plt_a <- ggplot(acc, aes(model_id, value, fill = model_id2)) +
  geom_boxplot(
    outlier.shape = 'o',
    notch = FALSE,
    width = 0.5,
    lwd = 0.3,
    varwidth = TRUE
  ) +
  theme_light() + xlab("") +
  ylab("Criteria Score") +
  labs(fill = "Model") +
  stat_boxplot(geom = 'errorbar',
               width = 0.5,
               lwd = 0.3) +
  scale_fill_brewer(palette = 'Set2') +
  ggtitle("Accuracy") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5, size=12, face='bold.italic'),
        legend.position = "none") 


plt_f <- ggplot(f1s, aes(model_id, value, fill = model_id2)) +
  geom_boxplot(
    outlier.shape = 'o',
    notch = FALSE,
    width = 0.5,
    lwd = 0.3,
    varwidth = TRUE
  ) +
  theme_light() + xlab("") +
  ylab("") +
  labs(fill = "model", colour = "drug") +
  stat_boxplot(geom = 'errorbar',
               width = 0.5,
               lwd = 0.3) +
  scale_fill_brewer(palette = 'Set2') +
  ggtitle("F1 Score") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5, size=12, face='bold.italic'),legend.position='none')


#=========================================================#
## PLOTS: PRECISION AND RECALL ##

plt_p <- ggplot(pre, aes(model_id, value, fill = model_id2)) +
  geom_boxplot(
    outlier.shape = 'o',
    notch = FALSE,
    width = 0.5,
    lwd = 0.3,
    varwidth = TRUE
  ) +
  theme_light() + xlab("") +
  ylab("Criteria Score") +
  labs(fill = "Model") +
  stat_boxplot(geom = 'errorbar',
               width = 0.5,
               lwd = 0.3) +
  scale_fill_brewer(palette = 'Set2') +
  ggtitle("Precision") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5, size=12, face='bold.italic'),
        legend.position = "none") 


plt_r <- ggplot(rec, aes(model_id, value, fill = model_id2)) +
  geom_boxplot(
    outlier.shape = 'o',
    notch = FALSE,
    width = 0.5,
    lwd = 0.3,
    varwidth = TRUE
  ) +
  theme_light() + xlab("") +
  ylab("") +
  labs(fill = "Model") +
  stat_boxplot(geom = 'errorbar',
               width = 0.5,
               lwd = 0.3) +
  scale_fill_brewer(palette = 'Set2') +
  ggtitle("Recall") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5, size=12, face='bold.italic'),
        legend.position='none')


#==========================================================#
## Do 4-by Plots#

plt_nice <- ggarrange(plt_a, plt_f, plt_p, plt_r, ncol=2, nrow=2, 
                      common.legend = TRUE, legend="bottom")

plt_nice <- annotate_figure(
  plt_nice,
  top = text_grob("Celltype Annotation Classification Models",size=14),
  bottom = NULL,
  left = NULL,
  right = NULL,
  fig.lab = NULL,
  fig.lab.pos = c("top")
)


ggsave(
  'plt_nice.png',
  plot   = plt_nice,
  device = 'png',
  width  = 11,
  height = 7,
  units  = 'in',
  dpi    = 300
)

rm(plt_a, plt_f, plt_p, plt_r,
   acc, f1s, pre, rec)

#=============================================================================#
#### PLOT TREE DATA ####

plt_t <- ggplot(melted_tree, aes(criteria_id, value,fill = criteria_id)) +
  geom_boxplot(
    outlier.shape = 'o',
    notch = FALSE,
    width = 0.5,
    lwd = 0.3,
    varwidth = TRUE
  ) +
  theme_light() + xlab("Tree Criteria") +
  ylab("Criteria Score") +
  labs(fill = "Tree Critera") +
  stat_boxplot(geom = 'errorbar',
               width = 0.5,
               lwd = 0.3) +
  scale_fill_brewer(palette = 'Set2') +
  ggtitle("Boxplots for Classification Tree") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5, size=12, face='bold.italic'),
        legend.position = "bottom") 

ggsave(
  'plt_t.png',
  plot   = plt_t,
  device = 'png',
  width  = 5,
  height = 5.5,
  units  = 'in',
  dpi    = 300
)

#=============================================================================#
#### MODEL SIGNIFICANCE TESTING ####
print('7.4 - Comparing Models by Criteria')

mvp <- matrix(NA, nrow = n_models, ncol = 12)
colnames(mvp) <- c('Model','Mean Accuracy','p-value',
                   'Model','Mean Precision','p-value',
                   'Model','Mean Recall','p-value',
                   'Model','Mean F1 Score','p-value')

a <- scores[, 1:n_models]
p <- scores[, 1:n_models + n_models*1]
r <- scores[, 1:n_models + n_models*2]
f <- scores[, 1:n_models + n_models*3]

a.id <- sort(apply(a, 2, mean), decreasing = TRUE)
p.id <- sort(apply(p, 2, mean), decreasing = TRUE)
r.id <- sort(apply(r, 2, mean), decreasing = TRUE)
f.id <- sort(apply(f, 2, mean), decreasing = TRUE)

a.sorted <- a[, names(a.id)]
p.sorted <- p[, names(p.id)]
r.sorted <- r[, names(r.id)]
f.sorted <- f[, names(f.id)]

mvp[, 1] <- colnames(a.sorted)
mvp[, 2] <- a.id
mvp[, 4] <- colnames(p.sorted)
mvp[, 5] <- p.id
mvp[, 7] <- colnames(r.sorted)
mvp[, 8] <- r.id
mvp[,10] <- colnames(f.sorted)
mvp[,11] <- f.id

for(i in 2:n_models){
  mvp[i, 3] <- 
    wilcox.test((a.sorted[, 1] - a.sorted[, i]))$p.value
  mvp[i, 6] <- 
    wilcox.test((p.sorted[, 1] - p.sorted[, i]))$p.value
  mvp[i, 9] <- 
    wilcox.test((r.sorted[, 1] - r.sorted[, i]))$p.value
  mvp[i,12] <- 
    wilcox.test((f.sorted[, 1] - f.sorted[, i]))$p.value
  
}

mvp <- as.data.frame(mvp)

xtable(mvp,digits=4)

fwrite(mvp,'mean_values_p_values.csv')