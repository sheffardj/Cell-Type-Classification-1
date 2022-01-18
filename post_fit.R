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


df_sml <- fread('df_sml.csv',header=T) %>% as.data.frame()
preds  <- fread('cv_model_predictions.csv',header=T) %>% as.data.frame()

#preds <- fread('~/CC_R/cv_model_predictions_10cvs.csv') %>% as.data.frame()
#preds<-preds[,-1]
###TEMP ^^^^^

num_rows <- dim(df_sml)[1]
num_cols <- dim(df_sml)[2]


#### Prelims & Load required data ####
n_CVs = dim(preds)[1]/4954 ### retrieve n_CVs specified elsewhere
nfold = 10
n_models = 8


#### Calculating Criteria Scores ####
print('8. Calculating Criteria Scores')

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

#==============================================================================#
print('9. Format Data for ggplot')
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

#==============================================================================#
#### Individual Plots ####

acc <- melted[melted$criteria_id=='a',]
pre <- melted[melted$criteria_id=='p',]
rec <- melted[melted$criteria_id=='r',]
f1s <- melted[melted$criteria_id=='f1',]

#===================================================#
## PLOTS: ACCURACY AND F1-SCORE ##


plt_a <- ggplot(acc, aes(model_id, value, fill = model_id)) +
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


plt_f <- ggplot(f1s, aes(model_id, value, fill = model_id)) +
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

plt_p <- ggplot(pre, aes(model_id, value, fill = model_id)) +
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


plt_r <- ggplot(rec, aes(model_id, value, fill = model_id)) +
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
## Do Plot#
print('10. Plotting')

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
#### MODEL SIGNIFICANCE TESTING ####
print('11. Compare Models by Criteria')

mvp <- matrix(NA, nrow = n_models, ncol = 12)
colnames(mvp) <- c('Model','Mean Accuracy','p-value',
                   'Model','Mean Precision','p-value',
                   'Model','Mean Recall','p-value',
                   'Model','Mean F1 Score','p-value')

a <- scores[, 1:n_models]
p <- scores[, 1:n_models + n_models*1]
r <- scores[, 1:n_models + n_models*2]
f <- scores[, 1:n_models + n_models*3]

a.id <- apply(a, 2, mean)
p.id <- apply(p, 2, mean)
r.id <- apply(r, 2, mean)
f.id <- apply(f, 2, mean)

a.ord <- match(sort(a.id, decreasing = TRUE), a.id)
p.ord <- match(sort(p.id, decreasing = TRUE), p.id)
r.ord <- match(sort(r.id, decreasing = TRUE), r.id)
f.ord <- match(sort(f.id, decreasing = TRUE), f.id)

a.sorted <- a[, a.ord]
p.sorted <- p[, p.ord]
r.sorted <- r[, r.ord]
f.sorted <- f[, f.ord]

mvp[, 1] <- rownames(as.matrix(reshape::melt(sort(a.id), decreasing = TRUE)))
mvp[, 2] <-          as.matrix(reshape::melt(sort(a.id), decreasing = TRUE))
mvp[, 4] <- rownames(as.matrix(reshape::melt(sort(p.id), decreasing = TRUE)))
mvp[, 5] <-          as.matrix(reshape::melt(sort(p.id), decreasing = TRUE))
mvp[, 7] <- rownames(as.matrix(reshape::melt(sort(r.id), decreasing = TRUE)))
mvp[, 8] <-          as.matrix(reshape::melt(sort(r.id), decreasing = TRUE))
mvp[,10] <- rownames(as.matrix(reshape::melt(sort(f.id), decreasing = TRUE)))
mvp[,11] <-          as.matrix(reshape::melt(sort(f.id), decreasing = TRUE))

for(i in 2:n_models){
  mvp[i, 3] <- 
    wilcox.test((a.sorted[, 1] - a.sorted[, i]), correct = TRUE)$p.value
  mvp[i, 6] <- 
    wilcox.test((p.sorted[, 1] - p.sorted[, i]), correct = TRUE)$p.value
  mvp[i, 9] <- 
    wilcox.test((r.sorted[, 1] - r.sorted[, i]), correct = TRUE)$p.value
  mvp[i,12] <- 
    wilcox.test((f.sorted[, 1] - f.sorted[, i]), correct = TRUE)$p.value
  
}

mvp <- as.data.frame(mvp)
mvp <- rbind(mvp,rep(NA,12),rep(NA,12))

xtable(mvp,digits=4)

fwrite(mvp,'mean_values_p_values.csv')