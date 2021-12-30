print('5.  - EDA: Heatmap & Contingency Table')

library(data.table)

aux=fread('df_sml.csv')
ctype = aux$ctype
dim(df_sml)
head(df_sml[,c(1:5,995:1001)])

aux$ctype <- match(aux$ctype, c(unique(aux$ctype))) - 1
aux$condt <- match(aux$condt, c(unique(aux$condt))) - 1

png("df_sml_image.png",width=7, height = 7,units='in',res=300)
heatmap(as.matrix(aux[2:length(aux)]))
dev.off()

a <- table(aux$ctype, aux$condt)
a <- cbind(a,rowSums(a))
a <- rbind(a,colSums(a))
colnames(a) <- c('Control','ILD','Total')
rownames(a) <- c(unique(ctype),'Total')

fwrite(as.data.frame(a), "condt_vs_ctype.csv",row.names=TRUE)
xtable(a)
