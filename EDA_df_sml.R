print('5. EDA: Heatmap & Contingency Table')

library(data.table)

aux=fread('df_sml.csv')


aux$ctype <- match(aux$ctype, c(unique(aux$ctype))) - 1
aux$condt <- match(aux$condt, c(unique(aux$condt))) - 1

png("df_sml_image.png")
heatmap(as.matrix(aux[2:length(aux)]))
dev.off()

a <- table(ctype_enc, condt_enc)
a <- cbind(a,rowSums(a))
a <- rbind(a,colSums(a))
colnames(a) <- c('Control','ILD','Total')
rownames(a) <- c(unique(ctype),'Total')

fwrite(as.data.frame(a), "condt_vs_ctype.csv",row.names=TRUE)
xtable(a)
