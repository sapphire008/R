# Simple barplot using ggplot
require(ggplot2)

df <- aggregate(cbind(RMP, fAHP) ~ Num + Cell + Drug, data=df.GIRK, FUN=mean)
df_mean <- aggregate(cbind(RMP, fAHP) ~ Drug, data=df, FUN=mean)
serr <- function(x){
  sd(x)/sqrt(length(x))
}
df_serr = aggregate(cbind(RMP, fAHP) ~ Drug, data=df, FUN=serr)
df_final = merge(df_mean, df_serr, by=c("Drug"))


p <- ggplot(df_final, aes(x=Drug, y=RMP.x)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=RMP.x-RMP.y, ymax=RMP.x+RMP.y),
                width=0.2,
                position=position_dodge(0.9))

p