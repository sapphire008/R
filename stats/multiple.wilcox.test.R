multiple.wilcox.test <- function(fm, data, paired=FALSE, correction='bonferroni'){
  # Generate pair-wise comparison
  f = data[[all.vars(fm)[2]]]
  f_comb = t( combn( levels(f), 2) ) # factor cobminations
  d = data[[all.vars(fm)[1]]] # data

  wmw <- c()

  for(i in 1:dim(f_comb)[1]){
    # Select the data
    subres = subset(data, subset= (f==f_comb[i,1] | f==f_comb[i,2]) )
    # Do the stats
    wmw <- c(wmw, wilcox.test(formula=fm, data=subres, paired=FALSE, correct=TRUE)$p.value)
  }

  # Correct for multiple comparison
  wmw <- p.adjust(wmw, method=correction)

  # Make the data frame
  df <- data.frame(Type1=f_comb[,1], Type2=f_comb[,2], p.values=wmw)
  return(df)
}
