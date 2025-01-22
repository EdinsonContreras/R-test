library(tidyverse)
library(ggplot2)

media= function(x) {
  sum(x)/length(x)
}
#https://r-coder.com/table-r/
#https://r-coder.com/which-max-which-min-r/
#https://www.statology.org/names-function-in-r/
moda= function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

mediana= function(x){
  n <-length(x)
  s <-sort(x)
  ifelse(n%%2==1,s[(n+1)/2],media(s[n/2+0:1]))
}


x <- c(2,4,3,6,3,7,5,8,10)


moda(x)

media(x)

mediana(x)
