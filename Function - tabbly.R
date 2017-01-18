

# Sapply table on all the factor-columns in a dataframe
# Equivalent to mapply table only on factor-columns

tabbly<-function(x) {
        v<-vector()
        for(i in names(x)) {
                if(is.factor(x[,(i)])) {
                v<-c(v, (i))        
                }
        }
        sapply(x[,v], table)
}
