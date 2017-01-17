

# Sapply table on factor-columns in a dataframe

tabbly<-function(x) {
        v<-vector()
        for(i in names(x)) {
                if(is.factor(x[,(i)])) {
                v<-c(v, (i))        
                }
        }
        sapply(x[,v], table)
}