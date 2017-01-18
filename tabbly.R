
# The problem: tabulating all columns in a df returns unneccessary tables (like id:s), while
# tabulating each relevant column separately involves unneccessary typing.

# The solution: tabbly is a function that checks which columns in data.frame x are factors, and then
# sapply:s the table function on those columns. Equivalent to mapply table only on factor-columns.

tabbly<-function(x) {
        v<-vector()
        for(i in names(x)) {
                if(is.factor(x[,(i)])) {
                        v<-c(v, (i))        
                }
        }
        sapply(x[,v], table)
}