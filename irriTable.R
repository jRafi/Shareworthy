
### irriTable

# The problem: Need to output a frequency table from a dataframe with survey scores (or other 
# factor) and an index variable.

# The solution: subset the dataframe so it only contains the desired table row and column, tabulate
# it, convert it to a dataframe and keep the index as a variable.

irriTable<-function(x, tableRow="", tableCol="") {
        df<-as.data.frame.matrix(table(subset(x[,c(tableRow,tableCol)])))
        df<-as.data.frame(cbind(index=rownames(df), df))
        row.names(df)<-NULL
        df
}      