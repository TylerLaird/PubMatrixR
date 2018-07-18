A<-c()
B<-c()
PubMatrix<-function(A,B){require(pbapply,plotly,xml2,stringer)
search_list<-sapply(A, function(x) sapply(B,function(y) paste(x,y,sep='+AND+')) )
search_list<-gsub(' ','+',search_list)
z<-unlist(pblapply(search_list, function(x) as.numeric(str_extract_all(read_html(paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", 
            "db=pubmed&term=", x, "&usehistory=y", sep = "")), '(?<=<esearchresult><count>)\\d+')) ))
#z<-unlist(lapply(search_list, function(x) as.numeric(get_pubmed_ids(x)$Count) ))
result_matrix<-matrix(z, nrow=length(B),ncol=length(A))
searchterm_matrix<-matrix(paste('<b><a href="https://www.ncbi.nlm.nih.gov/pubmed/?term=',gsub(' ','+',search_list),'"style="color:#f44242">',unlist(result_matrix),'</a></b>',sep=''),nrow=length(B),ncol=length(A))
m <- list(
  l = 200,
  r = 50,
  b = 200,
  t = 50,
  pad = 4)
p <- plot_ly(
  x = A, y = B,
  z = result_matrix, type = "heatmap") %>%
  layout(margin=m) %>% add_annotations(x=rep(0:(length(A)-1), each=length(B)),
                      y=rep(seq(0,length(B)-1),length(B)),
                      text=unlist(searchterm_matrix), showarrow=F)
p  
}

PubMatrix(A,B)
