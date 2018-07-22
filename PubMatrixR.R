##libraries used
library(pbapply)
library(plotly)
library(stringr)
library(rvest)

A<-c() #a list of terms to search against B
B<-c() #a list of terms to search against A

# variables include A and B which are
# lists of terms to do pairwise searches with
# API.key is an entrez eutils key which enables more searches per second 
#(however this function does not seem to exceed the normal usage of 3 requests/second)
# Database is either 'pubmed' or 'pmc' (I do not have the dates coded yet for pmc useage so it may be iffy)
# daterange takes in two concatenated years if you would like to filter the search by a range of dates (example: c(2012,2017) )

PubMatrix<-function(A,B,API.key=NULL,Database='pubmed',daterange=NULL){
  search_list<-sapply(A, function(x) sapply(B,function(y) paste(x,y,sep='+AND+')) )
  search_list<-gsub(' ','+',search_list)
  url<-paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", 
             "db=",Database)
  result_url<-paste0('<b><a href="https://www.ncbi.nlm.nih.gov/',Database,'/?term=')
  
  if(!is.null(API.key)){
  url<-paste0(url,'&api_key=',API.key)  
  }
  if(!is.null(daterange)){
    mindate<-daterange[1]
    maxdate<-daterange[2]
  url<-paste0(url,'&datetype=pdat','&mindate=',mindate,'&maxdate=',maxdate)
  result_url<-paste0(result_url,mindate,':',maxdate,'[DP]+AND+')

  }
  print(url)
  print(result_url)
  z<-unlist(pblapply(search_list, function(x) as.numeric(str_extract_all(read_html(paste0(url,"&term=",x,"&usehistory=y")), '(?<=<esearchresult><count>)\\d+')) ))
  
  result_matrix<-matrix(z, nrow=length(B),ncol=length(A))
  searchterm_matrix<-matrix(paste(result_url,search_list,'"style="color:#f44242">',unlist(result_matrix),'</a></b>',sep=''),nrow=length(B),ncol=length(A))
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
