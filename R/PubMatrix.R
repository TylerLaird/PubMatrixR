#'PubMatrix
#'
#' This function tallies boolean search results on PubMed using the Entrez search parameters
#'
#' @param file Path to the input file where two search lists are separated by comment character
#' @param A A vector of terms to search against B assuming file is not used
#' @param B A vector of terms to search against A assuming file is not used
#' @param API.key An API key obtained from Entrez; not necessary
#' @param Database Either pubmed or pmc
#' @param daterange a range of dates to search if desired
#' @param outfile A file path to export the search matrix as an .xlsx file. 
#'
#' @return A search matrix
#' @export
PubMatrix<-function(file,A=NULL,B=NULL,API.key=NULL,Database='pubmed',daterange=NULL,outfile=NULL){
  if(is.null(A) & is.null(B)){
    file<-readLines(file,warn=F)
    A<-file[1:which(file=='#')-1]
    B<-file[(which(file=='#')+1):length(file)]
  }
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
  z<-unlist(pbapply::pblapply(search_list, function(x) as.numeric(stringr::str_extract_all(xml2::read_html(paste0(url,"&term=",x,"&usehistory=y")), '(?<=<esearchresult><count>)\\d+')) ))
  
  result_matrix<-matrix(z, nrow=length(B),ncol=length(A))
  searchterm_matrix<-matrix(paste(result_url,search_list,'"style="color:#f44242">',unlist(result_matrix),'</a></b>',sep=''),nrow=length(B),ncol=length(A))
  p <- plotly::plot_ly(
    x = A, y = B,
    z = result_matrix, type = "heatmap") %>%
    plotly::layout(margin=list(l = 200,r = 50,b = 200,t = 50,pad = 4)) %>% plotly::add_annotations(x=rep(0:(length(A)-1), each=length(B)),
                                         y=rep(seq(0,length(B)-1),length(B)),
                                         text=unlist(searchterm_matrix), showarrow=F)
  print(p)
  
  if(!is.null(outfile)){
    result_url_xlsx<-paste0('https://www.ncbi.nlm.nih.gov/',Database,'/?term=')
    url_matrix<-matrix(paste0(result_url_xlsx,search_list),nrow=nrow(result_matrix),ncol=ncol(result_matrix))
    wb <- xlsx::createWorkbook()
    sheet1 <- xlsx::createSheet(wb, "Sheet1")
    rows   <- xlsx::createRow(sheet1, 1:(nrow(result_matrix)+1))            
    cells  <- xlsx::createCell(rows, colIndex=1:(ncol(result_matrix)+1))
    for(i in 1:length(B)){
      cell<-cells[[i+1,1]]
      xlsx::setCellValue(cell, B[i] )
    }
    for(i in 1:length(A)){
      cell<-cells[[1,i+1]]
      xlsx::setCellValue(cell, A[i] )
    }
    for(i in 1:nrow(result_matrix)){
      for(j in 1:ncol(result_matrix)){
        print(paste(i,j))
        cell <- cells[[i+1,j+1]]
        address<-url_matrix[i,j]
        xlsx::setCellValue(cell, result_matrix[i,j]) 
        xlsx::addHyperlink(cell, address)
        
      }
      
    }
    xlsx::saveWorkbook(wb, outfile)
    
  }
}




