read_signatures <- function(filename){
  
  gsc.file <- scan(filename,what = "", sep="\n")
  gsclist<- strsplit(gsc.file, "[[:space:]]+")
  names(gsclist) <- sapply(gsclist, `[[`, 1)
  gsclist <- lapply(gsclist, `[`, -1)
  uniqueList <- lapply(gsclist, unique)
  require("GSEABase")
  makeSet <- function(geneIds, n) {GeneSet(geneIds, geneIdType=SymbolIdentifier(), setName=n)}
  gsclist <- mapply(makeSet, uniqueList[], names(gsclist))
  gsc <- GeneSetCollection(gsclist)
  return(gsc)
}


add_annotation <- function(eset){

  platforms_map <- read.csv("data/platforms_map.csv")
  annotation <- platforms_map[match(annotation(eset),platforms_map[,1]),2]
  annotation(eset) <- paste(as.character(annotation),".db", sep="")
  return(eset)
}




applyGSVA <- function(edata,geneset){
es<-gsva(edata, geneset, min.sz=3, max.sz=500, verbose=TRUE)$es.obs    
}




formatesdf <- function(esdf){
  
  # store the numer of signatures

  sig_num <- nrow(exprs(esdf))+1 
  # convert to data frame
  esdf<-data.frame(esdf)
  esdf<-cbind(rownames(esdf),esdf)
  rownames(esdf)<-1:nrow(esdf)
  
  # Identify columns that do not hold identical entries (colind1) 
  # (cannot be experimental condition)
  
    colind<-numeric()

    for (i in (sig_num+1):ncol(esdf)){
      
      if(length(unique(esdf[,i]))!=1)
      colind[length(colind)+1]=i   
  }
  
  #some experiments don't hold varying conditions, check
  if (length(colind)>0) #remove columns only if there are any
  esdf <- esdf[,c(1:sig_num,colind)]
  
}

showboxplot<-function(indata, inx, iny) {
  dat <- indata
  
  
  condition <- dat[,ncol(dat)]
  
  
  p <- ggplot(dat, aes(condition, y=dat[,iny]), environment = environment())
  p <- p + geom_boxplot()
  p <- p + geom_jitter(aes(size=1,colour=factor(condition)),position = position_jitter(width = 0.2))
  p <- p + ylab(colnames(dat)[iny]) + xlab(colnames(dat)[inx])
  print(p)
}

showplot<-function(indata, inx, iny) {
  
  dat <- indata
  condition <- dat[,ncol(dat)]
  
  
  p <- ggplot(dat, aes(condition, y=dat[,iny]), environment = environment())
  p <- p + geom_dotplot()
 # p <- p + geom_jitter(aes(size=1,colour=factor(condition)),position = position_jitter(width = 0.2))

 # p <- p + ylab(colnames(dat)[iny]) + xlab(colnames(dat)[inx])
  print(p)
}
