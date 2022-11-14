# GENERATE ALL DATA FILES
# - generate artificial samples
# - process real data


# arguments: 
# -type: "artif", "words"
# -which artif

source("aux_functions.R") 
args = commandArgs(trailingOnly=TRUE)
args <- strsplit(args, ",")
if (length(args) == 0) args[[1]] == "words" else if (length(args) == 1) args[[2]] = artif_models


# ARTIF
# set values    
n <- 20  
N <- 10000     # sample size
q=0.2
q1=0.5
q2=0.1
dstar=4
gamma=1.6


if (args[[1]] =="artif") {
  # ARTIFICIAL
  artificial_data <- lapply(args[[2]], function(model) {
    print(model)
    init <- Sys.time()
    print(init)
    d <- SimData(model,n,q,q1,q2,dstar,gamma,N)
    print(Sys.time()-init)
    write.csv(d,paste("data/artificial/",model,"_",N,".csv",sep=""))
    d
  })

} else {
  # REAL
  ## create dfs of the collections, with every language's dd table
    real_data <- lapply(COLLS, function(coll){
      suffix <- ifelse(args[[1]]=="words","",paste("_",args[[1]],sep=""))
      chunk <- ifelse(args[[1]]=="words",F, args[[1]])
      print(chunk)
      init <- Sys.time()
      print(init)
      tab <- ReadColl(coll,chunk)
      print(Sys.time()-init)
      file <- file.path(paste0("data/real/collections/",coll,suffix,".csv"))
      write.csv(tab,file,row.names = FALSE)
    })
}



## TEST BINARY SEARCH
#library(gtools)
#
#dmax <- 19
#model <- '4'
#x <- 1:dmax
#p <- switch(model,'3'=model3(x,q1,q2,dstar),  '4'=model4(x,q1,q2,dstar,dmax),
#            '6'=model6(x,gamma,q,dstar),'7'=model7(x,gamma,q,dstar,dmax))
#P <- cumsum(p$y_pred)
#
#fun <- function(X) {
#  u <- runif(1)
#  ifelse(P[X] > u, 1, 0)
#}
#
#sampleN <- integer()
#while(length(sampleN) < N) {
#  new_d <- binsearch(fun, range = c(1, 19))$where
#  sampleN <- append(sampleN,new_d)
#}
#
#real_df <- TableCumul(sampleN,cumul=F)








