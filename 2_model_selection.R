# PERFORM MODEL SELECTION AND WRITE FILES

# arguments:

# -type: "all", "artif", "words"
# -artif model

source("aux_functions.R")
args = commandArgs(trailingOnly=TRUE)
args <- strsplit(args, ",")
if (length(args)==0) {
  print("Choose one of: 'artif','words'")
} else if (length(args)==1) {
  args[[2]] = artif_models
}



# ------------------------------------------------------------------------------
# CHECK FORMULAS
e <- 10^-6

## truncated models: should sum to 1 for any max(d)=dmax
maxd_list <- seq(3,60,10)
list <- sapply(maxd_list, function(maxd) {
  stopifnot(
    between(sum(model0(1:maxd,maxd+1)$y_pred),1-e,1+e),
    between(sum(model2(1:maxd,runif(1,0,1),maxd)$y_pred),1-e,1+e),
    between(sum(model4(1:maxd,runif(1,0,1),runif(1,0,1),as.integer(runif(1,2,maxd-1)),maxd)$y_pred),1-e,1+e),
    between(sum(model5(1:maxd,maxd,runif(1,0,10))$y_pred),1-e,1+e),
    between(sum(model7(1:maxd,runif(1,0,10),runif(1,0,1),as.integer(runif(1,2,maxd-1)),maxd)$y_pred),1-e,1+e)
  )
})

## non-truncated models: should get to 1 soon enough 
d <- 1:100
stopifnot(
  between(sum(model1(d,runif(1,0,1))$y_pred),1-e,1+e),
  between(sum(model3(d,runif(1,0,1),runif(1,0,1),as.integer(runif(1,2,max(d)-1)))$y_pred),1-e,1+e),
  between(sum(model6(d,runif(1,0,10),runif(1,0,1),as.integer(runif(1,2,max(d)-1)))$y_pred),1-e,1+e)
)

print("Every probability distribution sums to 1! We can go on with the analysis.")



# ------------------------------------------------------------------------------
# MODEL SELECTION

if (args[[1]] %in% c("all","artif")) {
  # ARTIF
    # compute model selection tables
    art_df <- RunModelSelection(args[[2]],"artif")
    #if (all(args[[2]]==artif_models)) write.csv(art_df,"results/artificial/ms_results.csv", row.names = F)
}
if (args[[1]] %in% c("all","words")) {
  # REAL
    real_dfs <- lapply(COLLS, function(collection) {
      # compute model selection tables
      RunModelSelection(ISO,"words",collection)
    })
   ms_results <- do.call(rbind.data.frame,real_dfs)
   #write.csv(ms_results,"results/real/mixed_n/ms_results.csv", row.names = F)
}


