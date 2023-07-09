source("aux_functions.R")



args = commandArgs(trailingOnly=TRUE)
args <- strsplit(args, ",")
if (length(args)==0) print("Choose one of: 'artif','words'")

suffix <- ifelse(args[[1]]=="words","",paste("_",args[[1]],sep=""))
print('suffix created')



#------------------------------------------------------------------------------
# ----------------------------------- ◊ ARTIFICIAL ◊ ----------------------------------

if (args[[1]]=="artif") {
  artif_path <- "latex_outputs/artificial/"
  
  Bart_df <- read.csv("results/artificial/ms_results.csv",check.names = F)
  Bart_ICdiff <- DfICdiff(Bart_df,'artif')
  Bart_params <- Dfparams(Bart_df)
  
  ## BIC
  latexbic <- Bart_df[1:length(artif_models)] %>% `colnames<-`(c(paste("Model",artif_models))) %>%
              mutate(model = artif_models) 
  latexbic <- latexbic[,c(ncol(latexbic),1:length(artif_models))]   # reorder
  print(xtable(latexbic, type = "latex"), 
        file = paste0(artif_path,"latexbic.tex") ,only.contents=T, include.rownames=FALSE)

  ## parameters
  max_d_artif <- sapply(artif_models,function(model) max(ReadArtif(model)$d) )
  latex_params <- Bart_params %>% mutate(max_d = max_d_artif, best=NULL)
  latex_params <- latex_params[,c(ncol(latex_params)-1,ncol(latex_params),1:length(params_names))]   # reorder
  xt1 <- xtable(latex_params, type = "latex", digits=3)
  colnames(xt1) <- c( "model", "$max(d) $", "$d_{max}$", "$q$", "$q$", "$d_{max}$" , "$q_1$",
                      "$q_2$", "$d^*$", "$q_1$", "$q_2$","$d^*$", "$d_{max}$",
                      "$d_{max}$", "$\\gamma$", "$\\gamma$","$q$","$d^*$","$\\gamma$","$q$","$d^*$","$d_{max}$")
  print(xt1, file = paste0(artif_path,"latex_params.tex"),
        include.rownames=FALSE, only.contents = TRUE,sanitize.text.function = function(x) {x})
  
  
  ## parameters comparison
  estimated <- sapply(artif_models, function(mod) {
    latex_params[latex_params$model==mod,grep(paste(mod,".",sep=""),colnames(latex_params))]
  })
  real <- c(19, 0.2, 0.2, 19, 0.5, 0.1, 4, 0.5, 0.1, 4, 19, 19, 1.6, 1.6, 0.2, 4, 1.6, 0.2, 4, 19)
  df <- do.call(cbind, estimated) %>% rbind(real) %>% rbind(do.call(cbind, estimated) - real)
  df1 <- xtable(df, type = "latex", digits=3)
  colnames(df1) <- c("$d_{max}$", "$q$", "$q$", "$d_{max}$" , "$q_1$",
                     "$q_2$", "$d^*$", "$q_1$", "$q_2$","$d^*$", "$d_{max}$","$d_{max}$", 
                     "$\\gamma$", "$\\gamma$","$q$","$d^*$","$\\gamma$","$q$","$d^*$","$d_{max}$")
  rownames(df1) <- c("estimated","real","error")
  print(df1, file = paste(artif_path,"params_compare.tex",sep=""),caption.placement = "top",
        include.rownames=T, only.contents = TRUE,sanitize.text.function = function(x) {x})

}




#-------------------------------------------------------------------------------
# ------------------------ ◊ REAL LANGUAGES ◊ ----------------------------------
print('begin real')
real_path <- "latex_outputs/real/"


if (args[[1]] != "artif")  {
  
## collections summaries -------------------------------------------------------------
list <- lapply(COLLS, function(coll) {
    sum_tab <- read.csv("results/real/collections_summary.csv") %>% 
      filter(collection==coll) %>% mutate(collection=NULL)
    xt <- xtable(sum_tab,type = "latex")
    print(xt,file = paste0(real_path,coll,"_summary_table.tex"),
          include.rownames=F, include.colnames=F, only.contents=T,hline.after = NULL)
})


## language information
info <- dplyr::select(langs_info,-ISO_language) %>% arrange(language)
xt <- xtable(info,type = "latex")
print(xt,file = paste0(real_path,"language_info.tex"),
      include.rownames=F, include.colnames=F, only.contents=T,hline.after = NULL)


print("begin mixed n")
## MIXED N -------------------------------------------------------------

print("begin model selection")
## model selection -------------------------------------------------------------
list <- lapply(COLLS, function(collection) {
    real_df1     <- read.csv('results/real/mixed_n/ms_results.csv',check.names = F) %>% filter(coll==collection)
    real_ICdiff1 <- DfICdiff(real_df1,'real')
    real_params1 <- Dfparams(real_df1) %>% dplyr::select(-`0.dmax`,-coll,-best)
    
    ## AIC
    latexaic <- real_df1[1:length(artif_models)] %>% `colnames<-`(c(paste("Model",artif_models))) %>% mutate(language=LANGS) 
    latexaic <- latexaic[,c(ncol(latexaic),1:length(artif_models))]   # reorder
    print(xtable(latexaic, label=paste0("tab:",collection,"_AIC_scores"),type = "latex",digits=0), 
          file = paste0(real_path,collection,"_AIC_scores.tex"), only.contents=T, 
          include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL)
    
    ## AIC diff
    latexaic_diff <- mutate(real_ICdiff1,best=NULL) %>% `colnames<-`(c(paste("Model",artif_models),"language"))
    latexaic_diff <- latexaic_diff[,c(ncol(latexaic_diff),1:length(artif_models))]    # reorder
    print(xtable(latexaic_diff, label=paste0("tab:",collection,"_AIC_diff"),type = "latex"), 
          file = paste0(real_path,collection,"_AIC_diff.tex"), only.contents=T, 
          include.colnames=FALSE,include.rownames=FALSE,hline.after = NULL)
    
    ## parameters
    tab <- read.csv(paste0("data/real/collections/",collection,".csv")) %>% 
      group_by(ISO_language) %>% summarise(max_n = max(sent_n),max_d = max(d))
    latex_params <- merge(tab,real_params1,by='ISO_language') %>% mutate(ISO_language=langs_map[ISO_language])
    latex_params[c(grep("dstar",colnames(latex_params)),grep("dmax",colnames(latex_params)))] <- unlist(latex_params[c(grep("dstar",colnames(latex_params)),grep("dmax",colnames(latex_params)))]) %>%
      as.integer()
    xt1 <- xtable(latex_params,type = "latex", digits=3)
    colnames(xt1) <- c( "language", "$max(d)$", "$max(n)$", "$q$", "$q$", "$d_{max}$" , "$q_1$",
                        "$q_2$", "$d^*$", "$q_1$", "$q_2$","$d^*$", "$d_{max}$",
                        "$d_{max}$", "$\\gamma$","$\\gamma$","$q$","$d^*$","$\\gamma$","$q$","$d^*$","$d_{max}$")
    print(xt1,file = paste0(real_path,collection,"_params.tex"),
          include.rownames=FALSE, only.contents = TRUE,sanitize.text.function = function(x) {x})
    
})


  # - dstar summary
list <- lapply(COLLS, function(collection) {
    best_dstar <- best_dstar_mixed_n(collection)
    best_dstar_sum <- dstar_summary(best_dstar)
    
    best_dstar_sum$type   <- c('3-4','6-7','3-4-6-7')
    best_dstar_sum$sample <- c(unique(best_dstar$sample[best_dstar$type == '3-4']),
                   unique(best_dstar$sample[best_dstar$type == '6-7']),20)
    
    best_dstar_sum <- best_dstar_sum[,c(ncol(best_dstar_sum),1:ncol(best_dstar_sum)-1)]
    best_dstar_sum <- best_dstar_sum[,c(ncol(best_dstar_sum),1:ncol(best_dstar_sum)-1)]
    xt <- xtable(best_dstar_sum, caption = " Summary statistics of d^∗ parameter.",
                 label=paste("tab:",collection,"_dstar_summary",sep=""),type = "latex")
    print(xt,file = paste(real_path,collection,"_dstar_summary.tex",sep=""),caption.placement = "top",
          include.colnames=F, include.rownames=F,  only.contents = TRUE,hline.after = NULL)
})

  # - slopes summary
sum_df <- slopes_summary_mixed_n()
print(xtable(sum_df[1:2,], caption = " Summary statistics of slopes and slopes ratio.",
             label="tab:q1q2_summary",type = "latex"), 
      file = paste("latex_outputs/real/q1q2_summary_q1",suffix,".tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,include.colnames=FALSE,only.contents=T)
print(xtable(sum_df[3:4,], caption = " Summary statistics of slopes and slopes ratio.",
             label="tab:q1q2_summary",type = "latex"), 
      file = paste("latex_outputs/real/q1q2_summary_q2",suffix,".tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,include.colnames=FALSE,only.contents=T)
print(xtable(sum_df[5:6,], caption = " Summary statistics of slopes and slopes ratio.",
             label="tab:q1q2_summary",type = "latex"), 
      file = paste("latex_outputs/real/q1q2_summary_ratio",suffix,".tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,include.colnames=FALSE,only.contents=T)

 # - best PUD PSUD
bests <- lapply(COLLS, function(collection) {
  read.csv(paste("results/real/mixed_n/ms_results.csv")) %>% filter(coll==collection) %>% 
    dplyr::select(ISO_language,best) %>% rename(!!collection:=best) 
    })
df <- merge(bests[[2]],bests[[1]])
df$ISO_language <- langs_map[df$ISO_language]
df <- arrange(df,ISO_language)

print(xtable(df[1:10,], label="tab:best_PUD_PSUD_1",type = "latex"), 
      file = paste("latex_outputs/real/best_PUD_PSUD_1.tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,only.contents = TRUE,include.colnames=F,hline.after = NULL)
print(xtable(df[11:20,], label="tab:best_PUD_PSUD_2",type = "latex"), 
      file = paste("latex_outputs/real/best_PUD_PSUD_2.tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,only.contents = TRUE,include.colnames=F,hline.after = NULL)








print("begin fixed n")
## fixed n  -------------------------------------------------------------

# - dstar summary
list <- lapply(COLLS, function(collection) {
  best_dstar <- read.csv(paste0("results/real/fixed_n/ms_results.csv")) %>% 
    filter(coll==collection) %>% na.omit()
  best_dstar_sum <- dstar_summary(best_dstar)
  
  samples <- best_dstar %>% group_by(type) %>% filter(type %in% c('3-4','6-7')) %>% summarise(sample=n())
  best_dstar_sum$type   <- c('3-4','6-7','3-4-6-7')
  best_dstar_sum$sample <- c(samples$sample,sum(samples$sample))

  best_dstar_sum <- best_dstar_sum[,c(ncol(best_dstar_sum),1:ncol(best_dstar_sum)-1)]
  best_dstar_sum <- best_dstar_sum[,c(ncol(best_dstar_sum),1:ncol(best_dstar_sum)-1)]
  xt <- xtable(best_dstar_sum,type = "latex")
  print(xt,file = paste0(real_path,collection,"_dstar_summary_fixed_n.tex"),
        include.colnames=F, include.rownames=F,  only.contents = TRUE,hline.after = NULL)
})


# - slopes percentage
bests <- lapply(COLLS, function(collection) {
  read.csv("results/real/fixed_n/ms_results.csv") %>% filter(coll==collection) %>% 
    mutate(ratio = ifelse(q1>q2,1,0))  %>% filter(best %in% c(3,4,6,7)) %>% 
    group_by(ISO_language,coll) %>% summarise(perc=sum(ratio)/n()) %>% 
    dplyr::select(perc) %>% rename(!!collection:=perc)
})
df <- merge(bests[[2]],bests[[1]])
df$language <- langs_map[df$ISO_language]

print(xtable(df[1:10,c(4,2,3)], label="tab:fixed_n_slopes_share_1",type = "latex"), 
      file = paste("latex_outputs/real/fixed_n_slopes_share_1.tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,only.contents = TRUE)
print(xtable(df[11:20,c(4,2,3)], label="tab:fixed_n_slopes_share_2",type = "latex"), 
      file = paste("latex_outputs/real/fixed_n_slopes_share_2.tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,only.contents = TRUE)

## - slopes perc summary
df_sum <- apply(df[,c(2,3)],2,summary) %>% t()
print(xtable(df_sum, label="tab:fixed_n_slopes_share_summary",type = "latex"), 
      file = paste("latex_outputs/real/fixed_n_slopes_share_summary.tex",sep=""),caption.placement = "top",
      include.rownames=T,only.contents = TRUE,include.colnames=FALSE)


## - modal best PUD PSUD
bests <- lapply(COLLS, function(collection) {
  read.csv(paste("results/real/fixed_n/ms_results.csv")) %>% filter(coll==collection) %>% 
    group_by(ISO_language) %>% summarise(modal_best=getmode(type)) %>% 
    dplyr::select(ISO_language,modal_best) %>% rename(!!collection:=modal_best)
})
df <- merge(bests[[2]],bests[[1]]) 
df$ISO_language <- langs_map[df$ISO_language]
df <- arrange(df,ISO_language)

print(xtable(df[1:10,], label="tab:best_PUD_PSUD_fixed_1",type = "latex"), 
      file = paste("latex_outputs/real/best_PUD_PSUD_fixed_1.tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,only.contents = TRUE,include.colnames=F,hline.after = NULL)
print(xtable(df[11:20,], label="tab:best_PUD_PSUD_fixed_2",type = "latex"), 
      file = paste("latex_outputs/real/best_PUD_PSUD_fixed_2.tex",sep=""),caption.placement = "top",
      include.rownames=FALSE,only.contents = TRUE,include.colnames=F,hline.after = NULL)



## omega   -------------------------------------------------------------
df_omega <- omega_null_table()
print(xtable(df_omega[,c(2,1,3,4,5)], label=paste0("tab:omega_0"),type = "latex"), 
      file = paste0("latex_outputs/real/omega_0.tex"),
      include.rownames=FALSE,include.colnames=FALSE,only.contents=T,hline.after = NULL)
}








