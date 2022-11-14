# ANALYSE RESULTS

## contents
# - summary tab of collections
# - slopes (results_analyis)
# - dstar fixed n stats (results_analyis) + recollect all df creation
# - dstar n indep stats (model_sel_real)

## arguments
# - type: "artif","real","real_chunks"


source("aux_functions.R")


## ----  summary of collections --------------------------------------------------

list <- lapply(COLLS, function(collection) {
    tabcoll <- read.csv(paste0("data/real/collections/",collection,".csv"))
    collection_summary(tabcoll) %>% mutate(collection=collection)
  })
df_summaries <- do.call(rbind.data.frame,list)
write.csv(df_summaries,paste0("results/real/collections_summary.csv"),row.names = F)




## ---- MIXED N ----------------------------

## q1 q2 slopes and ratio
slopes_dfs <- lapply(COLLS, function(collection) {
    tabCOLL     <- read.csv(paste0("data/real/collections/",collection,".csv"))
    results_df  <- read.csv(paste0("results/real/mixed_n/ms_results.csv"),check.names = F) %>% filter(coll==collection)
    
    slopes_df <- apply(results_df,1,function(row) {
      best <- row[colnames(results_df)=='best'] %>% as.numeric()
      lang <- row[colnames(results_df)=='ISO_language']
      if (best %in% c(3,4)) {          
      # take original slopes
        q1    <- row[grep(paste0(best,".q1"),colnames(results_df))] %>% as.numeric()
        q2    <- row[grep(paste0(best,".q2"),colnames(results_df))] %>% as.numeric()
      } else if (best %in% c(6,7)) {   
      # approximate slope q1 with models 3 or 4 and original d*
        q2    <- row[grep(paste0(best,".q"),colnames(results_df))] %>% as.numeric()
        dstar <- row[grep(paste0(best,".dstar"),colnames(results_df))] %>% as.numeric()
        dmax <- row[grep(paste0(best,".dmax"),colnames(results_df))] %>% as.numeric()
        tab <- filter(tabCOLL,ISO_language==lang); d_vals <- sort(unique(tab$d)); freq_d <- table(tab$d)
        M <- sum(freq_d*d_vals); N <- length(tab$d)
        q1 <- if (best==6) MS_model3(d_vals,freq_d,N,M,dstar)$pars[1] else MS_model4(d_vals,freq_d,N,M,dstar,dmax)$pars[1]
      }
      cbind(lang,q1,q2,q1/q2,best)
    }) %>% t() %>% data.frame() %>% `colnames<-` (c("ISO_language","q1","q2","ratio","best"))
    slopes_df$coll <- collection
    slopes_df[,2:4] <- as.numeric(unlist(slopes_df[,2:4]))
    slopes_df
})
slopes_df <- do.call(rbind,slopes_dfs)
write.csv(slopes_df,paste0("results/real/mixed_n/slopes.csv"),row.names = F)


## slopes summary fun
slopes_summary_mixed_n <- function() {
  slopes_df <- read.csv(paste0("results/real/mixed_n/slopes.csv"))
  summaries <- lapply(c("PUD","PSUD"), function(collection) {
    rows_to_summarise  <- slopes_df[slopes_df$coll==collection,2:4]
    stats <- apply(rows_to_summarise,2,summary)
    sd    <- apply(rows_to_summarise,2,sd)
    summary  <- rbind(stats,sd) %>% t() %>% data.frame(check.names = F) %>% 
      mutate(var_name=rownames(.),coll=collection)
  })
  sum_df <- do.call(rbind,summaries)
  sum_df[order(match(sum_df$var_name, c("q1","q2","ratio"))),c(9,1,2,3,4,5,6,7,8)] %>% mutate(var_name=NULL)
}




# ---- fixed sentence length ------

# model selection results

params_fixed_n <- lapply(COLLS, function(collection) {
    tabb <- read.csv(paste0("data/real/collections/",collection,".csv"))
    result_ls <- lapply(ISO, function(lang) {
      tab <- filter(tabb,ISO_language==lang)
      unique_lengths <- unique(tab$sent_n)
      params_onesentn <- lapply(unique_lengths, function(sent_length) {
        tab <- filter(tab,sent_n==sent_length)
        unique_d_num <- length(unique(tab$d))
        if (unique_d_num >= 3) {
          print(paste(lang,sent_length))
          dfs          <- GetMleEst(tab,"AIC")$df  
          real_params  <- Dfparams(dfs)
          best         <- colnames(dfs)[which.min(dfs[,1:length(artif_models)])]
          dstar     <- if (best %in% two_reg_models) real_params[grep(paste0(best,":dstar"),colnames(real_params))] else real_params$`3:dstar`
          q1        <- if (best %in% c('3','4')) real_params[grep(paste0(best,":q1"),colnames(real_params))] else real_params$`3:q1`
          q2        <- if (best %in% c('3','4')) real_params[grep(paste0(best,":q2"),colnames(real_params))] else real_params$`3:q2`
          list('sent_n'=sent_length,'best'=best,'dstar'=as.numeric(dstar),'q1'=as.numeric(round(q1,4)),'q2'=as.numeric(round(q2,4)))
        } else "NULL"
      })
      do.call(rbind.data.frame,params_onesentn) %>% mutate(ISO_language=lang)
    })
    df_params <- do.call(rbind.data.frame,result_ls)
    
    # ADD type
    df_params <- df_params %>% mutate(type = get_type(best))
    
    # ADD distinct sentences number
    tabb <- tabb %>% group_by(ISO_language,sent_n) %>% summarise(sent_numb=length(unique(sentence_ID)))
    
    # merge all
    merge(df_params,tabb, by = c("ISO_language","sent_n")) %>% mutate(coll=collection)
}) 

fixed_n_df <- do.call(rbind.data.frame,params_fixed_n)

print('start omega')

# ---------------- OMEGA -----------------

# average omega in each sentence length
omega_dfs <- lapply(COLLS, function(collection) {
  read.csv(paste0("results/real/omega/",tolower(collection),"26_list_full.csv"),sep='\t') %>% 
    rename(ISO_language=treebank, sent_n = n) %>% mutate(omega=Omega(D,Dmin,Drla),coll=collection) %>% 
    group_by(coll,ISO_language,sent_n) %>% summarise(omega=mean(omega)) 
})
df_omega <- do.call(rbind.data.frame,omega_dfs)
joined_fixed_n <- merge(fixed_n_df,df_omega,by=c('sent_n','ISO_language','coll'),all.y = T)
write.csv(joined_fixed_n,"results/real/fixed_n/ms_results.csv",row.names = F)


# dd distribution for omega < 0 fun 
omega_null_table <- function() {
  omegabest_dfs <- lapply(COLLS, function(collection) {
      bestmodel_df <- read.csv(paste0("results/real/fixed_n/ms_results.csv")) %>% 
        filter(coll==collection) %>% na.omit()
      neg_omega <- filter(bestmodel_df,omega <= 0.1 & omega>=-0.1) %>% arrange(omega)
      neg_omega$language = langs_map[neg_omega$ISO_language]
      write.csv(neg_omega,paste("results/real/omega/",collection,"_omega_vs_best.csv",sep=""))
      neg_omega %>%  mutate(collection=collection)
  })
  do.call(rbind.data.frame,omegabest_dfs) %>% 
    dplyr::select(language,coll,sent_n,omega,best)
}
print('finish omega')









## ------------ CHUNKS -------------
#
## chunk sizes
#if (args[[1]] %!in% c("artif","words")) {
#lapply(COLLS, function(collection) {
#  coll_rows <- lapply(ISO, function(ISO_language) {
#              forrest <- readForrest(collection,ISO_language)
#              rows <- lapply(1:length(forrest), function(i) {
#                heads <- forrest[[i]]
#                sentence_ID <- i
#                n <- length(heads)
#                positions <- 1:n
#                # get partition in segments
#                chunk_names <- assign_chunks(heads,type=args[[1]])
#                new_n <- length(unique(chunk_names))
#                chunk_sizes <- table(chunk_names) %>% as.numeric()
#                data.frame("ISO_language"=ISO_language,"sentence_ID"=sentence_ID,
#                          "sent_l"=new_n,"sent_n"=n,"chunk_size"=chunk_sizes)
#              })
#              do.call(rbind,rows)
#          })
#  tab <- do.call(rbind,coll_rows)
#  write.csv(tab,paste("data/real/collections/",collection,"_chunks_sizes",suffix,".csv",sep=""))
#  
## and add to summary
#  sum_chunksize <- tab %>% group_by(ISO_language) %>% 
#    summarise(min_size=min(chunk_size),mean_size=mean(chunk_size),max_size=max(chunk_size))
#  sum_chunksize <- sum_chunksize[match(ISO, sum_chunksize$ISO_language),];sum_chunksize$ISO_language <- LANGS
#  summary <- read.csv(paste("data/real/",collection,"_summary",suffix,".csv",sep=""))[-1]
#  sum <- merge(summary,sum_chunksize, by="ISO_language") 
#  write.csv(sum,paste("data/real/",collection,"_summary",suffix,".csv",sep=""))
#})
#
#  # summary of summary TO FINISH
#lapply(COLLS, function(collection) {
#  sum <- read.csv(paste("data/real/",collection,"_summary",suffix,".csv",sep=""))[-1]
#  columns <- grep("mean",colnames(sum)) %>% c(grep("max",colnames(sum)))
#  sum <- apply(sum[,columns],2,function(col) summary(col) %>% round(2)) %>% t()
#  sum <- sum[c(1,4,2,5,3,6),]
#  write.csv(sum,paste("data/real/",collection,"_summary_of_summary",suffix,".csv",sep=""))
#})
#
#}
#
#
#
#
#
#
##### fixed n model selection for sent_n_words   TO FINISH
#
#lapply(COLLS, function(collection) {
#    tabb <- read.csv(paste("data/real/collections/",collection,suffix,".csv",sep=""))
#    all_langs <- lapply(ISO, function(lang) {
#      tab <- filter(tabb,ISO_language==lang)
#      unique_lengths <- unique(tab$sent_n)
#      dstar_one_lang <- lapply(unique_lengths, function(sent_length) {
#        tab_n <- filter(tab,sent_n==sent_length)
#        unique_d_num <- length(unique(tab_n$d))
#        # do I have more than 3 d values in the sample?
#        if (unique_d_num >= 3) {
#          print(paste(lang,sent_length))
#          dfs          <- GetMleEst(tab_n,"AIC")$df  
#          best         <- colnames(dfs)[which.min(dfs[,1:length(artif_models)])]
#          real_params  <- Dfparams(dfs)
#          dstar        <- if (best %in% two_reg_models) real_params[grep(paste(best,":dstar",sep=""),colnames(real_params))] else real_params$`3:dstar`
#          data.frame("dstar"=unname(dstar),"best"=best,"sent_n_words"=unique(tab_n$word_num),
#                     "sent_n"=sent_length,"ISO_language"=lang)
#        }
#      })
#      do.call(rbind,dstar_one_lang)
#    })
#  df <- do.call(rbind,all_langs)
#  write.csv(df,paste("data/real/",collection,"_fixed_n_words_df",suffix,".csv",sep=""))
#})
#
#
#
#
## DSTAR CONSTRAINT - TO DO: adapt to run in script
## dstar (in words) for each chunk size
#tabs <- lapply(COLLS, function(COLL) {
#  tab_s <- read.csv(paste("data/real/collections/",COLL,"_chunks_sizes",suffix,".csv",sep=""))[-1]
#  tab_dstar <- read.csv(paste("data/real/",COLL,"_fixed_n_df.csv",sep=""))[-1] %>%
#    dplyr::select(language,sent_n,dstar) %>% rename(ISO_language=language)
#  df <- merge(tab_s,tab_dstar, by=c("ISO_language","sent_n")) %>% na.omit() %>% mutate(coll=COLL)
#  write.csv(df, paste("data/real/",COLL,"_dstar_chunksize",suffix,".csv",sep=""))
#  df
#})
#
#







