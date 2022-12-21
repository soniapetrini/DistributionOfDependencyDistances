suppressMessages(library("dplyr"))
options(dplyr.summarise.inform = FALSE)
suppressMessages(library("ISOcodes"))
suppressMessages(library("ggplot2"))
suppressMessages(require("ggpubr"))
suppressMessages(require("qpcR"))
suppressMessages(library("stats4"))
suppressMessages(library("bbmle"))
suppressMessages(library("reshape2"))
suppressMessages(library("data.table"))
suppressMessages(library("RColorBrewer"))
suppressMessages(library("VGAM"))
suppressMessages(library("ggcorrplot"))
suppressMessages(library("parallel"))
library("ggrepel")
library("xtable")




# GLOBAL --------------------------

## languages
COLLS <- c("PSUD","PUD")
LANGS <- c("Arabic","Chinese","Czech","English","Finnish","French","German","Hindi","Icelandic",
           "Indonesian","Italian","Japanese","Korean","Polish","Portuguese","Russian","Spanish","Swedish","Thai","Turkish")

## iso codes
ISO <- sapply(LANGS,function(l) ISO_639_3 %>% filter(Name==l) %>% dplyr::select(1)) %>% unlist() %>% unname() 
labs <- LANGS; names(labs) <- ISO
langs_map = setNames(LANGS,ISO)

## family and script
langs_info <- read.csv("data/real/collections/attributes.csv")[-1]

## models
artif_models   <- c("0","1","2","3","4","5","6","7")
two_reg_models <- c("3","4","6","7")
type <- c("0","1","2","3-4","5","6-7")
K <- c(1, 1, 2, 3, 4, 2, 3, 4)
models_names <- c("0: Random","1: Geometric","2: Truncated Geometric","3: 2geom",
                  "4: Truncated 2geom","5: Truncated Zeta","6: zeta-geom",
                  "7: Truncated zeta-geom")
params_names <- c("0:dmax","1:q","2:q","2:dmax","3:q1","3:q2","3:dstar","4:q1","4:q2","4:dstar","4:dmax",
                  "5:dmax","5:gamma","6:gamma","6:q","6:dstar","7:gamma","7:q","7:dstar","7:dmax")
colnames <- append(artif_models,params_names)

## visualization
colors = c("#27AE14","#C9CC1B","#ECEF26","#C54126","#F26144","#F244DF","#2144B3","#649FF1")
names(colors) <- levels(factor(artif_models))
colors_type <-  c("#27AE14","#C9CC1B","#ECEF26","red","#F244DF","blue")
names(colors_type) <- levels(factor(type))




## reading functions --------------------------------------

# ---- artificial data ---- #
ReadArtif <- function(model,N_sam=NULL) {
  N_sam <- if (is.null(N_sam)) 10000 else N_sam
  read.csv(paste0("data/artificial/",model,"_",N_sam,".csv"))[-1]
}

# ---- real data  ---- #
readForrest <- function(collection,ISO_language) {
    forrest_ch <- read.csv(paste0("collections/",collection,"26/",LANGS[ISO==ISO_language],"-all.heads"), header=FALSE, sep=";")
    apply(forrest_ch,1,split_and_make_int)
}

# read one language
ReadOneLang <- function(collection,ISO_language,sent_length=NULL){
  forrest <- readForrest(collection,ISO_language)
  rows <- lapply(1:length(forrest), function(i) {
    tree <- forrest[[i]]
    sentence_ID <- i
    n <- length(tree)
    d = abs(tree-seq(1,n))
    d <- d[tree!=0]
    data.frame("ISO_language"=ISO_language,"sentence_ID"=sentence_ID,"sent_n"=n,"d"= d)
  })
  lang_table <- do.call(rbind,rows)
  if (!is.null(sent_length)) lang_table <- filter(lang_table, sent_n %in% sent_length) 
  return(lang_table)
}

# read a whole collection
ReadColl <- function(collection) {
  trees <- lapply(ISO,ReadOneLang,collection=collection)
  forrest <- do.call(rbind,trees)
  return(forrest)
}



## computations --------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

split_and_make_int <- function(x) {
  y <- x %>% strsplit(split=" ")
  y <- as.integer(y[[1]])
  return(y)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cumforward <- function(prob) {
  sort(cumsum(rev(prob)),decreasing=T)
}

TableCumul <- function(d_seq,cumul=F) {
  freq <- table(d_seq)/length(d_seq)
  y_pred <- if (cumul==F) as.numeric(freq) else cumforward(freq)
  x_vals <- names(freq) %>% as.numeric()
  df <- data.frame(x_vals,y_pred)
  return(df)
}

Omega <- function(D,Dmin,Drla) (Drla-D)/(Drla-Dmin)

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

add_missing_lengths <- function(df) {
for (lang in ISO)  {
  obs_lengths <- filter(df,ISO_language==lang) %>% dplyr::select(sent_n) %>% unlist()
  all_length <- seq(3,max(obs_lengths))
  for (sentence_n in setdiff(all_length,obs_lengths)) {
    df <- df %>% add_row(ISO_language=lang,sent_n=sentence_n)
  }}
  df
}

collection_summary <- function(df) {
    # d statistics 
    sum_d <- group_by(df, ISO_language) %>%
               summarise(sent_num = length(unique(sentence_ID)),
                        d_num = length(d),min_d = min(d), mean_d = mean(d), max_d = max(d))
    # n statistics
    sum_n <- group_by(df, ISO_language,sentence_ID) %>% slice_head() %>%
               group_by(ISO_language) %>% 
               summarise(min_n = min(sent_n), mean_n = mean(sent_n), max_n = max(sent_n))
    # merge
    merge(langs_info, sum_d, by = 'ISO_language') %>% merge(sum_n, by = 'ISO_language') %>% 
      mutate(ISO_language=NULL)
}


# d* of best model
best_dstar_mixed_n <- function(collection,type = "words") {
  real_df <- read.csv("results/real/mixed_n/ms_results.csv",check.names = F) %>% filter(coll == collection)

  df3 <- dplyr::select(real_df,`3.dstar`,ISO_language,best,coll)[real_df$best =="3",] %>% rename(dstar=`3.dstar`)
  df4 <- dplyr::select(real_df,`4.dstar`,ISO_language,best,coll)[real_df$best =="4",] %>% rename(dstar=`4.dstar`)
  df6 <- dplyr::select(real_df,`6.dstar`,ISO_language,best,coll)[real_df$best =="6",] %>% rename(dstar=`6.dstar`)
  df7 <- dplyr::select(real_df,`7.dstar`,ISO_language,best,coll)[real_df$best =="7",] %>% rename(dstar=`7.dstar`)
  
  best_dstar <- do.call(rbind.data.frame,list(df3,df4,df6,df7))
  best_dstar <- best_dstar %>% mutate(type = get_type(best)) %>% 
    group_by(type) %>% mutate(sample = n_distinct(ISO_language))
  return(best_dstar)
}


# d* summary by best model
dstar_summary <- function(best_dstar) {
  # best_dstar is a df with columns 'dstar' and 'best'
  c(summary(best_dstar$dstar[best_dstar$type == '3-4']),sd(best_dstar$dstar[best_dstar$type == '3-4'])) %>% data.frame() %>% t() %>%
    rbind(c(summary(best_dstar$dstar[best_dstar$type == '6-7']),sd(best_dstar$dstar[best_dstar$type == '6-7']))) %>%
    rbind(c(summary(best_dstar$dstar[best_dstar$type %in% c('3-4','6-7')]),sd(best_dstar$dstar[best_dstar$type %in% c('3-4','6-7')]))) %>%
    `colnames<-`(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "standard deviation")) %>%
    `rownames<-`(c("3-4","6-7","3-4-6-7")) %>% data.frame(check.names = F)
}

get_type <- function(best) {
  case_when(best == 0 ~ "0",best == 1 ~ "1",best == 2 ~ "2",
            best %in% c(3,4) ~ "3-4", best == 5 ~ "5", best %in% c(6,7) ~ "6-7")
}


# probabilities
GetCumProbs <- function(table) {
  table_prob <- table %>% group_by(ISO_language,sent_n,d) %>% summarise(n = n()) %>% 
    group_by(ISO_language,sent_n) %>% 
    mutate(prob = n / sum(n), 
           control_line = model0(d,sent_n)$y_pred,
           cumprob = cumforward(prob), 
           cum_control = cumforward(control_line),
           sent_n = as.factor(sent_n))
  return(table_prob)
}

GetCumProbsIndep <- function(table) {
  table_prob <- table %>% group_by(ISO_language,d) %>% summarise(n = n()) %>% 
    group_by(ISO_language) %>%
    mutate(prob = n / sum(n),
           cumprob = cumforward(prob))
  # control line
  table_control <- table %>%
    group_by(ISO_language) %>% mutate(unique_n = length(unique(sentence_ID)))  %>%
    group_by(ISO_language,sent_n) %>% summarise(n_num = length(unique(sentence_ID)),d=d,unique_n=unique_n) %>% 
    group_by(ISO_language,sent_n,d) %>% summarise(d_num = n(),d=d,unique_n=unique_n,n_num) %>% 
    distinct(d_num,.keep_all=T) %>%
    summarise(n_freq=n_num/unique_n,d_freq=d_num/n_num,n_num=n_num,d=d) %>% 
    mutate(control=control_line(sent_n,d)) %>% 
    group_by(ISO_language,d) %>% summarize(mean_control=sum(control*n_freq)) %>%
    mutate(cum_mean_control=cumforward(mean_control))
  return(cbind(table_prob,"mean_control"=table_control$mean_control,"cum_mean_control"=table_control$cum_mean_control))
}




## plots ------------

control_line <- function(sent_n,d) {
  y <- 2*(sent_n-d)/(sent_n*(sent_n-1))
  return(y)
}

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

scale_y_log_formatted <- function() {
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^as.integer(x)),
                labels=scales::trans_format('log10',scales::math_format(10^.x)))
}

AppendStat <- function(l,table,stat="mean") {
  iso <- ISO[LANGS==l]
  if (stat=="mean") { lab <- paste(l,": ",as.numeric(table[table$ISO_language==iso,2]),sep="")
  } else { lab <- paste(l,": ", as.numeric(table[table$ISO_language==iso,3]),sep="") }
  return(lab)
}



# - p for chosen sentence lengths
plot_prob <- function(table, collection, ISO_languages, sentence_lengths, cumulative="no",scale_x="linear") {
  table <- table %>% filter(sent_n %in% sentence_lengths & ISO_language %in% ISO_languages) %>% GetCumProbs() 
  
  y <- switch(cumulative, "yes" = table$cumprob, "no" = table$prob)
  y_lab <- switch(cumulative, "yes" = "P(d)", "no" = "p(d)")
  subtitle <- switch(cumulative, "yes" = "Forward cumulative probability", "no" = "Probability")
  y_control <- switch(cumulative, "yes" = table$cum_control, "no" = table$control_line)
  
  table %>% mutate(ISO_language=factor(ISO_language,levels=ISO)) %>% 
    ggplot() + geom_line(aes(d, y, group=sent_n, color=sent_n)) + 
    facet_wrap(~ISO_language, labeller = labeller(ISO_language=labs)) +
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^as.integer(x)),
                  labels=scales::trans_format('log10',scales::math_format(10^.x))) +
    labs(title=paste(collection, "sentences of length", sentence_lengths),
         x="d",y=y_lab) + 
    geom_line(aes(x=d, y=y_control, group=sent_n, color=sent_n),linetype="dashed") +
    guides(color=guide_legend(title="sentence length")) + theme(legend.position="bottom") +
    { if (scale_x == "log") scale_x_log10()}
}


# - cumulative p independent of sentence length
plot_prob_tot <- function(table, collection, ISO_langs,cumul="no",scale_x="linear") {

  table <- table %>% filter(ISO_language %in% ISO_langs) %>% 
    GetCumProbsIndep()
  labs <- LANGS; names(labs) <- ISO
  
  y <- switch(cumul,"yes"=table$cumprob,"no"=table$prob)
  y_lab <- switch(cumul,"yes"="P(d)","no"="p(d)")
  subtitle <- switch(cumul,"yes"="Forward comulative probability","no"="Probability")
  
  table %>% mutate(ISO_language=factor(ISO_language,levels=ISO)) %>%
  ggplot() + geom_line(aes(d,y)) + geom_point(aes(d,y,color=ISO_language)) + 
    facet_wrap(~ISO_language,labeller = labeller(ISO_language=labs[sort(names(labs))])) +
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^as.integer(x)),
                    labels=scales::trans_format('log10',scales::math_format(10^.x)))+ 
    geom_line(aes(x=d, y=cum_mean_control,color=ISO_language),linetype="dashed") +
    theme(legend.position = "none") + labs(title=paste(collection,", sentence length independent",sep=""),
                                           x = "d", y = y_lab) +
    { if (scale_x == "log") scale_x_log10()}
}


# - p for mean and modal sentence length 
PlotStatN <- function(table,collection,stat="mean",cumul="no") {
  stats_tab <- table %>% group_by(ISO_language,sentence_ID) %>% slice_head() %>%
    group_by(ISO_language) %>% summarise(mean = round(mean(sent_n)), mode = getmode(sent_n))

  stats_tab <- stats_tab[match(ISO, stats_tab$ISO_language),]
  
  numerosities <- lapply(ISO, function(iso) nrow(filter(table,ISO_language==iso))) %>% unlist()
  table <- table %>% mutate(mean = rep(stats_tab$mean,numerosities),mode = rep(stats_tab$mode,numerosities))
  
  if (stat=="modal") {
    labs_stat <- lapply(LANGS,AppendStat,stats_tab,stat="modal") %>% unlist()
    table <- table %>% group_by(ISO_language) %>% filter(sent_n==mode) %>% GetCumProbs()
  } else { 
    labs_stat <- lapply(LANGS,AppendStat,stats_tab,stat="mean") %>% unlist()
    table <- table %>% group_by(ISO_language) %>% filter(sent_n==mean) %>% GetCumProbs() }
  
  names(labs_stat) <- ISO
  y <- switch(cumul,"yes"=table$cumprob,"no"=table$prob)
  y_control <- switch(cumul,"yes"=table$cum_control,"no"=table$control_line)
  y_lab <- switch(cumul,"yes"="P(d)","no"="p(d)")
  
  table %>% mutate(ISO_language=factor(ISO_language,levels=ISO)) %>% 
    ggplot(aes(d, y, color=sent_n)) + 
      geom_line(size=1) + facet_wrap(~ISO_language, labeller = labeller(ISO_language=labs_stat)) +
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^as.integer(x)),
                    labels=scales::trans_format('log10',scales::math_format(10^.x))) + 
      labs(x="d",y=y_lab) + xlim(c(0,max(table$d)+1)) + 
    geom_line(aes(x=d, y=y_control, group=sent_n, color=sent_n),linetype="dashed") +
    theme(legend.position="none", text = element_text(size=18))

} 


# - heatmaps of best model
heatmap_best_mixed_n <- function(collection) {
  best_dstar <- best_dstar_mixed_n(collection) 
  best_dstar %>% mutate(sent_n = factor("  all n"), best=factor(best),
                        language = langs_map[best_dstar$ISO_language]) %>% arrange(language) %>%
    ggplot(aes(y=language, x=sent_n, fill=best)) + geom_tile() + 
    scale_fill_manual(values=colors) + coord_flip() +
    geom_hline(yintercept = seq(1.5,20.5,1),color='white') +
    guides(fill=guide_legend(title = 'best \nmodel')) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=18),
          axis.title.y = element_blank()) 
}

heatmap_best_fixed_n <- function(collection) {
  df <- read.csv("results/real/fixed_n/ms_results.csv") %>% filter(coll==collection)
  df %>% na.omit() %>% mutate(best=factor(best),language=factor(ISO_language,levels=ISO)) %>% 
    ggplot(aes(y=language, x=sent_n, fill=best)) + geom_tile()  + 
    scale_x_continuous(breaks=seq(5,100,5)) + scale_y_discrete(labels=labs) +
    scale_fill_manual(values=colors,na.value = "#b9d0ed") + coord_flip() +
    theme(axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),axis.title.x = element_blank(),
          legend.position = "none",text=element_text(size=18)) +
    labs(x="sentence length") + 
    guides(fill=guide_legend(title = 'best \nmodel'))
}


# - best models
### real data
DfBestModel <- function(ISO_lang,collection) {
  # real curve
  tab <- read.csv(paste0("data/real/collections/",collection,'.csv')) %>%
    filter(ISO_language==ISO_lang)
  df_real <- TableCumul(tab$d, cumul = F)
  x_vals <- df_real$x_vals
  
  # predicted curve
  results_df <- read.csv("results/real/mixed_n/ms_results.csv",check.names = F) %>% 
    filter(ISO_language==ISO_lang, coll == collection)
  best_model <- as.character(results_df$best)
  best_parameters_df <- results_df[,-c(1:length(artif_models))]
  df_pred_list <- suppressWarnings(YPredList(best_parameters_df,x_vals,max(x_vals))) 
  df_pred <- switch(best_model, "0"=df_pred_list[[1]],"1"=df_pred_list[[2]], "2"=df_pred_list[[3]],
                    "3"=df_pred_list[[4]],"4"=df_pred_list[[5]], "5"=df_pred_list[[6]], 
                    "6"=df_pred_list[[7]],"7"=df_pred_list[[8]]) 
  
  # bind
  df_list <- list(df_real,df_pred)
  df <- do.call(rbind,df_list) %>% cbind("color"=rep(c("data","fitted model"),sapply(df_list,nrow)))
  
  # plot
  ggplot(df, aes(x_vals,y_pred, color=color)) + geom_line(aes(group = x_vals),color="green")+ 
    geom_line(data = subset(df, color == "data"),stat="identity", color = "blue",size=1) +
    geom_line(data = subset(df, color != "data"),aes(color="fitted model"),stat="identity",size=1) +
    geom_point(data = subset(df, color == "data"),aes(color="data"),size=3) +
    theme_minimal() +
    geom_vline(xintercept=4,color='red',linetype='dashed') +
    labs(x="d",y="p(d)", colour = "Legend \n",subtitle=LANGS[ISO==ISO_lang]) +
    theme(legend.position = c(0.2,0.2), text = element_text(size=15)) +
    scale_color_manual(labels = c("data", "fitted model"), values = c("blue", "magenta")) +
    { if (best_model == "5") scale_x_log10() } +
    {if (best_model != "0") scale_y_log_formatted() }
}  



### artificial data
DfModSelArtif <- function(artif_model,criterion="BIC") {
  # artificial curve
  d_art  <- ReadArtif(artif_model,10000)
  df_art <- TableCumul(d_art[,1],cumul=F)
  x_vals <- df_art$x_vals
  
  # predicted curve
  results_df <- read.csv("results/artificial/ms_results.csv",check.names = F) %>% filter(model==artif_model)
  best_model <- as.character(results_df$best)
  best_parameters_df <- results_df[,-c(1:length(artif_models))]
  df_pred_list       <- YPredList(best_parameters_df,x_vals,unique(d_art$sent_n)) 
  df_pred <- switch(best_model, 
                    "0"=df_pred_list[[1]],"1"=df_pred_list[[2]],"2"=df_pred_list[[3]],
                    "3"=df_pred_list[[4]],"4"=df_pred_list[[5]], "5"=df_pred_list[[6]],
                    "6"=df_pred_list[[7]], "7"=df_pred_list[[8]],) 
  
  # bind
  df_list <- list(df_art,df_pred) 
  df <- do.call(rbind,df_list) %>%  
    cbind("color"=rep(c("data","fitted model"),sapply(df_list,nrow)))
  
  ggplot(df, aes(x_vals,y_pred, color=color)) + geom_line(aes(group = x_vals),color="green")+ 
    geom_line(data = subset(df, color == "data"),stat="identity", color = "darkgreen") +
    geom_line(data = subset(df, color != "data"),aes(color="fitted model"),stat="identity",size=1) +
    geom_point(data = subset(df, color == "data"),aes(color="data")) +
    labs(title=paste("Model",artif_model,'sample'),x="d",y="p(d)")  +
    theme(legend.position = c(0.2,0.2),axis.text = element_text(size = 13)) +
    scale_color_manual(labels = c("data", "fitted model"), values = c("blue", "magenta")) +
    { if (artif_model == "5") scale_x_log10() } +
    {if (artif_model != "0") scale_y_log_formatted() }
}



PlotSampleVsReal <- function(model) {
  # artificial
  d_art <- ReadArtif(model)
  df_art <- TableCumul(d_art[,1],cumul=F)
  x_vals <- df_art$x_vals
  
  # true model
  n=20; q=0.2; q1=0.5; q2=0.1; dstar=4; gamma=1.6
  df_theo  <- switch(model,
                     "0"= model0(x_vals,n),
                     "1"= model1(x_vals,q,plot=F),   "2"= model2(x_vals,q,n-1),
                     "3"= model3(x_vals,q1,q2,dstar),"4"= model4(x_vals,q1,q2,dstar,n-1),
                     "5"= model5(x_vals,n-1,gamma),  "6"= model6(x_vals,gamma,q,dstar),"7"= model7(x_vals,gamma,q,dstar,n-1)) 
  
  # bind
  df_list <- list(df_art,df_theo)
  df <- do.call(rbind,df_list) %>% cbind("color"=rep(c("sample","empirical"),sapply(df_list,nrow)))
  
  # plot
  ggplot(df, aes(x_vals,y_pred, color=color)) +
    {if (model != "0") scale_y_log_formatted() } +
    {if (model %in% c("5")) scale_x_log10() } +
    geom_line(data = subset(df, color != "sample"),aes(color="empirical")) +
    geom_point(data = subset(df, color == "sample"),aes(color="sample")) +
    labs(title=paste0('Model ',model,' sample'), x="d", y="p(d)", colour = "Legend \n") +
    theme(legend.position = c(0.2,0.2),axis.text = element_text(size = 13)) +
    scale_color_manual(labels = c("empirical", "sample"), values = c("red", "#33CC99"))
  
} 




# model selection --------------------------------------

# - Information Criteria
GetAIC <- function(l,est_df,N,K) {
  K <- K[l]
  m2loglik <- est_df[,l]
  m2loglik + 2*K
}

GetBIC <- function(l,est_df,N,K) {
  K <- K[l]
  m2loglik <- est_df[,l]
  m2loglik + K*log(N)
}

# - sums
H <- function(dmax,gamma)  sum(seq(1, dmax)^(-gamma))
W <- function(d_vals,freq_d,sent_n)  sum(freq_d*log(sent_n-d_vals))
M_prime <- function(d_vals,freq_d) sum(freq_d*log(d_vals))
M_star <- function(d_vals,freq_d,dstar) {
  newd <- d_vals[d_vals <= dstar]
  freq_star <- freq_d[d_vals <= dstar]
  sum(freq_star*newd)
}
M_prime_star <- function(d_vals,freq_d,dstar) {
  newd <- d_vals[d_vals <= dstar]
  freq_star <- freq_d[d_vals <= dstar]
  M_prime(newd,freq_star)
}
N_star <- function(d_vals,freq_d,dstar) freq_d[d_vals <= dstar] %>% sum()



# - models------------
c1_model3 <- function(q1,q2,dstar) q1*q2/(q2 + (1-q1)^(dstar-1)*(q1-q2))
c1_model4 <- function(q1,q2,dstar,dmax) q1*q2/(q2 + (1-q1)^(dstar-1)*(q1-q2-q1*(1-q2)^(dmax-dstar+1)))
c2_fun    <- function(c1,q1,q2,dstar) c1*((1-q1)^(dstar-1))/(1-q2)^(dstar-1)
r1_model6 <- function(gamma,q,dstar) q/(q*H(dstar,gamma)+dstar^(-gamma)*(1-q))
r1_model7 <- function(gamma,q,dstar,dmax) q/(q*H(dstar,gamma)+dstar^(-gamma)*(1-q-(1-q)^(dmax-dstar+1)))
r2_fun    <- function(r1,gamma,q,dstar) r1*dstar^(-gamma)/(1-q)^(dstar-1)
l1_model8 <- function(gamma1,gamma2,dstar) dstar^(-gamma2)/(H(dstar,gamma1)*dstar^(-gamma2) + dstar^(-gamma1)*(zeta(gamma2,shift=dstar+1)))
l1_model9 <- function(gamma1,gamma2,dstar,dmax) 1/(H(dstar,gamma1) + (dstar^(-gamma1)/dstar^(-gamma2))*(H(dmax,gamma2)-H(dstar,gamma2)))
l2_fun    <- function(l1,gamma1,gamma2,dstar) l1*dstar^(gamma2)/dstar^(gamma1)


model0  <- function(d,n,cumul=F) { 
  y <- 2*(n-d)/(n*(n-1))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}

model1  <- function(d,q,plot=F,cumul=F) { 
  if (plot==T) d <- 1:max(d)
  y = q*(1-q)^(d-1)
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}

model2  <- function(d,q,dmax,cumul=F) {
  y <- (q*(1-q)^(d-1))/(1-(1-q)^(dmax))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}


model3 <- function(d,q1,q2,dstar,cumul=F) {
  c1 <- c1_model3(q1,q2,dstar)
  c2 <- c2_fun(c1,q1,q2,dstar)
  y <- ifelse(d<=dstar,c1*(1-q1)^(d-1),c2*(1-q2)^(d-1))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}

model4 <- function(d,q1,q2,dstar,dmax,cumul=F) {
  c1 <- c1_model4(q1,q2,dstar,dmax)
  c2 <- c2_fun(c1,q1,q2,dstar)
  y <- ifelse(d<=dstar,c1*(1-q1)^(d-1),c2*(1-q2)^(d-1))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
} 

model5  <- function(d,dmax,gamma,cumul=F) {
  y <- d^(-gamma)/(H((dmax),gamma))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}

model6 <- function(d,gamma,q,dstar,cumul=F) {
  r1 <- r1_model6(gamma,q,dstar)
  r2 <- r2_fun(r1,gamma,q,dstar)
  y <- ifelse(d<=dstar,r1*d^(-gamma),r2*(1-q)^(d-1))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}

model7  <- function(d,gamma,q,dstar,dmax,cumul=F) {
  r1 <- r1_model7(gamma,q,dstar,dmax)
  r2 <- r2_fun(r1,gamma,q,dstar)
  y <- ifelse(d<=dstar,r1*d^(-gamma),r2*(1-q)^(d-1))
  if (cumul) cumforward(y) else y
  return(data.frame("x_vals" = d,"y_pred"=y))
}




# - log-likelihoods ------------
epsilon_q <- 10^-8

# Model 0
MS_model0 <- function(tab,d_vals,freq_d) {
  ### Random
  # supply
  loglik_0_ext <- if (ncol(tab)>2 & length(unique(tab$sent_n))!=1) sapply(unique(tab$sent_n), function(n_w) {
    tab_w <- filter(tab, sent_n == n_w) %>% group_by(d) %>% 
      summarise(d=d,freq_d = n()) %>%
      distinct(d,.keep_all = T) 
    N_n <- sum(tab_w$freq_d)
    W_n <- W(tab_w$d,tab_w$freq_d,n_w)
    N_n*log(2/(n_w*(n_w-1))) + W_n
  }) %>% sum()
  
  # estimate
  N <- nrow(tab)
  
  mloglik_0 <- function(dmax) {
    -( W(d_vals,freq_d,dmax+1) + N*log(2/(dmax*(dmax+1))) )
  }
  
  mle_0 <- if (ncol(tab)<3 | length(unique(tab$sent_n))==1) tryCatch( {
    mle(mloglik_0, start = list(dmax = max(d_vals)),method = "L-BFGS-B",
        lower = c(dmax = max(d_vals)))
  }, warning=function(e) {
    print("trying mle2")
    print(e)
    mle2(mloglik_0, start = list(dmax = max(d_vals)),method = "L-BFGS-B",
         lower = c(dmax = max(d_vals)))
  }, error=function(e) { 
    message(e)
    mle2(mloglik_0, start = list(dmax = max(d_vals)),method = "L-BFGS-B",
         lower = c(dmax = max(d_vals)))
  })
  
  m2logL <- if (ncol(tab)<3 | length(unique(tab$sent_n))==1) attributes(summary(mle_0))$m2logL else loglik_0_ext*-2
  pars   <- if (ncol(tab)<3 | length(unique(tab$sent_n))==1) attributes(summary(mle_0))$coef[1] else "-" 
  return(list(m2logL=m2logL, pars=pars,pars_names= c("0:dmax")))
}


# Model 1
MS_model1 <- function(q_init,d_vals,freq_d,N,M) {
  mloglik_1 <- function(q) - ( (M - N) * log(1 - q) + N*log(q) )
  
  mle_1 <- tryCatch( {
    mle(mloglik_1, start = list(q = q_init),method = "L-BFGS-B",
        lower = c(epsilon_q),upper = c(1-epsilon_q))
  }, error=function(e) { 
    message(e)
    NULL })
  
  m2logL <- attributes(summary(mle_1))$m2logL
  pars   <- attributes(summary(mle_1))$coef[1]
  return(list(m2logL=m2logL, pars=pars, pars_names=c("1:q")))
}

# Model 2
MS_model2 <- function(q_init,d_vals,freq_d,N,M) {
    mloglik_2 <- function(q,dmax) - ( (M - N)*log(1 - q) + N*log(q/(1-(1-q)^dmax)) )
    
    mle_2 <- tryCatch( {
     mle(mloglik_2, start = list(q = q_init,dmax=max(d_vals)),method = "L-BFGS-B",
          lower = list(q = epsilon_q,dmax=max(d_vals)), 
          upper = list(q = 1-epsilon_q))
    }, error=function(e) { 
     mle2(mloglik_2, start = list(q = q_init,dmax=max(d_vals)),method = "L-BFGS-B",
          lower = list(q = epsilon_q,dmax=max(d_vals)), 
          upper = list(q = 1-epsilon_q))
    }, error=function(e) {
      message(e)
      NULL })
    
    m2logL <- attributes(summary(mle_2))$m2logL
    pars   <- lapply(1:2,function(n) attributes(summary(mle_2))$coef[n]) %>% unlist()
    if (m2logL < 2) m2logL <- mloglik_2(pars[1],pars[2])*2
    return(list(m2logL=m2logL, pars=pars, pars_names=c("2:q","2:dmax")))
}



# Model 4
MS_model4 <- function(d_vals,freq_d,N,M,dstar_vals,dmax_vals) {
  mle_4_ls <- lapply(dmax_vals,function(dmax) {
    mle_4_dstar <-  lapply(dstar_vals,function(dstar) {
      print(dstar)
      rel_freq <- freq_d/N
      df <- cbind(p=unname(rel_freq), d=d_vals) %>% data.frame()
      lm1 <- lm(log(p) ~ d, data=df[df$d<=dstar,])
      q1_init <- 1-exp(lm1$coefficients[2])
      thresh2 <- ifelse(dstar %in% d_vals,dstar,max(d_vals[d_vals < dstar]))
      lm2 <- lm(log(p) ~ d, data=df[df$d>=thresh2,])
      q2_init <- 1-exp(lm2$coefficients[2])
      q2_init <- ifelse(q2_init<epsilon_q,epsilon_q,q2_init)
      
      mloglik_4 <- function(q1,q2) {
        c1 <- c1_model4(q1,q2,dstar,dmax)
        c2 <- c2_fun(c1,q1,q2,dstar)
        Mstar <- M_star(d_vals,freq_d,dstar)
        Nstar <- N_star(d_vals,freq_d,dstar)
        - ( Nstar*log(c1) + (N-Nstar)*log(c2) + (Mstar - Nstar)*log((1-q1)/(1-q2)) + (M - N)*log(1-q2) )
      }
      mle_4 <- tryCatch( {
        mle(mloglik_4,start = list(q1 = q1_init, q2 = q2_init), method = "L-BFGS-B",
            lower = list(q1 = epsilon_q, q2 = epsilon_q), 
            upper = list(q1 = 1-epsilon_q, q2 = 1-epsilon_q))
      }, error=function(e) {
        print("Error in mle:")
        message(e)
        print("Trying mle2:")
        mle2(mloglik_4, start = list(q1 = q1_init, q2 = q2_init), method = "L-BFGS-B",
             lower = list(q1 = epsilon_q, q2 = epsilon_q),
             upper = list(q1 = 1-epsilon_q, q2 = 1-epsilon_q))
      }, warning=function(e) {
        print("Warning in mle2:")
        message(e)
        mle2(mloglik_4, start = list(q1 = q1_init, q2 = q2_init), method = "L-BFGS-B",
             lower = list(q1 = epsilon_q, q2 = epsilon_q),
             upper = list(q1 = 1-epsilon_q, q2 = 1-epsilon_q))
      })
      m2logL <- attributes(summary(mle_4))$m2logL
      pars   <- lapply(1:2,function(n) attributes(summary(mle_4))$coef[n]) %>% unlist() %>% c(dstar)
      list(m2logL=m2logL, pars=pars)
  })

  m2logLiks <- sapply(mle_4_dstar,"[[",1)
  mle_4     <- mle_4_dstar[[which.min(m2logLiks)]]
  mle_4$pars       <-  c(mle_4$pars,dmax)
  mle_4$pars_names <- c("4:q1","4:q2","4:dstar","4:dmax")
  mle_4
 })
  
  m2logLiks <- sapply(mle_4_ls,"[[",1)
  mle_4 <- mle_4_ls[[which.min(m2logLiks)]]
  mle_4
}

# Model 5
MS_model5 <- function(gamma_init,d_vals,freq_d,N) {
    mloglik_5 <- function(dmax,gamma) - (- gamma*M_prime(d_vals,freq_d) - N*log(H(dmax,gamma)) )
    
    mle_5 <- tryCatch( {
      mle(mloglik_5, start = list(dmax=max(d_vals),gamma = gamma_init),method = "L-BFGS-B",
          lower = list(dmax=max(d_vals),gamma = 0.00))
    }, error=function(e) { 
      mle2(mloglik_5, start = list(dmax=max(d_vals),gamma = gamma_init),method = "L-BFGS-B",
           lower = list(dmax=max(d_vals),gamma = 0.00))
    }, error=function(e) {
      message(e)
      NULL })
    
    m2logL <- attributes(summary(mle_5))$m2logL
    pars   <- lapply(1:2,function(n) attributes(summary(mle_5))$coef[n]) %>% unlist()
    if (m2logL < 2) m2logL <- mloglik_5(pars[1],pars[2])*2
    list(m2logL=m2logL, pars=pars, pars_names=c("5:dmax","5:gam"))
}

# Model 6
MS_model6 <- function(d_seq,freq_d,N,M,dstar_vals) {
  d_vals <- sort(unique(d_seq))
  mle_6_ls <- lapply(dstar_vals,function(dstar) {
    gamma_init <- 1 + length(d_seq[d_seq<=dstar])/sum(log(d_seq[d_seq<=dstar]/min(d_seq[d_seq<=dstar])))
    q_init <- 1/mean(d_seq[d_seq>=dstar])
    q_init <- ifelse(q_init<epsilon_q,epsilon_q,q_init)
    
    mloglik_6 <- function(gamma,q) {
      r1 <- r1_model6(gamma,q,dstar)
      r2 <- r2_fun(r1,gamma,q,dstar)
      Mprimestar <- M_prime_star(d_vals,freq_d,dstar)
      Mstar      <- M_star(d_vals,freq_d,dstar)
      Nstar      <- N_star(d_vals,freq_d,dstar)
      - ( Nstar*log(r1) -gamma*Mprimestar + (N - Nstar)*log(r2) + (M - Mstar - N + Nstar)*log(1-q) )
    }

    mle_6 <- tryCatch( {
      mle(mloglik_6, start = list(gamma = gamma_init, q = q_init),method = "L-BFGS-B",
          lower = list(gamma = 0.00, q = epsilon_q), 
          upper = list(q = 1-epsilon_q))
    }, error=function(e) {
      message(e)
      print("trying mle2")
      mle2(mloglik_6, start = list(gamma = gamma_init, q = q_init),method = "L-BFGS-B",
           lower = list(gamma = 0.00, q = epsilon_q), 
           upper = list(q = 1-epsilon_q))
    }, warning=function(e) { 
      mle2(mloglik_6, start = list(gamma = gamma_init, q = q_init),method = "L-BFGS-B",
           lower = list(gamma = 0.00, q = epsilon_q), 
           upper = list(q = 1-epsilon_q)) 
    } )
    m2logL <- attributes(summary(mle_6))$m2logL
    pars <- lapply(1:2,function(n) attributes(summary(mle_6))$coef[n]) %>% unlist()
    list(m2logL=m2logL,pars=pars)
  })
  
  m2logLiks <- sapply(mle_6_ls,"[[",1)
  mle_6     <- mle_6_ls[[which.min(m2logLiks)]]
  mle_6$pars       <- append(mle_6$pars,dstar_vals[[which.min(m2logLiks)]])
  mle_6$pars_names <- c("6:gamma","6:q","6:dstar")
  return(mle_6)
}


# Model 7
MS_model7 <- function(d_seq,freq_d,N,M,dstar_vals,dmax_vals) {
  d_vals <- sort(unique(d_seq))
  mle_7_ls <- lapply(dmax_vals,function(dmax) {
    mle_7_dstar <-  lapply(dstar_vals,function(dstar) {
      gamma_init <- 1 + length(d_seq[d_seq<=dstar])/sum(log(d_seq[d_seq<=dstar]/min(d_seq[d_seq<=dstar])))
      q_init <- 1/mean(d_seq[d_seq>=dstar])
      q_init <- ifelse(q_init<epsilon_q,epsilon_q,q_init)
      
      mloglik_7 <- function(gamma,q) {
        r1 <- r1_model7(gamma,q,dstar,dmax)
        r2 <- r2_fun(r1,gamma,q,dstar)
        Mprimestar <- M_prime_star(d_vals,freq_d,dstar)
        Mstar      <- M_star(d_vals,freq_d,dstar)
        Nstar      <- N_star(d_vals,freq_d,dstar)
        - ( Nstar*log(r1) -gamma*Mprimestar + (N - Nstar)*log(r2) + (M - Mstar - N + Nstar)*log(1-q) )
      }
      mle_7 <- tryCatch( {
        mle(mloglik_7, start = list(gamma = gamma_init, q = q_init),method = "L-BFGS-B",
            lower = list(gamma = 0.00, q = epsilon_q), 
            upper = list(q = 1-epsilon_q))
      }, error=function(e) {
        message(e)
        print("trying mle2")
        mle2(mloglik_7, start = list(gamma = gamma_init, q = q_init),method = "L-BFGS-B",
             lower = list(gamma = 0.00, q = epsilon_q), 
             upper = list(q = 1-epsilon_q))
      }, warning=function(e) { 
        mle2(mloglik_7, start = list(gamma = gamma_init, q = q_init),method = "L-BFGS-B",
             lower = list(gamma = 0.00, q = epsilon_q), 
             upper = list(q = 1-epsilon_q)) 
      })
      m2logL <- attributes(summary(mle_7))$m2logL
      pars <- lapply(1:2,function(n) attributes(summary(mle_7))$coef[n]) %>% unlist() %>% c(dstar)
      list(m2logL=m2logL,pars=pars)
    })
    
    m2logLiks <- sapply(mle_7_dstar,"[[",1)
    mle_7     <- mle_7_dstar[[which.min(m2logLiks)]]
    mle_7$pars       <-  c(mle_7$pars,dmax)
    mle_7$pars_names <- c("7:gamma","7:q","7:dstar","7:dmax")
    mle_7
  })
  
  m2logLiks <- sapply(mle_7_ls,"[[",1)
  mle_7 <- mle_7_ls[[which.min(m2logLiks)]]
  mle_7
}


RunModelSelection <- function(objects,type="words",collection=NULL) {
  ### objects: models or languages
  ### type: "artif","words"
  
  criterion <- ifelse(type=="artif","BIC","AIC")
  # run selection for each language or model
  rows <- lapply(objects, function(obj) {
    print(obj)
    tab <- if (type == "artif") { 
      ReadArtif(obj)
    } else { 
      read.csv(paste0("data/real/collections/",collection,'.csv')) %>% filter(ISO_language==obj)
    } 
    estimates <- GetMleEst(tab,criterion)$df
    cbind(estimates, obj)
  })
  
  # bind results
  real_df  <- do.call(rbind,rows) %>% data.frame() 
  obj_name <- ifelse(type=='artif','model','ISO_language')
  colnames(real_df) <- c(sub("X","",colnames(real_df))[-ncol(real_df)],obj_name)
  real_df$best      <- colnames(real_df)[apply(real_df[1:8],1,function(x) which(x==min(x)))]
  if (type!='artif') real_df$coll <- collection
  real_df
}

# Model 3
MS_model3 <- function(d_vals,freq_d,N,M,dstar_vals) {
  
  # TEST
  eps_3 <- 10^-3
  eps_8 <- 10^-8
  
  mle_3_ls <- lapply(dstar_vals,function(dstar) {
    # initial values
    print(dstar)
    rel_freq <- freq_d/N
    df <- cbind(p=unname(rel_freq), d=d_vals) %>% data.frame()
    lm1 <- lm(log(p) ~ d, data=df[df$d<=dstar,])
    q1_init <- 1-exp(lm1$coefficients[2])
    thresh2 <- ifelse(dstar %in% d_vals,dstar,max(d_vals[d_vals < dstar]))
    lm2 <- lm(log(p) ~ d, data=df[df$d>=thresh2,])
    q2_init <- 1-exp(lm2$coefficients[2])
    q2_init <- ifelse(q2_init<=eps_8,eps_8,q2_init)
    print(q1_init)
    print(q2_init)
    
    mloglik_3 <- function(q1,q2) {
      c1 <- c1_model3(q1,q2,dstar)
      c2 <- c2_fun(c1,q1,q2,dstar)
      Mstar <- M_star(d_vals,freq_d,dstar)
      Nstar <- N_star(d_vals,freq_d,dstar)
      - ( Nstar*log(c1) + (N-Nstar)*log(c2) + (Mstar - Nstar)*log((1-q1)/(1-q2)) + (M - N)*log(1-q2) )
    }
    mle_3 <- tryCatch( {
      mle(mloglik_3, start = list(q1 = q1_init, q2 = q2_init),method = "L-BFGS-B",
          lower = list(q1 = eps_8, q2 = eps_8), 
          upper = list(q1 = 1-eps_8, q2 = 1-eps_3))
    }, error=function(e) {
      message(e)
      print("trying mle2")
      mle2(mloglik_3, start = list(q1 = q1_init, q2 = q2_init),method = "L-BFGS-B",
           lower = list(q1 = eps_8, q2 = eps_8), 
           upper = list(q1 = 1-eps_8, q2 = 1-eps_3))
    }, warning=function(e) { 
      mle2(mloglik_3, start = list(q1 = q1_init, q2 = q2_init),method = "L-BFGS-B",
           lower = list(q1 = eps_8, q2 = eps_8), 
           upper = list(q1 = 1-eps_8, q2 = 1-eps_3))
    })
    m2logL <- attributes(summary(mle_3))$m2logL
    pars   <- lapply(1:2,function(n) attributes(summary(mle_3))$coef[n]) %>% unlist()
    list(m2logL=m2logL, pars=pars)
  })
  
  m2logLiks        <- sapply(mle_3_ls,"[[",1)
  mle_3            <- mle_3_ls[[which.min(m2logLiks)]]
  mle_3$pars       <- append(mle_3$pars,dstar_vals[[which.min(m2logLiks)]])
  mle_3$pars_names <- c("3:q1","3:q2","3:dstar")
  return(mle_3)
}

GetMleEst <- function(tab,criterion) {
  # fixed variables
  d_seq <- tab$d
  d_vals <- sort(unique(d_seq))
  freq_d <- table(d_seq)
  M <- sum(freq_d*d_vals)
  N <- length(d_seq)
  dstar_vals <- d_vals[2:(length(d_vals)-1)]
  dmax_vals  <- seq(max(d_vals),max(d_vals)+10)
  
  # initial values
  q_init <- 1/mean(d_seq)
  gamma_init <- 1 + N/sum(log(d_seq/min(d_seq)))
  
  mle_0     <- tryCatch( { MS_model0(tab,d_vals,freq_d)                                 
                    }, error=function(e) {
                      print("while fitting model 0 ...")
                      message (e);NULL})  
  mle_1     <- tryCatch( { MS_model1(q_init,d_vals,freq_d,N,M)            
                    }, error=function(e) {
                      print("while fitting model 1 ...")
                      message (e);NULL})  
  mle_2     <- tryCatch( { MS_model2(q_init,d_vals,freq_d,N,M)            
                    }, error=function(e) {
                      print("while fitting model 2 ...")
                      message (e);NULL})  
  mle_3     <- tryCatch( { MS_model3(d_vals,freq_d,N,M,dstar_vals) 
                    }, error=function(e) {
                      print("warning while fitting model 3 ...")
                      message (e)
                      MS_model3(d_vals,freq_d,N,M,dstar_vals) 
                    })
  mle_4     <- tryCatch( { MS_model4(d_vals,freq_d,N,M,dstar_vals,dmax_vals) 
                    }, error=function(e) {
                      print("while fitting model 4 ...")
                      message (e)
                      MS_model4(d_vals,freq_d,N,M,dstar_vals,dmax_vals) 
                      }) 
  mle_5     <- tryCatch( { MS_model5(gamma_init,d_vals,freq_d,N)          
                    }, error=function(e) {
                      print("while fitting model 5 ...")
                      message (e);NULL}) 
  mle_6     <- tryCatch( { MS_model6(d_seq,freq_d,N,M,dstar_vals)          
                    }, error=function(e) {
                      print("while fitting model 6 ...")
                      message (e)
                      MS_model6(d_seq,freq_d,N,M,dstar_vals)  
                      }) 
  mle_7     <- tryCatch( { MS_model7(d_seq,freq_d,N,M,dstar_vals,dmax_vals)          
                    }, error=function(e) {
                      print("while fitting model 7 ...")
                      message (e)
                      MS_model7(d_seq,freq_d,N,M,dstar_vals,dmax_vals)  
                      }) 
  
  mle_models   <- list(mle_0,mle_1,mle_2,mle_3,mle_4,mle_5,mle_6,mle_7)
  m2logL       <- sapply(mle_models,"[[",1) %>% unlist()
  params       <- sapply(mle_models,"[[",2) %>% unlist()
  params_names <- sapply(mle_models,"[[",3) %>% unlist()
  
  df <- data.frame(t(c(m2logL,params)))
  colnames(df) <- c(artif_models,params_names)
  K <- if (ncol(tab)<3 | length(unique(tab$sent_n))==1) c(1, 1, 2, 3, 4, 2, 3, 4) else c(0, 1, 2, 3, 4, 2, 3, 4)
  df[-grep("0:dmax",colnames(df))] <- as.numeric(df[-grep("0:dmax",colnames(df))])
  df[1,1:length(artif_models)] <- sapply(1:length(K[1:length(artif_models)]),
                                            ifelse(criterion=="AIC",GetAIC,GetBIC),
                                            est_df=df,N=N,K=K)
  return(list(df=df,N=N))
}





# IC diff 
DfICdiff <- function(df,type){
  ICs_diff <- df[,1:length(artif_models)]
  ICs_diff[,] <- apply(ICs_diff[,],1,function(x) x - min(x)) %>% t()
  ICs_diff$best <- colnames(ICs_diff)[apply(ICs_diff,1,function(x) which(x==min(x)))]
  if (type=="artif")  ICs_diff$model <- artif_models else ICs_diff$language <- LANGS
  ICs_diff
  }

# best parameters
Dfparams <- function(df) df[-(1:length(artif_models))] 



### obtain list of predicted probabilities for every model
YPredList <- function(best_parameters_df,x_vals,sent_n) {
  dmax       <-  as.numeric(best_parameters_df[,1])
  y_pred0    <- if (is.na(dmax)) model0(x_vals,sent_n) else model0(x_vals,dmax+1)
  q          <-  as.numeric(best_parameters_df[,2])
  y_pred1    <- model1(x_vals,q)
  q          <-  as.numeric(best_parameters_df[,3])
  dmax       <-  as.numeric(best_parameters_df[,4])
  y_pred2    <- model2(x_vals,q,dmax)
  q1         <-  as.numeric(best_parameters_df[,5])
  q2         <-  as.numeric(best_parameters_df[,6])
  dstar      <-  as.numeric(best_parameters_df[,7])
  y_pred3    <- model3(x_vals,q1,q2,dstar)
  q1         <-  as.numeric(best_parameters_df[,8])
  q2         <-  as.numeric(best_parameters_df[,9])
  dstar      <- as.numeric(best_parameters_df[,10])
  dmax       <- as.numeric(best_parameters_df[,11])
  y_pred4    <- model4(x_vals,q1,q2,dstar,dmax)
  dmax       <- as.numeric(best_parameters_df[,12])
  gam        <- as.numeric(best_parameters_df[,13])
  y_pred5    <- model5(x_vals,dmax,gam)
  gamma      <-  as.numeric(best_parameters_df[,14])
  q          <-  as.numeric(best_parameters_df[,15])
  dstar      <-  as.numeric(best_parameters_df[,16])
  y_pred6    <- model6(x_vals,gamma,q,dstar)
  gamma      <-  as.numeric(best_parameters_df[,17])
  q          <-  as.numeric(best_parameters_df[,18])
  dstar      <- as.numeric(best_parameters_df[,19])
  dmax       <- as.numeric(best_parameters_df[,20])
  y_pred7    <- model7(x_vals,gamma,q,dstar,dmax)
  
  y_list <- list(y_pred0,y_pred1,y_pred2,y_pred3,y_pred4,y_pred5,y_pred6,y_pred7) 
  return(Filter(Negate(is.null), y_list))
} 





# simulation --------------------------------------
BinSearch <- function(u,vector) {
   indices <- 1:(length(vector))
   low <- indices[1]
   up  <- indices[length(indices)]
   while (low!=up) {
     center <- ceiling((up-low)/2)+low
     if (u<=vector[center] & u>vector[center-1]) {
       return(center-1)
     } else if (u > vector[center]) {
       low <- center
     } else if (u < vector[center]) {
       up <- center
     }
   }
   return(center)
}
 

SimData <- function(model,n,q,q1,q2,dstar,gamma,N) {
   set.seed(327)
   sampleN <- integer()
   b       <- 2^(gamma-1)
   lambda  <- log(1-q)
   dmax <- if (model %in% c('3','6')) 10^6 else n-1
   if (model %in% c("1","2")) {
     while(length(sampleN) < N) {
       x <- runif(1,0,1)
       L = 1 + floor(log(x)/lambda)
       if (model=="2") {
         if (L<n) { sampleN <- append(sampleN,L) 
         } else {sampleN <- sampleN}
       } else if (model=="1") sampleN <- append(sampleN,L)
     }
   } else if (model %in% c("0","3","4","6","7")) {
     x <- 1:dmax
     p <- switch(model,'0'=model0(x,n),'3'=model3(x,q1,q2,dstar),'4'=model4(x,q1,q2,dstar,dmax),
                       '6'=model6(x,gamma,q,dstar),'7'=model7(x,gamma,q,dstar,dmax))
     vector <- c(0,cumsum(p$y_pred))
     u_list <- runif(N)
     sampleN <- sapply(u_list, BinSearch, vector=vector)
     
   } else if (model == "5") { 
     while(length(sampleN) < N) {
       U <- runif(1,0,1)
       V <- runif(1,0,1)
       X <- floor(U^(-1/(gamma-1)))
       T <- (1 + 1/X)^(gamma-1)
       if ( V*X*(T-1)/(b-1) <= T/b & X < n) sampleN <- append(sampleN,X)
     }
   }
   df <- data.frame(d = sampleN, sent_n = n)
   if (model %in% c("1","3","6"))  df$sent_n <- NULL
   df
 }
 
 

