# - PLOTS
source("aux_functions.R")



### arguments
# - type: "artif","real"

args = commandArgs(trailingOnly=TRUE)
args <- strsplit(args, ",")
if (length(args)==0) {
  print("Choose one of: 'artif','words'")
}

suffix <- ifelse(args[[1]]=="words","",paste("_",args[[1]],sep=""))



# ----------------------- ARTIFICIAL -----------------------------

if (args[[1]] == "artif") {
  ## sample vs real ------
  print("artif-plain")
  n <- 20; N <- 10000; q=0.2; q1=0.5; q2=0.1; dstar=4; gamma=1.6
  plot_list <- lapply(artif_models, function(mod) {
    PlotSampleVsReal(mod)
  })
  plot <- ggarrange(plotlist=plot_list, common.legend = TRUE, legend="bottom")
  ggsave("figures/artificial/sample_vs_real_p.pdf")
  
  ## BIC differences ------
  print("artif-BIC diff")
  labs_rs <- paste("Model",artif_models, 'sample'); names(labs_rs) <- artif_models
  # read
  df <- read.csv("results/artificial/ms_results.csv",check.names = F)
  BIC_df <- DfICdiff(df,'artif') %>% melt(id.vars=c("model","best")) %>% 
    suppressWarnings() %>% rename(fitted_model=variable,BIC_diff=value) %>%
    transform(best=factor(best))
  # plot
  ggplot(BIC_df,aes(fitted_model,BIC_diff,fill=fitted_model)) + geom_bar(stat="identity") + 
    facet_wrap(~model,scales = "free_y",labeller = labeller(model=labs_rs)) + geom_vline(aes(xintercept=best),color="red") +
    theme(legend.position = "bottom") + labs(y = "BIC difference",x ="fitted model") +
    scale_color_manual(labels = c("best model", "theoretical"), values = c("red", "darkgreen")) +
    guides(fill=guide_legend(title="Model")) + scale_fill_manual(values=colors)
  ggsave("figures/artificial/BICs_plot.pdf")
  
  ## Best fitted --------
  print("artif-best fitted")
  plot_list <- lapply(artif_models, function(mod) DfModSelArtif(mod,criterion="BIC"))
  plot <- ggarrange(plotlist=plot_list, common.legend = TRUE, legend="bottom")
  ggsave("figures/artificial/best_predicted_BIC.pdf")
}





# -------------------------- MIXED N --------------------------

if (args[[1]] != "artif") {
  ## plain data -------
  print("real-plain")
  plain_real <- lapply(COLLS, function(collection) {
    tabb <- read.csv(paste0("data/real/collections/",collection,".csv"))
    # - stats
    plain_stat <- lapply(c("mean","modal"), function(stats) {
      x <- PlotStatN(tabb,collection,stat=stats)
      ggsave(paste0("figures/real/",collection,"/",collection,"_",stats,".pdf"),
             width = 9, height = 5)
    })
  })
  
  ## Best fitted -------
  print("real-best fitted")
  list <- lapply(COLLS, function(collection) {
    # plot in three groups of languages
    plot_list <- lapply(ISO[1:12], function(lang) DfBestModel(lang,collection))
    ggarrange(plotlist=plot_list, common.legend = TRUE, legend="bottom")
    ggsave(paste0("figures/real/",collection,"/",collection,"_best_fitted_1.pdf"))
    
    plot_list <- lapply(ISO[13:20], function(lang) DfBestModel(lang,collection))
    ggarrange(plotlist=plot_list, common.legend = TRUE, legend="bottom")
    ggsave(paste0("figures/real/",collection,"/",collection,"_best_fitted_2.pdf"))
    
  })
  
  # best model and language characteristics
  list <- lapply(COLLS, function(collection) {
    best_model <- read.csv(paste0("results/real/mixed_n/ms_results.csv")) %>% 
      dplyr::select(ISO_language, best, coll) %>% filter(coll==collection)
    df <- merge(best_model, langs_info, by = 'ISO_language') %>% 
      mutate(ISO_language=NULL, best = case_when(best %in% c(3,4) ~ '3-4', best %in% c(6,7) ~ '6-7'))
    
    p_script <- ggplot(df) + geom_bar(aes(x=script,fill=best), stat='count') + 
      facet_grid(~best) + coord_flip() + labs(y='number of languages') +
      theme(legend.position = 'none',text = element_text(size=20))
    
    p_family <- ggplot(df) + geom_bar(aes(x=family,fill=best), stat='count') + 
      facet_grid(~best) + coord_flip() + labs(y='number of languages') +
      theme(legend.position = 'none',text = element_text(size=20))
    
    ggarrange(p_script,p_family,nrow=2)
    ggsave(paste0("figures/real/",collection,"/best_and_info_",collection,".pdf"))
  })
    
}


# plot for presentation
ISO_lang <- 'ita'
collection <- 'PSUD'

DfBestModel(ISO_lang,collection)
ggsave(paste0("italian_lin_lin.png"))



if (args[[1]] != "artif") {

  ## slopes plot -----------
  print("real-slopes plot")
  # read
  melt_df <- read.csv("results/real/mixed_n/slopes.csv")
  melt_df <- melt_df %>% mutate(language= langs_map[melt_df$ISO_language]) %>% 
    reshape2::melt(id.vars=c("ISO_language","best","coll",'language')) %>% 
    group_by(coll,variable) %>% 
    mutate(outlier = if_else(is_outlier(value), language, NA_character_))
  # plot
  slopes <- ggplot(data=subset(melt_df, variable != "ratio"),aes(x=variable,y=value,fill=variable)) + geom_boxplot() +
    geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F, size = 5) + 
    theme(legend.position = "none", text = element_text(size=20))+ 
    scale_x_discrete(breaks=c('q1','q2'),labels=expression(q[1],q[2])) +
    facet_wrap(~factor(coll,levels=c("PUD","PSUD"))) + labs(x='parameter')
  ggsave("figures/real/q1q2_boxplots.pdf")
  ratio <- ggplot(data=subset(melt_df, variable == "ratio"),aes(x=variable,y=value)) + geom_boxplot(fill="green") +
    geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F, size = 5) + 
    theme(text = element_text(size=20)) +
    scale_x_discrete(breaks=c('ratio'),labels=expression(q[1]/q[2])) +
    facet_wrap(~factor(coll,levels=c("PUD","PSUD"))) + labs(x='paramters ratio')
  ggsave("figures/real/q1q2_ratio_boxplots.pdf")
  

}





# -------------------------------◊ FIXED N ◊----------------------------------

if (args[[1]] != "artif") {
  print("real-violin plot")
  
  ## violin plot -------
  summary_fixed <- read.csv("results/real/fixed_n/ms_results.csv")
  best_dstar_ls <- lapply(COLLS, function(collection) best_dstar_mixed_n(collection))
  df_dstar <- do.call(rbind,best_dstar_ls) %>% rename(dstar_indep = dstar,best_indep = best)
  df <- merge(summary_fixed,df_dstar, by=c('ISO_language','coll') )
  
  # plot
  ggplot(df, aes(x=factor(ISO_language,levels=ISO))) + 
    geom_bar(aes(y=dstar_indep,fill=factor(best_indep)),position="dodge", stat="identity") + 
    geom_violin(aes(y=dstar),trim=T) +
    facet_wrap(~factor(coll,levels=c("PUD","PSUD"))) + coord_flip() +
    scale_fill_manual(values=colors) + labs(x="language",y='d*') +
    theme(legend.position = "bottom",text = element_text(size=18)) + 
    scale_x_discrete(labels=labs,limits=rev) + guides(fill=guide_legend(title="best model \nindependent of n"))
  ggsave("figures/real/dstar_violinplot.pdf")

## for presentation
#  ggplot(filter(df,coll=='PSUD'), aes(x=factor(ISO_language,levels=ISO))) + 
#    geom_bar(aes(y=dstar_indep,fill=factor(best_indep)),position="dodge", stat="identity") + 
#    geom_violin(aes(y=dstar),trim=T) + theme_void() + 
#    scale_fill_manual(values=colors) + labs(x="language",y='d*') +
#    theme(legend.position = "none",text = element_text(size=18)) + 
#    scale_x_discrete(labels=labs) + guides(fill=guide_legend(title="best model \nindependent of n"))
  

  ## Best model -------
  # fixed n
  print("real-fixed n heatmap")
  hist_ls <- lapply(COLLS, function(collection) {
    top    <- heatmap_best_mixed_n(collection)
    main   <- heatmap_best_fixed_n(collection)
    ggarrange(main,top + rremove("xlab"),nrow=2,heights = c(7,2),
              common.legend = T,legend = "right")
    ggsave(paste0("figures/real/",collection,"/",collection,"_fixed_n_tiles.pdf"))
  })
  


  ## slopes plot  ------
  df <- read.csv("results/real/fixed_n/ms_results.csv") %>% 
    mutate(slopes=ifelse((q1>q2),'q1>q2','q1<=q2')) %>% filter(best %in% c(3,4,6,7))
  df %>% na.omit() %>% mutate(language=factor(ISO_language,levels=ISO)) %>% 
    ggplot(aes(y=language, x=sent_n, fill=slopes)) + geom_tile() + 
    facet_wrap(~factor(coll,levels=c('PUD','PSUD'))) + 
    scale_y_discrete(labels=labs, limits=rev) +
    labs(x="sentence length") + 
    guides(fill=guide_legend(title='relation')) +
    scale_fill_manual(values=c("#D55E00", "#56B4E9"), labels = expression(q[1]<=q[2],q[1]>q[2])) +
    theme(legend.position = 'bottom',text = element_text(size=20))
  ggsave("figures/real/fixed_n_slopes.pdf")
  
  
  # histogram ----------
  print("real-fixed n bars")
  hist_ls <- lapply(COLLS, function(collection) { 
      df <- read.csv("results/real/fixed_n/ms_results.csv") %>% filter(coll == collection) %>% na.omit()
      # histogram
      plot <- df %>% mutate(best=as.numeric(best), language = factor(ISO_language,levels=ISO)) %>%
        ggplot(aes(fill=type,x=type)) + geom_bar(stat = 'count') + 
        facet_wrap(~language, labeller = labeller(language=labs)) +
        guides(fill=guide_legend(title="best \nmodel \ntype")) +
        scale_fill_manual(values=colors_type) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) + ylim(c(0,58))
      ggsave(paste0("figures/real/",collection,"/",collection,"_fixed_n_bars.pdf"))
  })
  
  
  ## modal model for increasing threshold ----------
  print("real-fixed n threshold")
  threshold_ls <- lapply(COLLS, function(collection) { 
    df <- read.csv("results/real/fixed_n/ms_results.csv") %>% filter(coll == collection) %>% na.omit()
    df_thresh <- lapply(1:(max(df$sent_numb)), function(threshold) {
        tab <- filter(df, sent_numb >= threshold)
        aggregate(tab$type, list(tab$ISO_language), getmode) %>% mutate(threshold=threshold)
      })
    df_thresh <- do.call(rbind.data.frame,df_thresh)
    colnames(df_thresh)[1:2] <- c('ISO_language',"modal type")
    df_thresh %>% mutate(coll=collection)
  })
      
  do.call(rbind.data.frame,threshold_ls) %>% 
    ggplot(aes(y=factor(ISO_language,levels=ISO), x=threshold, fill=`modal type`)) + 
    facet_wrap(~factor(coll,levels=c('PUD','PSUD'))) + geom_tile() +
    theme(legend.position = 'bottom',text = element_text(size=18)) +
    labs(x="threshold",y="language") + guides(fill=guide_legend(title='most voted best \nmodel')) +
    scale_y_discrete(labels=labs,limits=rev) + scale_fill_manual(values=colors_type)
  ggsave("figures/real/fixed_n_threshold.pdf")




  ## OMEGA -------------
  print("real-fixed n omega heatmaps")
  combined_heatmap_ls <- lapply(COLLS, function(collection) { 
    # model selection
    df <- read.csv("results/real/fixed_n/ms_results.csv") %>% filter(coll == collection,sent_n <=20)
    df_ms <- na.omit(df) %>% add_missing_lengths()
    df_ms$labels <- ifelse(is.na(df_ms$best),"|","")
    
    plot1 <- mutate(df_ms,best=factor(best), language=factor(ISO_language,levels=ISO)) %>% 
      ggplot(aes(y=language, x=sent_n, fill=best)) + labs(x="sentence length") +
      geom_tile() + scale_x_continuous(breaks=seq(min(df_ms$sent_n),20,1)) +
      scale_y_discrete(labels=labs) + geom_text(aes(label=labels))+ 
      scale_fill_manual(values=colors,na.value = "#b9d0ed") + coord_flip() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=18)) + 
      guides(fill=guide_legend(title='best \nmodel')) +
      geom_vline(xintercept=3.5)
    ggsave(paste0("figures/real/",collection,"/",collection,"_omega_tiles.pdf"))
    
    # omega
    df <- add_missing_lengths(df)
    df$labels <- ifelse(is.na(df$omega),"|","")
    
    omega_plot <- mutate(df,language = factor(ISO_language,levels=ISO)) %>%
      ggplot(aes(y=language, x=sent_n, fill=omega)) + labs(x="sentence length", y="language") + 
      scale_y_discrete(labels=labs) + geom_tile() + geom_text(aes(label=labels)) + 
      coord_flip() + scale_x_continuous(breaks=seq(min(df$sent_n),20,1)) +
      theme(axis.text.x = element_text(angle = 45, hjust=1),text=element_text(size=18))+ 
      scale_fill_gradient2(midpoint=0, low="#fcba03",high = "#5c1c87", mid = "white", 
                           na.value = "#b9d0ed", name= expression(Omega)) +
      geom_vline(xintercept=3.5)
    ggsave(paste0("figures/real/",collection,"/",collection,"_omega.pdf"))
  })


} # condition on type argument



# plot of log(1-q) vs q
q <- seq(0,1,0.002)
data.frame('q'=q,'slope'=log(1-q)) %>% 
ggplot() + geom_point(aes(q,slope)) + theme(text=element_text(size=18)) +
  labs(y='log(1-q)')
ggsave(paste0("figures/real/q_plot.pdf"))

# plot of minimum requirement on regime size
tabb <- read.csv(paste0("data/real/collections/PSUD.csv")) %>% 
  filter(ISO_language=='ita') %>% group_by(sent_n) %>% filter(n_distinct(d) ==3) 
tab <- filter(tabb,sent_n==4)
df_real <- TableCumul(tab$d,cumul=F)
ggplot(df_real,aes(x_vals,y_pred)) + geom_point() + geom_line() +
  labs(x='d', y = 'p(d)') + theme(text=element_text(size=18))+ 
  scale_x_continuous(breaks= c(1,2,3)) + scale_y_log10()
ggsave(paste0("figures/real/min_size.pdf"))


# ----------- CHUNKS -------------


#if (args[[1]] %!in% c("artif","words")) {
#  # ---- ◊ d
#  tabs <- lapply(COLLS, function(COLL) {
#    read.csv(paste("data/real/collections/",COLL,suffix,".csv",sep="")) %>% mutate(coll=COLL)
#  })
#  tab <- do.call(rbind,tabs)
#  # - histogram
#  ggplot(tab, aes(x=sent_n,fill=coll)) + geom_bar() + facet_wrap(~factor(coll,levels=c("PUD","PSUD")),nrow=2) +
#    labs(x="number of chunks in sentence")
#  
#  
#  # ----- ◊ sizes
#  tabs <- lapply(COLLS, function(COLL) {
#    read.csv(paste("data/real/collections/",COLL,"_chunks_sizes",suffix,".csv",sep="")) %>% mutate(coll=COLL)
#  })
#  tab_sizes <- do.call(rbind,tabs)
#  
#  # - sizes statistics 
#  size_lev_df <- tab_sizes %>% group_by(coll,ISO_language) %>% 
#    summarise(mean_size = mean(chunk_size),sd_size = sd(chunk_size))
#  avg_df <- size_lev_df %>% group_by(coll) %>% summarise(avg_size=mean(mean_size)) 
#  # histogram
#  ggplot(size_lev_df,aes(x=ISO_language,y=mean_size,fill=coll)) + geom_bar(stat="identity",position="dodge") + 
#    geom_errorbar(aes(ymin=mean_size-sd_size, ymax=mean_size+sd_size), width=.2) +
#    facet_wrap(~factor(coll,levels=c("PUD","PSUD")),nrow=2) + geom_hline(aes(yintercept = avg_size),avg_df,linetype="dashed",color="blue") +
#    labs(x="language",y="mean chunk size") + scale_x_discrete(labels=labs)+
#    theme(axis.text.x = element_text(angle = 45, hjust=1))
#  ggsave(paste("figures/real/mean_chunks_size",suffix,".pdf",sep=""))
#  
#  
#  # - size dstar and n
#  tabs <- lapply(COLLS, function(COLL) {
#    read.csv(paste("data/real/",COLL,"_dstar_chunksize",suffix,".csv",sep=""))
#  })
#  df <- do.call(rbind,tabs)
#  # dstar and s against sent_n
#  plot <- ggplot(df) + geom_point(aes(sent_n,chunk_size,color="size")) + 
#    geom_point(aes(sent_n,dstar,color="dstar")) + facet_wrap(~factor(coll,levels=c("PUD","PSUD")),nrow=2)
#  ggsave(paste("figures/real/dstar_s",suffix,".pdf",sep=""))
#  
#  # dstar against s
#  df %>% group_by(coll,dstar,chunk_size) %>% summarise(count = n()) %>%
#  ggplot(aes(chunk_size, dstar, fill=count)) + geom_tile() +  facet_wrap(~factor(coll,levels=c("PUD","PSUD")),nrow=2) +
#    scale_fill_gradient2(midpoint=4000, low="black",high = "red", mid = "pink", na.value = "#b9d0ed") + coord_flip()
#  ggsave(paste("figures/real/dstar_vs_s",suffix,".pdf",sep=""))
#  









