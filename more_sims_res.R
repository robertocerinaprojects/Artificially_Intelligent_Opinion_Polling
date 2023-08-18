
# # # FUNCTION TO PLOT RELATIONSHIP BETWEEN TWO COVARIATES AND A SCORE
library(RColorBrewer)

plot_3d <-
  function(
    x1,x2,y,x1_lab,x2_lab,y_lab,x1lim,x2lim,cut.min,cut.max,name.pal,n.legend,leg.x,leg.y,rev,main
  ){
    
    cols <- brewer.pal(3, name.pal)
    pal <- colorRampPalette(cols)
    cuts <- seq(cut.min,cut.max,length.out = 31)
    pal.vec <- pal(30)
    pal.leg <- pal(n.legend)
    
    if(rev){
      pal.vec <- rev(pal.vec)
      pal.leg <- rev(pal.leg)
    }
    
    par(oma = c(0,1,0,8.5),mar = c(5,7.5,5,3.5))
    plot(
      x = x1,
      y = x2,
      xlab = x1_lab,
      ylab = x2_lab,
      pch = NA,
      main = main,
      bty = 'n',
      xlim = x1lim,
      ylim = x2lim
    )
    
    lattice <-
      expand.grid(
        x1 = seq(x1lim[1],x1lim[2],length.out = 300),
        x2 = seq(x2lim[1],x2lim[2],length.out = 300)
      )
    
    if(y_lab %in% c('pearson')){
      train <- mgcv::gam(formula = y ~ s(x1,x2),family=betar(link="logit"))
      preds_logit <- predict(train ,newdata = lattice)
      preds <- exp(preds_logit)/(1+exp(preds_logit))
    }
    if(y_lab %in% c('rmse','bias','cover')){
      y_log <- log(y)
      train <- mgcv::gam(formula = y_log ~ s(x1,x2))
      preds <- exp(predict(train ,newdata = lattice))
    }
    #if(y_lab %in% c('cover')){
    #  train <- mgcv::gam(formula = y ~ s(x1,x2))
    #  preds <- predict(train ,newdata = lattice)
    #}
    
    print(
      paste(
        'Limits to smooth y are:',
        paste0(round(c(min(preds),max(preds)),2),collapse = ','),
        '.\n adjust cut lims as needed.'
      )
    )
    
    points(
      x = lattice$x1,
      y = lattice$x2,
      col = pal.vec[as.integer(cut(preds,cuts))],
      pch = 15
    )
    legend <-
      paste(
        '(',
        round(seq(cut.min,cut.max,length.out = n.legend+1)[1:n.legend],2),
        ',',
        round(seq(cut.min,cut.max,length.out = n.legend+1)[2:(n.legend+1)],2),
        ']',
        sep = ''
      )
    legend[1] <- gsub('\\(','\\[',legend[1])
    legend(
      x = leg.x, y = leg.y,
      legend = legend,
      pch = 15,
      col = pal.leg,
      xpd = NA,
      bty = 'n',
      pt.cex = 2.5
    )
  }

# # # PENALTY AND PENALTY-VARIANCE PLOT
for(S in c('theta','pi')){
  for(Y in c('bias','rmse','pearson','cover')){
    for(J in c('SS.correct_str_Bern')){
      
      pdf(file = paste('plots/sims.extended.properties_penalty',S,'_',Y,'_',J,'.pdf',sep=''),width = 10,height = 5)
      
      
      if(Y=='bias'){cut.min = 0; cut.max = 0.1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='rmse'){cut.min = 0; cut.max = 0.2; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='pearson'){cut.min = 0.5; cut.max = 1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- FALSE}
      if(Y=='cover'){cut.min = 0; cut.max = 0.35; y <- abs(store.list[[paste(Y,S,J,sep="_")]]-0.9); rev <- TRUE}
      
      Ymain = gsub('cover','distance from 90% coverage',
                   gsub('pearson','pearson correlation',Y))
      
      plot_3d(
        x1 = store.list$penalty,
        x2 = store.list$var.penalty/(store.list$penalty*(1-store.list$penalty)),
        y = y,
        x1_lab = 'penalty',
        x2_lab =  bquote(frac('penalty variance','max(penalty variance)')),
        y_lab = Y,
        main =
          bquote(.(Ymain)~.(S)~':'~'online selection - structured - Bernoulli'),
        cut.min = cut.min,
        cut.max = cut.max,
        x1lim = c(0,1),
        x2lim = c(0,1),
        name.pal = 'Spectral',
        n.legend = 10,
        leg.x = 1.1*max(store.list$penalty),
        leg.y = max(store.list$var.penalty/(store.list$penalty*(1-store.list$penalty))),
        rev = rev
      )
      dev.off()
      
    } } }

# # # PREVALENCE v. SAMPLE PREVALENCE
for(S in c('theta','pi')){
  for(Y in c('bias','rmse','pearson','cover')){
    for(J in c('SS.correct_str_Bern')){
      
      pdf(file = paste('plots/sims.extended.properties_prevalence_',S,'_',Y,'_',J,'.pdf',sep=''),width = 10,height = 5)
      
      
      if(Y=='bias'){cut.min = 0; cut.max = 0.1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='rmse'){cut.min = 0; cut.max = 0.25; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='pearson'){cut.min = 0.5; cut.max = 1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- FALSE}
      if(Y=='cover'){cut.min = 0; cut.max = 0.5; y <- abs(store.list[[paste(Y,S,J,sep="_")]]-0.9); rev <- TRUE}
      
      Ymain = gsub('cover','distance from 90% coverage',
                   gsub('pearson','pearson correlation',Y))
      
      plot_3d(
        x1 = store.list$prevalence,
        x2 = store.list$prevalence.SS,
        y = y,
        x1_lab = 'population prevalence',
        x2_lab = 'sample prevalence',
        y_lab = Y,
        main =
          bquote(.(Ymain)~.(S)~':'~'online selection - structured - Bernoulli'),
        cut.min = cut.min,
        cut.max = cut.max,
        x1lim = c(0,1),
        x2lim = c(0,1),
        name.pal = 'Spectral',
        n.legend = 10,
        leg.x = 1.1*max(store.list$prevalence),
        leg.y = max(store.list$prevalence.SS),
        rev = rev
      )
      dev.off()
      
    } } }

# # # SAMPLE SIZE v. PENALTY
for(S in c('theta','pi')){
  for(Y in c('bias','rmse','pearson','cover')){
    for(J in c('SS.correct_str_Bern')){
      
      pdf(file = paste('plots/sims.extended.properties_n.penalty_',S,'_',Y,'_',J,'.pdf',sep=''),width = 10,height = 5)
      
      
      if(Y=='bias'){cut.min = 0; cut.max = 0.15; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='rmse'){cut.min = 0; cut.max = 0.35; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='pearson'){cut.min = 0.5; cut.max = 1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- FALSE}
      if(Y=='cover'){cut.min = 0; cut.max = 0.5; y <- abs(store.list[[paste(Y,S,J,sep="_")]]-0.9); rev <- TRUE}
      
      Ymain = gsub('cover','distance from 90% coverage',
                   gsub('pearson','pearson correlation',Y))
      
      plot_3d(
        x1 = store.list$n,
        x2 = store.list$penalty,
        y = y,
        x1_lab = 'n',
        x2_lab = 'penalty',
        y_lab = Y,
        main =
          bquote(.(Ymain)~.(S)~':'~'online selection - structured - Bernoulli'),
        cut.min = cut.min,
        cut.max = cut.max,
        x1lim = c(0,10000),
        x2lim = c(0,1),
        name.pal = 'Spectral',
        n.legend = 10,
        leg.x = 1.05*max(store.list$n),
        leg.y = max(store.list$penalty),
        rev = rev
      )
      dev.off()
      
    } } }

# # # PREVALENCE v. PENALTY
for(S in c('theta','pi')){
  for(Y in c('bias','rmse','pearson','cover')){
    for(J in c('SS.correct_str_Bern')){
      
      pdf(file = paste('plots/sims.extended.properties_prevalence.penalty_',S,'_',Y,'_',J,'.pdf',sep=''),width = 10,height = 5)
      
      
      if(Y=='bias'){cut.min = 0; cut.max = 0.1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='rmse'){cut.min = 0; cut.max = 0.35; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- TRUE}
      if(Y=='pearson'){cut.min = 0.5; cut.max = 1; y <- abs(store.list[[paste(Y,S,J,sep="_")]]); rev <- FALSE}
      if(Y=='cover'){cut.min = 0; cut.max = 0.5; y <- abs(store.list[[paste(Y,S,J,sep="_")]]-0.9); rev <- TRUE}
      
      Ymain = gsub('cover','distance from 90% coverage',
                   gsub('pearson','pearson correlation',Y))
      
      plot_3d(
        x1 = store.list$prevalence,
        x2 = store.list$penalty,
        y = y,
        x1_lab = 'prevalence',
        x2_lab = 'penalty',
        y_lab = Y,
        main =
          bquote(.(Ymain)~.(S)~':'~'online selection - structured - Bernoulli'),
        cut.min = cut.min,
        cut.max = cut.max,
        x1lim = c(0,1),
        x2lim = c(0,1),
        name.pal = 'Spectral',
        n.legend = 10,
        leg.x = 1.05*max(store.list$prevalence),
        leg.y = max(store.list$penalty),
        rev = rev
      )
      dev.off()
      
    } } }
