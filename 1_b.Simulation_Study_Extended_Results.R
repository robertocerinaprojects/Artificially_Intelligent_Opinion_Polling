# clean workspace
rm(list = ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen = 999)
# set timeout limit (laxed)
options(timeout = 10000)
# set work directory
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")

library(data.table)
library(mgcv)
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# load simulation results
load(file = 'data_generated/simulations_extended.RData')

# first compare marginal improvement in metrics
plot_compare <-
  function(y,col,xlab,ylab,main,lim.min,lim.max,
           legend.where,legend.size
           ){

    if(grepl('bias',main)){ y <- lapply(y,function(y){abs(y) } ) }

    plot(
      x = y[[2]],
      y = y[[1]],
      pch = NA,
      ylim = c(lim.min,lim.max),
      xlim = c(lim.min,lim.max),
      main = main,
      ylab = ylab,
      xlab = xlab
    )
    abline(0,1,col = adjustcolor('black',0.5),lty = 2)
    for(l in names(y)[-1]){
      y.temp <- y[[1]]
      x.temp <- y[[l]]

      points(
        x =  x.temp,
        y =  y.temp,
        pch = 1,
        col = adjustcolor(col[[l]],0.01),
        cex = 0.5
      )
      if(grepl('coverage',main)){
        fit <-
          mgcv::gam(
            formula = y.temp~s(x.temp,bs="cs")#,
            #family=scat(link="identity")
          )
        y.fit <- fit$fitted.values
      }
      if(grepl('bias|rmse',main)){
        fit <-
          mgcv::gam(
            formula = log(y.temp)~s(x.temp,bs="cs")#,
            #family=scat(link="identity")
          )
        y.fit <- exp(fit$fitted.values)
      }
      if(grepl('correlation',main)){
        fit <-
          mgcv::gam(
            formula = y.temp~s(x.temp,bs="cs"),
            family=betar(link="logit")
          )
        y.fit <- fit$fitted.values
      }
      j <- order(x.temp)
      lines(
        y = y.fit[j],
        x = x.temp[j],
        col = adjustcolor(col[[l]],0.5),
        lwd = 2
      )

    }

    legend(
      legend.where,
      legend =
        sapply(
          1:length(y),
          function(x){
            paste(
              gsub('RS_str_Bern','random sample - Bernoulli - structured',
              gsub('RS_unstr_Bern','random sample - Bernoulli - unstructured',
              gsub('RS_str_Multi','random sample -  Multinomial - structured',
              gsub('RS_unstr_Multi','random sample - Multinomial - unstructured',

              gsub('SS_str_Bern','online selection - Bernoulli - structured',
              gsub('SS_unstr_Bern','online selection - Bernoulli - unstructured',
              gsub('SS_str_Multi','online selection - Multinomial - structured',
              gsub('SS_unstr_Multi','online selection - Multinomial - unstructured',

              gsub('SS.correct_str_Bern','online selection - Bernoulli - structured - corrected',
              gsub('SS.correct_unstr_Bern','online selection - Bernoulli - unstructured - corrected',

              gsub('bias_theta_','',
                   gsub('rmse_theta_','',
                        gsub('pearson_theta_','',
                             gsub('cover_theta_','',
                                  gsub('bias_pi_','',
                                       gsub('rmse_pi_','',
                                            gsub('pearson_pi_','',
                                                 gsub('cover_pi_','',
                                                      names(y)[x]),
              ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ),

              ':',round(mean(y[[x]]-y[[1]]),3))
            } ),
      lty = 1,
      lwd = 2,
      col = sapply(1:length(col),function(x){col[[x]]}),
      pch = 1,
      cex = legend.size ,
      bty = 'n'
      )
  }

# # # plot comparison over bias, rmse, pearson correlation and coverage metrics
# theta
{
  pdf(file = paste('plots/sims.extended.comparison.theta.pdf',sep=''),width = 11,height = 10)

  par(mfrow = c(2,2))
  # # # bias
  y =
    list(
      bias_theta_RS_str_Bern = store.list[['bias_theta_RS_str_Bern']],
      bias_theta_RS_unstr_Bern = store.list[['bias_theta_RS_unstr_Bern']],
      bias_theta_RS_str_Multi = store.list[['bias_theta_RS_str_Multi']],
      bias_theta_RS_unstr_Multi = store.list[['bias_theta_RS_unstr_Multi']],

      bias_theta_SS_str_Bern = store.list[['bias_theta_SS_str_Bern']],
      bias_theta_SS_unstr_Bern = store.list[['bias_theta_SS_unstr_Bern']],
      bias_theta_SS_str_Multi = store.list[['bias_theta_SS_str_Multi']],
      bias_theta_SS_unstr_Multi = store.list[['bias_theta_SS_unstr_Multi']],

      bias_theta_SS.correct_str_Bern = store.list[['bias_theta_SS.correct_str_Bern']],
      bias_theta_SS.correct_unstr_Bern = store.list[['bias_theta_SS.correct_unstr_Bern']]
    )
  col =
    list(
      bias_theta_RS_str_Bern = c('dodgerblue1'),
      bias_theta_RS_unstr_Bern = c('blue4'),
      bias_theta_RS_str_Multi = c('aquamarine1'),
      bias_theta_RS_unstr_Multi = c('aquamarine4'),

      bias_theta_SS_str_Bern = c('orangered1'),
      bias_theta_SS_unstr_Bern = c('red4'),
      bias_theta_SS_str_Multi = c('lightsalmon1'),
      bias_theta_SS_unstr_Multi = c('lightsalmon4'),

      bias_theta_SS.correct_str_Bern = c('darkolivegreen'),
      bias_theta_SS.correct_unstr_Bern = c('darkolivegreen1')#,
      #bias_theta_SS.correct_str_Multi = c('springgreen1'),
      #bias_theta_SS.correct_unstr_Multi = c('springgreen4')
    )
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(absolute~bias~theta),
    lim.min = 0,
    lim.max = max(unlist(y)),
    legend.where = 'topleft',
    legend.size = 1
  )
  # # # rmse
  y =
    list(
      rmse_theta_RS_str_Bern = store.list[['rmse_theta_RS_str_Bern']],
      rmse_theta_RS_unstr_Bern = store.list[['rmse_theta_RS_unstr_Bern']],
      rmse_theta_RS_str_Multi = store.list[['rmse_theta_RS_str_Multi']],
      rmse_theta_RS_unstr_Multi = store.list[['rmse_theta_RS_unstr_Multi']],

      rmse_theta_SS_str_Bern = store.list[['rmse_theta_SS_str_Bern']],
      rmse_theta_SS_unstr_Bern = store.list[['rmse_theta_SS_unstr_Bern']],
      rmse_theta_SS_str_Multi = store.list[['rmse_theta_SS_str_Multi']],
      rmse_theta_SS_unstr_Multi = store.list[['rmse_theta_SS_unstr_Multi']],

      rmse_theta_SS.correct_str_Bern = store.list[['rmse_theta_SS.correct_str_Bern']],
      rmse_theta_SS.correct_unstr_Bern = store.list[['rmse_theta_SS.correct_unstr_Bern']]
    )
  names(col) = names(y)
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(rmse~theta),
    lim.min = min(unlist(y)),
    lim.max = max(unlist(y)),
    legend.where = 'topleft',
    legend.size = 1
  )
  # # # correlation
  y =
    list(
      pearson_theta_RS_str_Bern = store.list[['pearson_theta_RS_str_Bern']],
      pearson_theta_RS_unstr_Bern = store.list[['pearson_theta_RS_unstr_Bern']],
      pearson_theta_RS_str_Multi = store.list[['pearson_theta_RS_str_Multi']],
      pearson_theta_RS_unstr_Multi = store.list[['pearson_theta_RS_unstr_Multi']],

      pearson_theta_SS_str_Bern = store.list[['pearson_theta_SS_str_Bern']],
      pearson_theta_SS_unstr_Bern = store.list[['pearson_theta_SS_unstr_Bern']],
      pearson_theta_SS_str_Multi = store.list[['pearson_theta_SS_str_Multi']],
      pearson_theta_SS_unstr_Multi = store.list[['pearson_theta_SS_unstr_Multi']],

      pearson_theta_SS.correct_str_Bern = store.list[['pearson_theta_SS.correct_str_Bern']],
      pearson_theta_SS.correct_unstr_Bern = store.list[['pearson_theta_SS.correct_unstr_Bern']]
    )
  names(col) = names(y)
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(pearson~correlation~theta),
    lim.min = min(unlist(y)),
    lim.max = max(unlist(y)),
    legend.where = 'bottomright',
    legend.size = 1
  )
  # # # coverage
  y =
    list(
      cover_theta_RS_str_Bern = store.list[['cover_theta_RS_str_Bern']]- 0.9,
      cover_theta_RS_unstr_Bern = store.list[['cover_theta_RS_unstr_Bern']]- 0.9,
      cover_theta_RS_str_Multi = store.list[['cover_theta_RS_str_Multi']]- 0.9,
      cover_theta_RS_unstr_Multi = store.list[['cover_theta_RS_unstr_Multi']]- 0.9,

      cover_theta_SS_str_Bern = store.list[['cover_theta_SS_str_Bern']]- 0.9,
      cover_theta_SS_unstr_Bern = store.list[['cover_theta_SS_unstr_Bern']]- 0.9,
      cover_theta_SS_str_Multi = store.list[['cover_theta_SS_str_Multi']]- 0.9,
      cover_theta_SS_unstr_Multi = store.list[['cover_theta_SS_unstr_Multi']]- 0.9,

      cover_theta_SS.correct_str_Bern = store.list[['cover_theta_SS.correct_str_Bern']] - 0.9,
      cover_theta_SS.correct_unstr_Bern = store.list[['cover_theta_SS.correct_unstr_Bern']] -0.9
    )
  names(col) = names(y)
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(coverage~theta~':'~distance~from~90~'%'),
    lim.min = min(unlist(y)),
    lim.max = max(unlist(y)),
    legend.where = 'bottomright',
    legend.size = 1
  )
  dev.off()
}

# pi
{
  pdf(file = paste('plots/sims.extended.comparison.pi.pdf',sep=''),width = 11,height = 10)

  par(mfrow = c(2,2))
  # # # bias
  y =
    list(
      bias_pi_RS_str_Bern = store.list[['bias_pi_RS_str_Bern']],
      bias_pi_RS_unstr_Bern = store.list[['bias_pi_RS_unstr_Bern']],
      bias_pi_RS_str_Multi = store.list[['bias_pi_RS_str_Multi']],
      bias_pi_RS_unstr_Multi = store.list[['bias_pi_RS_unstr_Multi']],

      bias_pi_SS_str_Bern = store.list[['bias_pi_SS_str_Bern']],
      bias_pi_SS_unstr_Bern = store.list[['bias_pi_SS_unstr_Bern']],
      bias_pi_SS_str_Multi = store.list[['bias_pi_SS_str_Multi']],
      bias_pi_SS_unstr_Multi = store.list[['bias_pi_SS_unstr_Multi']],

      bias_pi_SS.correct_str_Bern = store.list[['bias_pi_SS.correct_str_Bern']],
      bias_pi_SS.correct_unstr_Bern = store.list[['bias_pi_SS.correct_unstr_Bern']]
    )
  col =
    list(
      bias_pi_RS_str_Bern = c('dodgerblue1'),
      bias_pi_RS_unstr_Bern = c('blue4'),
      bias_pi_RS_str_Multi = c('aquamarine1'),
      bias_pi_RS_unstr_Multi = c('aquamarine4'),

      bias_pi_SS_str_Bern = c('orangered1'),
      bias_pi_SS_unstr_Bern = c('red4'),
      bias_pi_SS_str_Multi = c('lightsalmon1'),
      bias_pi_SS_unstr_Multi = c('lightsalmon4'),

      bias_pi_SS.correct_str_Bern = c('darkolivegreen'),
      bias_pi_SS.correct_unstr_Bern = c('darkolivegreen1')#,
      #bias_pi_SS.correct_str_Multi = c('springgreen1'),
      #bias_pi_SS.correct_unstr_Multi = c('springgreen4')
    )
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(absolute~bias~pi),
    lim.min = 0,
    lim.max = max(unlist(y)),
    legend.where = 'topleft',
    legend.size = 1
  )
  # # # rmse
  y =
    list(
      rmse_pi_RS_str_Bern = store.list[['rmse_pi_RS_str_Bern']],
      rmse_pi_RS_unstr_Bern = store.list[['rmse_pi_RS_unstr_Bern']],
      rmse_pi_RS_str_Multi = store.list[['rmse_pi_RS_str_Multi']],
      rmse_pi_RS_unstr_Multi = store.list[['rmse_pi_RS_unstr_Multi']],

      rmse_pi_SS_str_Bern = store.list[['rmse_pi_SS_str_Bern']],
      rmse_pi_SS_unstr_Bern = store.list[['rmse_pi_SS_unstr_Bern']],
      rmse_pi_SS_str_Multi = store.list[['rmse_pi_SS_str_Multi']],
      rmse_pi_SS_unstr_Multi = store.list[['rmse_pi_SS_unstr_Multi']],

      rmse_pi_SS.correct_str_Bern = store.list[['rmse_pi_SS.correct_str_Bern']],
      rmse_pi_SS.correct_unstr_Bern = store.list[['rmse_pi_SS.correct_unstr_Bern']]
    )
  names(col) = names(y)
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(rmse~pi),
    lim.min = min(unlist(y)),
    lim.max = max(unlist(y)),
    legend.where = 'topleft',
    legend.size = 1
  )
  # # # correlation
  y =
    list(
      pearson_pi_RS_str_Bern = store.list[['pearson_pi_RS_str_Bern']],
      pearson_pi_RS_unstr_Bern = store.list[['pearson_pi_RS_unstr_Bern']],
      pearson_pi_RS_str_Multi = store.list[['pearson_pi_RS_str_Multi']],
      pearson_pi_RS_unstr_Multi = store.list[['pearson_pi_RS_unstr_Multi']],

      pearson_pi_SS_str_Bern = store.list[['pearson_pi_SS_str_Bern']],
      pearson_pi_SS_unstr_Bern = store.list[['pearson_pi_SS_unstr_Bern']],
      pearson_pi_SS_str_Multi = store.list[['pearson_pi_SS_str_Multi']],
      pearson_pi_SS_unstr_Multi = store.list[['pearson_pi_SS_unstr_Multi']],

      pearson_pi_SS.correct_str_Bern = store.list[['pearson_pi_SS.correct_str_Bern']],
      pearson_pi_SS.correct_unstr_Bern = store.list[['pearson_pi_SS.correct_unstr_Bern']]
    )
  names(col) = names(y)
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(pearson~correlation~pi),
    lim.min = min(unlist(y)),
    lim.max = max(unlist(y)),
    legend.where = 'bottomright',
    legend.size = 1
  )
  # # # coverage
  y =
    list(
      cover_pi_RS_str_Bern = store.list[['cover_pi_RS_str_Bern']]- 0.9,
      cover_pi_RS_unstr_Bern = store.list[['cover_pi_RS_unstr_Bern']]- 0.9,
      cover_pi_RS_str_Multi = store.list[['cover_pi_RS_str_Multi']]- 0.9,
      cover_pi_RS_unstr_Multi = store.list[['cover_pi_RS_unstr_Multi']]- 0.9,

      cover_pi_SS_str_Bern = store.list[['cover_pi_SS_str_Bern']]- 0.9,
      cover_pi_SS_unstr_Bern = store.list[['cover_pi_SS_unstr_Bern']]- 0.9,
      cover_pi_SS_str_Multi = store.list[['cover_pi_SS_str_Multi']]- 0.9,
      cover_pi_SS_unstr_Multi = store.list[['cover_pi_SS_unstr_Multi']]- 0.9,

      cover_pi_SS.correct_str_Bern = store.list[['cover_pi_SS.correct_str_Bern']] - 0.9,
      cover_pi_SS.correct_unstr_Bern = store.list[['cover_pi_SS.correct_unstr_Bern']] -0.9
    )
  names(col) = names(y)
  plot_compare(
    y = y,
    col = col,
    ylab = 'random sample - Bernoulli - structured',
    xlab = 'alternative sampling / modeling',
    main = expression(coverage~pi~':'~distance~from~90~'%'),
    lim.min = min(unlist(y)),
    lim.max = max(unlist(y)),
    legend.where = 'bottomright',
    legend.size = 1
  )
  dev.off()
}

# # 3 now understand determinants of properties
plot_performance <-
  function(y,col,x,ylab,xlab,main,
           lim.min.y,lim.max.y,lim.min.x,lim.max.x
  ){

    plot(
      x = unlist(x),
      y = unlist(x),
      ylab = ylab,
      xlab = xlab,
      main = main,
      pch = NA,
      xlim = c(lim.min.x,lim.max.x),
      ylim = c(lim.min.y,lim.max.y)
    )

    if(class(x)!='list'){
      for(l in names(y)){

        if(grepl('coverage|bias|correlation',main)){

          y.temp <- y[[l]]

          points(
            x =  x,
            y =  y.temp,
            pch = 1,
            col = adjustcolor(col[[l]],0.01),
            cex = 0.5
          )

          fit <-
            mgcv::gam(
              y.temp~s(x,bs="cs"),family=scat(link="identity")
            )
          x.new = seq(lim.min.x,lim.max.x,length.out = 100)
          pred <-
            predict(
              fit,
              newdata = data.table(x = x.new),
              se.fit = TRUE
            )
          sims <-
            sapply(
              1:length(pred$fit),
              function(i){
                rnorm(
                  n = 1000,
                  mean = pred$fit[i],
                  sd = pred$se.fit[i]
                )
              } )
          pred <- apply(sims ,2,function(x){quantile(x,c(0.05,0.5,0.95))})

          y.fit <- pred['50%',]
          y.fit.lo <-  pred['5%',]
          y.fit.hi <-  pred['95%',]
        }
        if(grepl('rmse',main)){

          y.temp <- abs(y[[l]])

          points(
            x =  x,
            y =  y.temp,
            pch = 1,
            col = adjustcolor(col[[l]],0.01),
            cex = 0.5
          )

          fit <-
            mgcv::gam(
              log(y.temp)~s(x,bs="cs"),family=scat(link="identity")
            )
          x.new = seq(lim.min.x,lim.max.x,length.out = 100)
          pred <-
            predict(
              fit,
              newdata = data.table(x = x.new),
              se.fit = TRUE
            )
          sims <-
            sapply(
              1:length(pred$fit),
              function(i){
                rnorm(
                  n = 1000,
                  mean = pred$fit[i],
                  sd = pred$se.fit[i]
                )
              } )
          pred <- apply(exp(sims) ,2,function(x){quantile(x,c(0.05,0.5,0.95))})

          y.fit <- pred['50%',]
          y.fit.lo <-  pred['5%',]
          y.fit.hi <-  pred['95%',]
        }
        #if(grepl('correlation',main)){
        #
        # y.temp <- y[[l]]
        #
        # points(
        #   x =  x,
        #   y =  y.temp,
        #   pch = 1,
        #   col = adjustcolor(col[[l]],0.01),
        #   cex = 0.5
        # )
        #
        #  fit <-
        #    mgcv::gam(
        #      formula = y.temp~s(x,bs="cs"),
        #      family=betar(link="logit")
        #    )
        #  x.new = seq(lim.min.x,lim.max.x,length.out = 100)
        #  pred <-
        #    predict(
        #      fit,
        #      newdata = data.table(x = x.new),
        #      se.fit = TRUE,type = 'response'
        #    )
        #  pars <- estBetaParams(mu =pred$fit ,var = pred$se.fit^2)
        #  sims <-
        #    sapply(
        #      1:length(pred$fit),
        #      function(i){
        #        rbeta(
        #         n = 1000,
        #          shape1 = pars$alpha[i],
        #          shape2 = pars$beta[i]
        #        )
        #      } )
        #  pred <- apply(sims ,2,function(x){quantile(x,c(0.05,0.5,0.95))})
        #y.fit <- pred['50%',]
        #  y.fit.lo <-  pred['5%',]
        #  y.fit.hi <-  pred['95%',]
        #}
        j <- order(x.new)

        polygon(
          c(x.new,rev(x.new)),
          c(y.fit.lo,rev(y.fit.hi)),
          col= adjustcolor(col[[l]],0.025),
          border=NA
        )
        lines(
          y = y.fit[j],
          x = x.new[j],
          col = col[[l]],
          lwd = 2
        )

      }
    }else{
      for(l in 1:length(y)){

        if(grepl('coverage|bias|correlation',main)){
          y.temp <- y[[l]]
          x.temp <- x[[l]]
          points(
            x =  x.temp,
            y =  y.temp,
            pch = 1,
            col = adjustcolor(col[[l]],0.01),
            cex = 0.5
          )

          fit <-
            mgcv::gam(
              y.temp~s(x.temp,bs="cs"),
              family=scat(link="identity")
            )
          x.new = seq(min(x.temp),max(x.temp),length.out = 100)
          pred <-
            predict(
              fit,
              newdata = data.table(x.temp = x.new),
              se.fit = TRUE
            )
          sims <-
            sapply(
              1:length(pred$fit),
              function(i){
                rnorm(
                  n = 1000,
                  mean = pred$fit[i],
                  sd = pred$se.fit[i]
                )
              } )
          pred <- apply(sims ,2,function(x){quantile(x,c(0.05,0.5,0.95))})

          y.fit <- pred['50%',]
          y.fit.lo <-  pred['5%',]
          y.fit.hi <-  pred['95%',]
        }

        if(grepl('rmse',main)){
          y.temp <- abs(y[[l]])
          x.temp <- x[[l]]
          points(
            x =  x.temp,
            y =  y.temp,
            pch = 1,
            col = adjustcolor(col[[l]],0.01),
            cex = 0.5
          )

          fit <-
            mgcv::gam(
              log(y.temp)~s(x.temp,bs="cs"),
              family=scat(link="identity")
            )
          x.new = seq(min(x.temp),max(x.temp),length.out = 100)
          pred <-
            predict(
              fit,
              newdata = data.table(x.temp = x.new),
              se.fit = TRUE
            )
          sims <-
            sapply(
              1:length(pred$fit),
              function(i){
                rnorm(
                  n = 1000,
                  mean = pred$fit[i],
                  sd = pred$se.fit[i]
                )
              } )
          pred <- apply(exp(sims) ,2,function(x){quantile(x,c(0.05,0.5,0.95))})

          y.fit <- pred['50%',]
          y.fit.lo <-  pred['5%',]
          y.fit.hi <-  pred['95%',]
        }
        #if(grepl('correlation',main)){
#
#          y.temp <- abs(y[[l]])
#          x.temp <- x[[l]]
#          points(
#            x =  x.temp,
#            y =  y.temp,
#            pch = 1,
#            col = adjustcolor(col[[l]],0.01),
#            cex = 0.5
#          )
#
#          fit <-
#            mgcv::gam(
#              formula = y.temp~s(x.temp,bs="cs"),
#              family=betar(link="logit")
#            )
#          x.new = seq(min(x.temp),max(x.temp),length.out = 100)
#          pred <-
#            predict(
#              fit,
#              newdata = data.table(x.temp = x.new),
#              se.fit = TRUE,type = 'response'
#            )
#          pars <- estBetaParams(mu =pred$fit ,var = pred$se.fit^2)
#          sims <-
#            sapply(
#              1:length(pred$fit),
#              function(i){
#                rbeta(
#                  n = 1000,
#                  shape1 = pars$alpha[i],
#                  shape2 = pars$beta[i]
#                )
#              } )
#          pred <- apply(sims ,2,function(x){quantile(x,c(0.05,0.5,0.95))})
#
#          y.fit <- pred['50%',]
#          y.fit.lo <-  pred['5%',]
#          y.fit.hi <-  pred['95%',]
#        }
        j <- order(x.new)

        polygon(
          c(x.new,rev(x.new)),
          c(y.fit.lo,rev(y.fit.hi)),
          col= adjustcolor(col[[l]],0.025),
          border=NA
        )
        lines(
          y = y.fit[j],
          x = x.new[j],
          col = col[[l]],
          lwd = 2
        )

      }
    }
  }
# rename prevalence
names(store.list)[names(store.list)=='prevalence.pop'] <- 'prevalence'

stimuli <- c('n',
             'penalty','var.penalty',
             'prevalence','sample prevalence bias')
# # # theta
for(x.name in stimuli){
  print(x.name)
  pdf(file = paste('plots/sims.extended.properties.theta.',make.names(x.name),'.pdf',sep=''),width = 15,height = 5)
  par(mfrow = c(1,4))
  if(x.name!='sample prevalence bias'){
    x = store.list[[x.name]]
  }else{
    x =
      list(

        pi_diff_RS_str_Bern = store.list[['prevalence.RS']] - store.list[['prevalence']],
        pi_diff_RS_unstr_Bern = store.list[['prevalence.RS']] - store.list[['prevalence']],
        pi_diff_RS_str_Multi = store.list[['prevalence.RS']] - store.list[['prevalence']],
        pi_diff_RS_unstr_Multi = store.list[['prevalence.RS']] - store.list[['prevalence']],

        pi_diff_SS_str_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS_unstr_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS_str_Multi = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS_unstr_Multi = store.list[['prevalence.SS']] - store.list[['prevalence']],

        pi_diff_SS.correct_str_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS.correct_unstr_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']]
      )
  }

  # # # bias
  y =
    list(
      bias_theta_RS_str_Bern = store.list[['bias_theta_RS_str_Bern']],
      bias_theta_RS_unstr_Bern = store.list[['bias_theta_RS_unstr_Bern']],
      bias_theta_RS_str_Multi = store.list[['bias_theta_RS_str_Multi']],
      bias_theta_RS_unstr_Multi = store.list[['bias_theta_RS_unstr_Multi']],

      bias_theta_SS_str_Bern = store.list[['bias_theta_SS_str_Bern']],
      bias_theta_SS_unstr_Bern = store.list[['bias_theta_SS_unstr_Bern']],
      bias_theta_SS_str_Multi = store.list[['bias_theta_SS_str_Multi']],
      bias_theta_SS_unstr_Multi = store.list[['bias_theta_SS_unstr_Multi']],

      bias_theta_SS.correct_str_Bern = store.list[['bias_theta_SS.correct_str_Bern']],
      bias_theta_SS.correct_unstr_Bern = store.list[['bias_theta_SS.correct_unstr_Bern']]
    )
  col =
    list(
      bias_theta_RS_str_Bern = c('dodgerblue1'),
      bias_theta_RS_unstr_Bern = c('blue4'),
      bias_theta_RS_str_Multi = c('aquamarine1'),
      bias_theta_RS_unstr_Multi = c('aquamarine4'),

      bias_theta_SS_str_Bern = c('orangered1'),
      bias_theta_SS_unstr_Bern = c('red4'),
      bias_theta_SS_str_Multi = c('lightsalmon1'),
      bias_theta_SS_unstr_Multi = c('lightsalmon4'),

      bias_theta_SS.correct_str_Bern = c('darkolivegreen'),
      bias_theta_SS.correct_unstr_Bern = c('darkolivegreen1')#,
      #bias_theta_SS.correct_str_Multi = c('springgreen1'),
      #bias_theta_SS.correct_unstr_Multi = c('springgreen4')
    )
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = 'B',
    xlab = x.name,
    main = expression(bias~theta),
    lim.min.y = min(unlist(y)),
    lim.max.y = max(unlist(y)),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  # # # rmse
  y =
    list(
      rmse_theta_RS_str_Bern = store.list[['rmse_theta_RS_str_Bern']],
      rmse_theta_RS_unstr_Bern = store.list[['rmse_theta_RS_unstr_Bern']],
      rmse_theta_RS_str_Multi = store.list[['rmse_theta_RS_str_Multi']],
      rmse_theta_RS_unstr_Multi = store.list[['rmse_theta_RS_unstr_Multi']],

      rmse_theta_SS_str_Bern = store.list[['rmse_theta_SS_str_Bern']],
      rmse_theta_SS_unstr_Bern = store.list[['rmse_theta_SS_unstr_Bern']],
      rmse_theta_SS_str_Multi = store.list[['rmse_theta_SS_str_Multi']],
      rmse_theta_SS_unstr_Multi = store.list[['rmse_theta_SS_unstr_Multi']],

      rmse_theta_SS.correct_str_Bern = store.list[['rmse_theta_SS.correct_str_Bern']],
      rmse_theta_SS.correct_unstr_Bern = store.list[['rmse_theta_SS.correct_unstr_Bern']]
    )
  names(col) <- names(y)
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = 'RMSE',
    xlab = x.name,
    main = expression(rmse~theta),
    lim.min.y = 0,
    lim.max.y = max(abs(unlist(y))),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  # # # pearson correlation
  y =
    list(
      pearson_theta_RS_str_Bern = store.list[['pearson_theta_RS_str_Bern']],
      pearson_theta_RS_unstr_Bern = store.list[['pearson_theta_RS_unstr_Bern']],
      pearson_theta_RS_str_Multi = store.list[['pearson_theta_RS_str_Multi']],
      pearson_theta_RS_unstr_Multi = store.list[['pearson_theta_RS_unstr_Multi']],

      pearson_theta_SS_str_Bern = store.list[['pearson_theta_SS_str_Bern']],
      pearson_theta_SS_unstr_Bern = store.list[['pearson_theta_SS_unstr_Bern']],
      pearson_theta_SS_str_Multi = store.list[['pearson_theta_SS_str_Multi']],
      pearson_theta_SS_unstr_Multi = store.list[['pearson_theta_SS_unstr_Multi']],

      pearson_theta_SS.correct_str_Bern = store.list[['pearson_theta_SS.correct_str_Bern']],
      pearson_theta_SS.correct_unstr_Bern = store.list[['pearson_theta_SS.correct_unstr_Bern']]
    )
  names(col) <- names(y)
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = expression(rho),
    xlab = x.name,
    main = expression(pearson~correlation~theta),
    lim.min.y = min(abs(unlist(y))),
    lim.max.y = max(abs(unlist(y))),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  # # # coverage
  y =
    list(
      cover_theta_RS_str_Bern = store.list[['cover_theta_RS_str_Bern']]- 0.9,
      cover_theta_RS_unstr_Bern = store.list[['cover_theta_RS_unstr_Bern']]- 0.9,
      cover_theta_RS_str_Multi = store.list[['cover_theta_RS_str_Multi']]- 0.9,
      cover_theta_RS_unstr_Multi = store.list[['cover_theta_RS_unstr_Multi']]- 0.9,

      cover_theta_SS_str_Bern = store.list[['cover_theta_SS_str_Bern']]- 0.9,
      cover_theta_SS_unstr_Bern = store.list[['cover_theta_SS_unstr_Bern']]- 0.9,
      cover_theta_SS_str_Multi = store.list[['cover_theta_SS_str_Multi']]- 0.9,
      cover_theta_SS_unstr_Multi = store.list[['cover_theta_SS_unstr_Multi']]- 0.9,

      cover_theta_SS.correct_str_Bern = store.list[['cover_theta_SS.correct_str_Bern']] - 0.9,
      cover_theta_SS.correct_unstr_Bern = store.list[['cover_theta_SS.correct_unstr_Bern']] -0.9
    )
  names(col) <- names(y)
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = expression(Gamma),
    xlab = x.name,
    main = expression(coverage~theta),
    lim.min.y = min(unlist(y)),
    lim.max.y = max(unlist(y)),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  dev.off()
}
# # # pi
for(x.name in stimuli){
  print(x.name)
  
  pdf(file = paste('plots/sims.extended.properties.pi.',make.names(x.name),'.pdf',sep=''),width = 15,height = 5)
  par(mfrow = c(1,4))
  if(x.name!='sample prevalence bias'){
    x = store.list[[x.name]]
  }else{
    x =
      list(

        pi_diff_RS_str_Bern = store.list[['prevalence.RS']] - store.list[['prevalence']],
        pi_diff_RS_unstr_Bern = store.list[['prevalence.RS']] - store.list[['prevalence']],
        pi_diff_RS_str_Multi = store.list[['prevalence.RS']] - store.list[['prevalence']],
        pi_diff_RS_unstr_Multi = store.list[['prevalence.RS']] - store.list[['prevalence']],

        pi_diff_SS_str_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS_unstr_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS_str_Multi = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS_unstr_Multi = store.list[['prevalence.SS']] - store.list[['prevalence']],

        pi_diff_SS.correct_str_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']],
        pi_diff_SS.correct_unstr_Bern = store.list[['prevalence.SS']] - store.list[['prevalence']]
      )
  }

  # # # absolute bias
  y =
    list(
      bias_pi_RS_str_Bern = store.list[['bias_pi_RS_str_Bern']],
      bias_pi_RS_unstr_Bern = store.list[['bias_pi_RS_unstr_Bern']],
      bias_pi_RS_str_Multi = store.list[['bias_pi_RS_str_Multi']],
      bias_pi_RS_unstr_Multi = store.list[['bias_pi_RS_unstr_Multi']],

      bias_pi_SS_str_Bern = store.list[['bias_pi_SS_str_Bern']],
      bias_pi_SS_unstr_Bern = store.list[['bias_pi_SS_unstr_Bern']],
      bias_pi_SS_str_Multi = store.list[['bias_pi_SS_str_Multi']],
      bias_pi_SS_unstr_Multi = store.list[['bias_pi_SS_unstr_Multi']],

      bias_pi_SS.correct_str_Bern = store.list[['bias_pi_SS.correct_str_Bern']],
      bias_pi_SS.correct_unstr_Bern = store.list[['bias_pi_SS.correct_unstr_Bern']]
    )
  col =
    list(
      bias_pi_RS_str_Bern = c('dodgerblue1'),
      bias_pi_RS_unstr_Bern = c('blue4'),
      bias_pi_RS_str_Multi = c('aquamarine1'),
      bias_pi_RS_unstr_Multi = c('aquamarine4'),

      bias_pi_SS_str_Bern = c('orangered1'),
      bias_pi_SS_unstr_Bern = c('red4'),
      bias_pi_SS_str_Multi = c('lightsalmon1'),
      bias_pi_SS_unstr_Multi = c('lightsalmon4'),

      bias_pi_SS.correct_str_Bern = c('darkolivegreen'),
      bias_pi_SS.correct_unstr_Bern = c('darkolivegreen1')#,
      #bias_pi_SS.correct_str_Multi = c('springgreen1'),
      #bias_pi_SS.correct_unstr_Multi = c('springgreen4')
    )
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = 'B',
    xlab = x.name,
    main = expression(bias~pi),
    lim.min.y = min(unlist(y)),
    lim.max.y = max(unlist(y)),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  # # # rmse
  y =
    list(
      rmse_pi_RS_str_Bern = store.list[['rmse_pi_RS_str_Bern']],
      rmse_pi_RS_unstr_Bern = store.list[['rmse_pi_RS_unstr_Bern']],
      rmse_pi_RS_str_Multi = store.list[['rmse_pi_RS_str_Multi']],
      rmse_pi_RS_unstr_Multi = store.list[['rmse_pi_RS_unstr_Multi']],

      rmse_pi_SS_str_Bern = store.list[['rmse_pi_SS_str_Bern']],
      rmse_pi_SS_unstr_Bern = store.list[['rmse_pi_SS_unstr_Bern']],
      rmse_pi_SS_str_Multi = store.list[['rmse_pi_SS_str_Multi']],
      rmse_pi_SS_unstr_Multi = store.list[['rmse_pi_SS_unstr_Multi']],

      rmse_pi_SS.correct_str_Bern = store.list[['rmse_pi_SS.correct_str_Bern']],
      rmse_pi_SS.correct_unstr_Bern = store.list[['rmse_pi_SS.correct_unstr_Bern']]
    )
  names(col) <- names(y)
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = 'RMSE',
    xlab = x.name,
    main = expression(rmse~pi),
    lim.min.y = 0,
    lim.max.y = max(abs(unlist(y))),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  # # # pearson correlation
  y =
    list(
      pearson_pi_RS_str_Bern = store.list[['pearson_pi_RS_str_Bern']],
      pearson_pi_RS_unstr_Bern = store.list[['pearson_pi_RS_unstr_Bern']],
      pearson_pi_RS_str_Multi = store.list[['pearson_pi_RS_str_Multi']],
      pearson_pi_RS_unstr_Multi = store.list[['pearson_pi_RS_unstr_Multi']],

      pearson_pi_SS_str_Bern = store.list[['pearson_pi_SS_str_Bern']],
      pearson_pi_SS_unstr_Bern = store.list[['pearson_pi_SS_unstr_Bern']],
      pearson_pi_SS_str_Multi = store.list[['pearson_pi_SS_str_Multi']],
      pearson_pi_SS_unstr_Multi = store.list[['pearson_pi_SS_unstr_Multi']],

      pearson_pi_SS.correct_str_Bern = store.list[['pearson_pi_SS.correct_str_Bern']],
      pearson_pi_SS.correct_unstr_Bern = store.list[['pearson_pi_SS.correct_unstr_Bern']]
    )
  names(col) <- names(y)
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = expression(rho),
    xlab = x.name,
    main = expression(pearson~correlation~pi),
    lim.min.y = min(abs(unlist(y))),
    lim.max.y = max(abs(unlist(y))),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  # # # coverage
  y =
    list(
      cover_pi_RS_str_Bern = store.list[['cover_pi_RS_str_Bern']]- 0.9,
      cover_pi_RS_unstr_Bern = store.list[['cover_pi_RS_unstr_Bern']]- 0.9,
      cover_pi_RS_str_Multi = store.list[['cover_pi_RS_str_Multi']]- 0.9,
      cover_pi_RS_unstr_Multi = store.list[['cover_pi_RS_unstr_Multi']]- 0.9,

      cover_pi_SS_str_Bern = store.list[['cover_pi_SS_str_Bern']]- 0.9,
      cover_pi_SS_unstr_Bern = store.list[['cover_pi_SS_unstr_Bern']]- 0.9,
      cover_pi_SS_str_Multi = store.list[['cover_pi_SS_str_Multi']]- 0.9,
      cover_pi_SS_unstr_Multi = store.list[['cover_pi_SS_unstr_Multi']]- 0.9,

      cover_pi_SS.correct_str_Bern = store.list[['cover_pi_SS.correct_str_Bern']] - 0.9,
      cover_pi_SS.correct_unstr_Bern = store.list[['cover_pi_SS.correct_unstr_Bern']] -0.9
    )
  names(col) <- names(y)
  plot_performance(
    y = y,
    col = col,
    x = x,
    ylab = expression(Gamma),
    xlab = x.name,
    main = expression(coverage~pi),
    lim.min.y = min(unlist(y)),
    lim.max.y = max(unlist(y)),
    lim.min.x = min(unlist(x)),
    lim.max.x = max(unlist(x))
  )
  dev.off()
}

