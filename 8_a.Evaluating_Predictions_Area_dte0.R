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

# load useful package
library(data.table)

# functiona to calculatre and plot pred v. obs
performance_summary <-
  function(pred_sims){

    summary_list <- data.table()
        # loop through choices and training data
    for(c in levels(pred_sims$choice)){
      for(t in levels(pred_sims$training_data)){

        # define simulations
        sims <- pred_sims[pred_sims$choice==c & pred_sims$training_data==t]

        obs <- unique(sims[,c('state_simple','observed')])$observed
        point <- sims[,lapply(.SD,function(x){quantile(x,0.5)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted
        upper <- sims[,lapply(.SD,function(x){quantile(x,0.95)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted

        if(! c %in% c('raw_ANES','last_election_result')){
          lower <- sims[,lapply(.SD,function(x){quantile(x,0.05)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted
          upper <- sims[,lapply(.SD,function(x){quantile(x,0.95)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted
        }

        if(! c %in% c('raw_ANES','last_election_result')){

        summary_list <-
          rbindlist(
            list(
              summary_list,
              data.table(
                choice = c,
                training_data = t,
                # calculate point estimate metrics
                bias = mean(point-obs),
                rmse = sqrt(mean((point-obs)^2)),
                cor = cor(x = point, y = obs),
              cover = mean(obs>lower & obs<upper)
              )
            )
          )
        }else{
          summary_list <-
            rbindlist(
              list(
                summary_list,
                data.table(
                  choice = c,
                  training_data = t,
                  # calculate point estimate metrics
                  bias = mean(point-obs),
                  rmse = sqrt(mean((point-obs)^2)),
                  cor = cor(x = point, y = obs),
                  cover = NA
                )
              )
            )
        }

        }
     }
            return(summary_list)
  }

plot_preds_v_obs <-
  function(pred_sims){

    # set plot space
    par(
      mfrow = c(nlevels(pred_sims$choice),nlevels(pred_sims$training_data)),
      mar = c(1,1,1,1),
      oma = c(3,3,1,0)
    )

    # loop through choices and training data
    for(c in levels(pred_sims$choice)){
      for(t in levels(pred_sims$training_data)){

        # define simulations
        sims <- pred_sims[pred_sims$choice==c & pred_sims$training_data==t]

        obs <- unique(sims[,c('state_simple','observed')])$observed
        point <- sims[,lapply(.SD,function(x){quantile(x,0.5)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted

        if(! c %in% c('raw_ANES','last_election_result')){
          lower <- sims[,lapply(.SD,function(x){quantile(x,0.05)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted
          upper <- sims[,lapply(.SD,function(x){quantile(x,0.95)}),by = c('state_simple'),.SDcols = c('predicted')]$predicted
        }
        ylim = xlim = c(0,1)

        if(c=='R.D_margin'){
          ylim = xlim = c(-1,1)
        }

        #main = paste(c,'\n',t)#paste('choice:',c,'& training:',t)

        #if(c=='R.D_margin'){
        #  main = paste(gsub('\\.','-',gsub('_',' ',c)),'\n',t)#paste(gsub('\\.','-',gsub('_',' ',c)),'& training:',t)
        #}

        if(c=='D'){
          main = substitute(paste(bold(t)))#paste('choice:',c,'& training:',t)
        }

        if(t=='gpt_10tweets'){
          ylab = substitute(paste(bold(paste('choice: ',c))))#paste('choice:',c,'& training:',t)
        }
        if(t=='gpt_10tweets' & c =='R.D_margin'){
          ylab = substitute(paste(bold(c)))#paste('choice:',c,'& training:',t)
        }

        # plot a scatterplot
        plot(
          x = point, y = obs,
          ylim = ylim,xlim = xlim,
          pch = 16,
          col = NA,
          main = '',
          xaxt = 'n',
          yaxt = 'n',
          ylab = '',#'observed',
          xlab = '',#,'predicted'
          xpd = NA
        )

      if(c=='D'){

      axis(side = 3, at = 0.5,labels = main,cex.axis = 1.25,tick = FALSE,line =  -0.5)

      }


      if(c=='R.D_margin'){
          if(t=='gpt_10tweets'){
        axis(side = 1, at = c(-1,0,1),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 4, at = c(-1,0,1),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 2, at = 0,labels = ylab,cex.axis = 1.25,tick = FALSE,line =  -0.5)
          }else{
        axis(side = 1, at = c(-1,0,1),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 4, at = c(-1,0,1),cex.axis = 0.85,tick = FALSE,line =  -1)
          }
      }else{
          if(t=='gpt_10tweets'){
        axis(side = 1, at = c(0,0.5,1),cex.axis = 0.85,tick = FALSE,line = -1)
        axis(side = 4, at = c(0,0.5,1),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 2, at = c(0.5),labels = ylab,cex.axis = 1.25,tick = FALSE,line =  -0.5)

          }else{
        axis(side = 1, at = c(0,0.5,1),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 4, at = c(0,0.5,1),cex.axis = 0.85,tick = FALSE,line =  -1)
          }
      }

        if(! t %in% c('raw_ANES','last_election_result','uniform_swing')){
        # plot interval
        segments(
          x0 = lower,
          y0 = obs ,
          x1 = upper ,
          y1 = obs,
          col =
            ifelse(c=='D','skyblue',
                   ifelse(c=='R','lightcoral',
                          ifelse(c=='L','yellow',
                                 ifelse(c=='G','green',
                                        ifelse(c=='other','tan',
                                               ifelse(c=='R.D_margin','violet',
                                                      'grey'
                                               )
                                        )
                                 )
                          )
                   )
            )
          )
        }

        points(
        x = point, y = obs,
        pch = 16,
        col = ifelse(c=='D','blue',
                     ifelse(c=='R','red',
                            ifelse(c=='L','orange',
                                   ifelse(c=='G','darkgreen',
                                          ifelse(c=='other','brown',
                                                 ifelse(c=='R.D_margin','purple',
                                                        'black'
                                                 )
                                          )
                                   )
                            )
                     )
        )
        )

        # reference optimal fit
        abline(0,1,lty = 2,col = adjustcolor('black',0.5))

        # calculate point estimate metrics
        bias <- mean(point-obs)
        rmse <- sqrt(mean((point-obs)^2))
        cor <- cor(x = point, y = obs)

        if(! t %in% c('raw_ANES','last_election_result','uniform_swing')){
          cover <- mean(obs>lower & obs<upper)
        }

        if(! t %in% c('raw_ANES','last_election_result','uniform_swing')){
        # legend
        legend(
          'bottomright',
          legend =
            c(
              paste('bias',round(bias,2)),
              paste('rmse',round(rmse,2)),
              paste('pearson:',round(cor,2)),
              paste('coverage:',round(cover,2))
            ),
          bty = 'n',
          cex = 1.15
        )
        }else{
          # legend
          legend(
            'bottomright',
            legend =
              c(
                paste('bias',round(bias,2)),
                paste('rmse',round(rmse,2)),
                paste('pearson:',round(cor,2))
              ),
            bty = 'n',
          cex = 1.15
          )
        }

      } }

mtext(expression(observed),side = 2, outer = TRUE,line = 1.5)
mtext(expression(predicted),side = 1, outer = TRUE,line = 1.5)


  }

plot_delta_preds_v_delta_obs <-
  function(pred_sims){

    # set plot space
    par(
      mfrow = c(nlevels(pred_sims$choice),(nlevels(pred_sims$training_data)-2)),
      mar = c(1,1,1,1),
      oma = c(3,3,1,0)
    )

    # loop through choices and training data
    for(c in levels(pred_sims$choice)){
      for(t in levels(pred_sims$training_data)){
        if(t %in% c('uniform_swing','raw_ANES')){next}

        # define simulations
        sims <- pred_sims[pred_sims$choice==c & pred_sims$training_data==t]

        obs <- unique(sims[,c('state_simple','delta_observed')])$delta_observed
        point <- sims[,lapply(.SD,function(x){quantile(x,0.5)}),by = c('state_simple'),.SDcols = c('delta_predicted')]$delta_predicted

        if(! c %in% c('raw_ANES','last_election_result')){
          lower <- sims[,lapply(.SD,function(x){quantile(x,0.05)}),by = c('state_simple'),.SDcols = c('delta_predicted')]$delta_predicted
          upper <- sims[,lapply(.SD,function(x){quantile(x,0.95)}),by = c('state_simple'),.SDcols = c('delta_predicted')]$delta_predicted
        }
        ylim = xlim = c(-0.5,0.5)

        if(c=='R.D_margin'){
          ylim = xlim = c(-0.5,0.5)
        }

        #main = paste(c,'\n',t)#paste('choice:',c,'& training:',t)

        #if(c=='R.D_margin'){
        #  main = paste(gsub('\\.','-',gsub('_',' ',c)),'\n',t)#paste(gsub('\\.','-',gsub('_',' ',c)),'& training:',t)
        #}

        if(c=='D'){
          main = substitute(paste(bold(t)))#paste('choice:',c,'& training:',t)
        }

        if(t=='gpt_10tweets'){
          ylab = substitute(paste(bold(paste('choice: ',c))))#paste('choice:',c,'& training:',t)
        }
        if(t=='gpt_10tweets' & c =='R.D_margin'){
          ylab = substitute(paste(bold(c)))#paste('choice:',c,'& training:',t)
        }

        # plot a scatterplot
        plot(
          x = point, y = obs,
          ylim = ylim,xlim = xlim,
          pch = 16,
          col = NA,
          main = '',
          xaxt = 'n',
          yaxt = 'n',
          ylab = '',#'observed',
          xlab = '',#,'predicted'
          xpd = NA
        )

      if(c=='D'){

      axis(side = 3, at = 0,labels = main,cex.axis = 1.25,tick = FALSE,line =  -0.5)

      }

        if(t=='gpt_10tweets'){
        axis(side = 1, at = c(-0.5,0,0.5),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 4, at = c(-0.5,0,0.5),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 2, at = 0,labels = ylab,cex.axis = 1.25,tick = FALSE,line =  -0.5)
          }else{
        axis(side = 1, at = c(-0.5,0,0.5),cex.axis = 0.85,tick = FALSE,line =  -1)
        axis(side = 4, at = c(-0.5,0,0.5),cex.axis = 0.85,tick = FALSE,line =  -1)
          }


        if(! t %in% c('raw_ANES','last_election_result','uniform_swing')){
        # plot interval
        segments(
          x0 = lower,
          y0 = obs ,
          x1 = upper ,
          y1 = obs,
          col =
            ifelse(c=='D','skyblue',
                   ifelse(c=='R','lightcoral',
                          ifelse(c=='L','yellow',
                                 ifelse(c=='G','green',
                                        ifelse(c=='other','tan',
                                               ifelse(c=='R.D_margin','violet',
                                                      'grey'
                                               )
                                        )
                                 )
                          )
                   )
            )
          )
        }

        points(
        x = point, y = obs,
        pch = 16,
        col = ifelse(c=='D','blue',
                     ifelse(c=='R','red',
                            ifelse(c=='L','orange',
                                   ifelse(c=='G','darkgreen',
                                          ifelse(c=='other','brown',
                                                 ifelse(c=='R.D_margin','purple',
                                                        'black'
                                                 )
                                          )
                                   )
                            )
                     )
        )
        )

        # reference optimal fit
        abline(0,1,lty = 2,col = adjustcolor('black',0.5))

        # calculate point estimate metrics
        bias <- mean(point-obs)
        rmse <- sqrt(mean((point-obs)^2))
        cor <- cor(x = point, y = obs)

        if(! t %in% c('raw_ANES','last_election_result','uniform_swing')){
          cover <- mean(obs>lower & obs<upper)
        }

        if(! t %in% c('raw_ANES','last_election_result','uniform_swing')){
        # legend
        legend(
          'bottomright',
          legend =
            c(
              paste('bias',round(bias,2)),
              paste('rmse',round(rmse,2)),
              paste('pearson:',round(cor,2)),
              paste('coverage:',round(cover,2))
            ),
          bty = 'n',
          cex = 1
        )
        }else{
          # legend
          legend(
            'bottomright',
            legend =
              c(
                paste('bias',round(bias,2)),
                paste('rmse',round(rmse,2)),
                paste('pearson:',round(cor,2))
              ),
            bty = 'n',
          cex = 1
          )
        }

      } }

mtext(expression(observed~Delta),side = 2, outer = TRUE,line = 1.5)
mtext(expression(predicted~Delta),side = 1, outer = TRUE,line = 1.5)

  }


# metrics posterior summary
load(file = 'data_generated/observed_vote.RData')
obs_dt$R.D_margin <- obs_dt$R - obs_dt$D
obs_dt$dte = 0
obs_dt <- melt(obs_dt[,!'registered'],id.vars = c('state_simple','dte'),variable.name = 'choice',value.name = 'observed')

load(file = 'data_generated/area_preds.RData')
area.pred_list$R.D_margin <- area.pred_list$R-area.pred_list$D
area.pred_list <- melt(area.pred_list,id.vars = c('state_simple','dte','sim_id','training_data'),variable.name = 'choice',value.name = 'predicted')
area.pred_list$training_data <- as.factor(area.pred_list$training_data)
area.pred_list$choice <- as.factor(area.pred_list$choice)

area.pred_list <-
  merge(
    obs_dt,
    area.pred_list,
    all.x =TRUE,
    by = c('state_simple','choice','dte')
    )

pred_sims <- area.pred_list

# make delta dataset
last_el <- pred_sims[training_data=='last_election']
last_el <- last_el[,c('state_simple','dte','choice','predicted')]
names(last_el)[names(last_el)=='predicted'] = 'lag_observed'

obs_dt_delta <- merge(obs_dt ,last_el,by = c('state_simple','dte','choice'),all=T)
obs_dt_delta$delta_observed <- obs_dt_delta$observed - obs_dt_delta $lag_observed

pred_sims_delta <- merge(pred_sims,last_el,by = c('state_simple','dte','choice'),all=T)
pred_sims_delta$delta_predicted <- pred_sims_delta$predicted - pred_sims_delta $lag_observed
pred_sims_delta$delta_observed <- pred_sims_delta$observed - pred_sims_delta$lag_observed

# drop last election from reference fits
area.pred_list$training_data <- as.character(unlist(area.pred_list$training_data))
area.pred_list <- area.pred_list[training_data!='last_election']
area.pred_list$training_data <- as.factor(area.pred_list$training_data)

levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="anes")
  ] = '5.ANES'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="anes_abc.wapo")
  ] = '6.ANES_abc.wapo'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="turks")
  ] = '4.turks'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="twitter_human")
  ] = '3.human_10tweets'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="twitter_gpt_10tweets")
  ] ='1.gpt_10tweets'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="twitter_gpt_5tweets")
  ] = '2.gpt_5tweets'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="uniform_swing")
  ] = '8.uniform_swing'
levels(area.pred_list$training_data)[
  which(levels(area.pred_list$training_data)=="raw_ANES")
  ] =  '7.raw_ANES'
area.pred_list$training_data <- as.factor(as.character(unlist(area.pred_list$training_data)))

levels(area.pred_list$training_data) <- substr(levels(area.pred_list$training_data),3,nchar(levels(area.pred_list$training_data)))

# drop last election from reference fits
pred_sims_delta$training_data <- as.character(unlist(pred_sims_delta$training_data))
pred_sims_delta <- pred_sims_delta[training_data!='last_election']
pred_sims_delta$training_data <- as.factor(pred_sims_delta$training_data)

levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="anes")
  ] = '5.ANES'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="anes_abc.wapo")
  ] = '6.ANES_abc.wapo'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="turks")
  ] = '4.turks'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="twitter_human")
  ] = '3.human_10tweets'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="twitter_gpt_10tweets")
  ] ='1.gpt_10tweets'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="twitter_gpt_5tweets")
  ] = '2.gpt_5tweets'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="uniform_swing")
  ] = '8.uniform_swing'
levels(pred_sims_delta$training_data)[
  which(levels(pred_sims_delta$training_data)=="raw_ANES")
  ] =  '7.raw_ANES'
pred_sims_delta$training_data <- as.factor(as.character(unlist(pred_sims_delta$training_data)))

levels(pred_sims_delta$training_data) <- substr(levels(pred_sims_delta$training_data),3,nchar(levels(pred_sims_delta$training_data)))

# plot predictions against observations
pdf(file = 'plots/pred_v_obs.pdf',width = 15,height = 10)
plot_preds_v_obs(area.pred_list)
dev.off()

# plot predictions against observations
pdf(file = 'plots/delta_pred_v_obs.pdf',width = 12.5,height = 10)
  plot_delta_preds_v_delta_obs(pred_sims_delta)
dev.off()

# save performance summary
performance <- performance_summary(area.pred_list)
fwrite(
  performance,
  file = 'data_generated/performance.csv'
  )