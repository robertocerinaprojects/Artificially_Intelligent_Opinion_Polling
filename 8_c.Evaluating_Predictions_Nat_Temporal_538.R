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

# metrics posterior summary
load(file = 'data_generated/nat_preds_f38_comparison.RData')

# use polling average for this - different from forecast, that's a valid 
# comparison only on election day ! 
obs <- 
  fread(file = 'data_auxiliary/f38_Model_Data_and_predictions/presidential_poll_averages_2020.csv')

# focus on last 30 days of the campaign
obs$modeldate <- as.Date(obs$modeldate,'%m/%d/%Y')
obs$dte <- difftime(as.Date("11/03/2020",'%m/%d/%Y'),obs$modeldate,units = 'days')
obs <- obs[dte %in% 0:30]
# focus on national 
obs  <- obs [state == 'National',!c('state','cycle')]
# standardise naming 
obs$candidate_name <- as.factor(obs$candidate_name)
levels(obs$candidate_name)[levels(obs$candidate_name)=='Donald Trump'] = 'R'
levels(obs$candidate_name)[levels(obs$candidate_name)=='Joseph R. Biden Jr.'] = 'D'
obs <- obs[candidate_name %in% c('R','D')]

# scale same as MrP
obs$pct_estimate <- obs$pct_estimate/100

# make wide and get R-D margin
obs <- 
  reshape(
    obs[,!c('modeldate','pct_trend_adjusted')],
    idvar = c('dte'),timevar = 'candidate_name',
    direction = 'wide'
  )
obs$R.D_margin <- obs$pct_estimate.R-obs$pct_estimate.D
obs$dte = as.numeric(as.character(unlist(obs$dte)))
# melt 
obs <- melt(obs,id.vars = c('dte'),variable.name = 'choice',value.name = 'observed_538')
obs$choice <- gsub('pct_estimate.','',obs$choice)

# load predictions 
rcp <- fread(file = 'data_auxiliary/RealClearPolitics_PollingAvg_2020_BidenTrump.csv')[1:150]
rcp$dte <- difftime(
  as.Date('03/11/2020',"%d/%m/%Y"),
  as.Date(paste('2020',trimws(sub('.+-(.+)', '\\1', rcp$Date),'both'),sep="/"),"%Y/%m/%d")
                )
rcp$dte <- as.numeric(as.character(unlist(rcp$dte)))
rcp$R = rcp$`Trump (R)`/100
rcp$D = rcp$`Biden (D)`/100
rcp$R.D_margin = rcp$R-rcp$D

rcp <- rcp[,c('dte','R','D','R.D_margin')]
# drop result and last published average
rcp <- rcp[-c(1,2)]
# impute time periods
rcp <- merge(rcp,data.table(dte = 0:30,R = NA,D = NA,R.D_margin = NA),all=TRUE)
rcp <- rcp[order(rcp$dte)]
# take daily average 
rcp <- rcp[,
           lapply(.SD,function(x){
             mean(x,na.rm=TRUE)
             }),
           by = c('dte'),
           .SDcols = c("R", "D","R.D_margin")]
# take weekly rolling average 
library(zoo)
# take 7 day rolling average 
rcp[, c("R", "D",'R.D_margin') := 
      lapply(.SD, rollmean, k = 7, fill = NA,na.rm=TRUE,align = 'left'), 
    .SDcols = c("R", "D","R.D_margin")]
# drop everything apart from last 30 days
rcp <- rcp[dte %in% 0:30]
# melt 
rcp <- melt(rcp,id.vars = c('dte'),variable.name = 'choice',value.name = 'observed_rcp')

# merge with 538 obs
obs <- merge(obs,rcp,by = c('dte','choice'),all = TRUE)

load(file = 'data_generated/nat_preds_f38_comparison.RData')
nat.pred_list_f38$R.D_margin <- nat.pred_list_f38$R-nat.pred_list_f38$D
nat.pred_list_f38 <- melt(nat.pred_list_f38,id.vars = c('dte','sim_id','training_data'),variable.name = 'choice',value.name = 'predicted')
nat.pred_list_f38$training_data <- as.factor(nat.pred_list_f38$training_data)
nat.pred_list_f38$choice <- as.factor(nat.pred_list_f38$choice)

# focus on daily change per state 
#nat.pred_list_f38 <- nat.pred_list_f38 [rev(order(nat.pred_list_f38 $dte))]
#nat.pred_list_f38[,predicted:=c(NA,diff(predicted)),by= c('state_simple','sim_id','choice','training_data')]
#nat.pred_list_f38 <- nat.pred_list_f38[!is.na(predicted)]

# get prediction interval
nat.pred_list_f38_5 <- nat.pred_list_f38[,lapply(.SD,function(x){quantile(x,c(0.05))}),by = c('choice','dte','training_data'),.SDcols = c('predicted')]
names(nat.pred_list_f38_5)[names(nat.pred_list_f38_5)=='predicted'] <- 'pred_5%'
nat.pred_list_f38_50 <- nat.pred_list_f38[,lapply(.SD,function(x){quantile(x,c(0.5))}),by = c('choice','dte','training_data'),.SDcols = c('predicted')]
names(nat.pred_list_f38_50)[names(nat.pred_list_f38_50)=='predicted'] <- 'pred_50%'
nat.pred_list_f38_95 <- nat.pred_list_f38[,lapply(.SD,function(x){quantile(x,c(0.95))}),by = c('choice','dte','training_data'),.SDcols = c('predicted')]
names(nat.pred_list_f38_95)[names(nat.pred_list_f38_95)=='predicted'] <- 'pred_95%'

nat.pred_list <- 
  merge(
    nat.pred_list_f38_5,
    merge(
      nat.pred_list_f38_50,nat.pred_list_f38_95,
      by = c('choice','dte','training_data')
    ),
    by = c('choice','dte','training_data')
  )


nat.pred_list <-
  merge(
    obs,
    nat.pred_list,
    all.x =TRUE,
    by = c('choice','dte')
  )


nat.pred_list <- nat.pred_list[!training_data %in% c('last_election','raw_ANES','538_forecast')]

levels(nat.pred_list$training_data)[
  which(levels(nat.pred_list$training_data)=="anes")
] = '5.ANES'
levels(nat.pred_list$training_data)[
  which(levels(nat.pred_list$training_data)=="anes_abc.wapo")
] = '6.ANES_abc.wapo'
levels(nat.pred_list$training_data)[
  which(levels(nat.pred_list$training_data)=="turks")
] = '4.turks'
levels(nat.pred_list$training_data)[
  which(levels(nat.pred_list$training_data)=="twitter_human")
] = '3.human_10tweets'
levels(nat.pred_list$training_data)[
  which(levels(nat.pred_list$training_data)=="twitter_gpt_10tweets")
] ='1.gpt_10tweets'
levels(nat.pred_list$training_data)[
  which(levels(nat.pred_list$training_data)=="twitter_gpt_5tweets")
] = '2.gpt_5tweets'

nat.pred_list$training_data <- as.factor(as.character(unlist(nat.pred_list$training_data)))
nat.pred_list$choice <- as.factor(nat.pred_list$choice)

levels(nat.pred_list$training_data) <- substr(levels(nat.pred_list$training_data),3,nchar(levels(nat.pred_list$training_data)))


# correlation between RCP and 538
cor(
  nat.pred_list[choice == 'R' & training_data==levels(nat.pred_list$training_data)[1]]$observed_538,
  nat.pred_list[choice == 'R' & training_data==levels(nat.pred_list$training_data)[1]]$observed_rcp
)
cor(
  nat.pred_list[choice == 'D' & training_data==levels(nat.pred_list$training_data)[1]]$observed_538,
  nat.pred_list[choice == 'D' & training_data==levels(nat.pred_list$training_data)[1]]$observed_rcp
)
cor(
  nat.pred_list[choice == 'R.D_margin' & training_data==levels(nat.pred_list$training_data)[1]]$observed_538,
  nat.pred_list[choice == 'R.D_margin' & training_data==levels(nat.pred_list$training_data)[1]]$observed_rcp
)


# prepare plots
col = list(D = 'blue',R = 'red',other = 'orange',R.D_margin = 'purple')

pdf(file = 'plots/preds_v_538_temporal_timeseries.pdf',width = 17.5,height = 7.5)
par(mfrow = c(3,6),mar = c(2,2,1,1),oma = c(3,3,2,0))
for(c in levels(nat.pred_list$choice)){
  
  y1 = nat.pred_list[choice == c & training_data==levels(nat.pred_list$training_data)[1] ][['observed_538']]
  y2 = nat.pred_list[choice == c & training_data==levels(nat.pred_list$training_data)[1] ][['observed_rcp']]
  
  for(t in levels(nat.pred_list$training_data) ){
    
    if(c!='R.D_margin'){
      ylab = substitute(paste(bold(paste('choice: ',value))),list(value = c))
    }else{
      ylab = substitute(paste(bold(value)),list(value = c))
    }
    
    if(c=='R'){ylim = c(0.4,0.6)}
    if(c == 'D'){ylim = c(0.45,0.65)}
    if(c == 'R.D_margin'){ylim = c(-0.15,0.15)}
    
    if(c==levels(nat.pred_list$choice)[1]){main = substitute(paste(bold(value)),list(value = t))}else{main = ''}
    
    yhat = nat.pred_list[choice == c & training_data==t ][['pred_50%']]
    yhat_lo = nat.pred_list[choice == c & training_data==t ][['pred_5%']]
    yhat_hi = nat.pred_list[choice == c & training_data==t ][['pred_95%']]
    
    x = nat.pred_list[choice == c & training_data==t][['dte']]
    
    plot(
      y = y1, x = x,
      xlim = c(30,1),
      col = col[[c]],
      type = 'l',
      xlab = '',
      ylab = '',
      main = '',
      lwd = 2,
      ylim = ylim ,
      yaxt = 'n'
    )
    lines(y = y2,x = x,col =col[[c]],lty = 2,lwd = 2)
    lines(y = yhat,x = x,col = 'black')
    
    if(c=='R'){abline(h = 0.4685,col = col[[c]],lwd = 2,lty = 3)}
    if(c=='D'){abline(h = 0.5131,col = col[[c]],lwd = 2,lty = 3)}
    if(c=='R.D_margin'){abline(h = 0.4685-0.5131,col = col[[c]],lwd = 2,lty = 3)}
          
    
    polygon(
      c(x,rev(x)),c(yhat_lo,rev(yhat_hi)),col = adjustcolor('black',0.1),
      border = NA
      )
    
    
    axis(side = 4,at = c(min(ylim),mean(ylim),max(ylim)),
         labels = round(c(min(ylim),mean(ylim),max(ylim)),2),
         tick = FALSE,line = -1,las = 3) 
    
    if(t==levels(nat.pred_list$training_data)[1]){
      axis(side = 2,
           at = mean(ylim),
           labels = ylab,
           tick = FALSE,las = 3,cex = 1.2) 
    }
    if(c==levels(nat.pred_list$choice)[1]){
      axis(side = 3,
           at = 15.5,
           labels = main ,
           tick = FALSE,
           cex.axis = 1.25) 
    }
    
    corr = c(cor(x = yhat, y = y1),cor(x = yhat, y = y2))
    if(c=='R'){error = yhat[x==0] - 0.4685; error.538 = y1[x==0] - 0.4685;  error.RCP = y2[x==0] - 0.4685; }
    if(c=='D'){error = yhat[x==0] - 0.5131; error.538 = y1[x==0] - 0.5131;  error.RCP = y2[x==0] - 0.5131;}
    if(c=='R.D_margin'){error = yhat[x==0] - (0.4685-0.5131); error.538 = y1[x==0] - (0.4685-0.5131);  error.RCP = y2[x==0] - (0.4685-0.5131);}
  
    legend(
      'topleft',
      legend = c(
        paste('e.d. error:',round(error,3)),
        paste('538 e.d. error:',round(error.538,3)),
        paste('RCP e.d. error:',round(error.RCP,3)),
        paste('pearson:( 538 =',round(corr[1],3),', RCP =',round(corr[2],3),')')
      ),
      lty = c(1,1,2,NA),
      col = c('black',col[[c]],col[[c]],NA),
      lwd = 2,
      bty = 'n'
    )
    
    #bias = c(mean(yhat -y1),mean(yhat -y2))
    #rmse = c(sqrt(mean((y1-yhat)^2)),sqrt(mean((y2-yhat)^2)))
    #
    #cover = c(mean(y1>=yhat_lo &  y1<=yhat_hi),mean(y2>=yhat_lo &  y2<=yhat_hi))
    
    #legend(
    #  'topleft',
    #  legend = c(
    #    paste('bias:( 538 =',round(bias[1],2),', RCP =',round(bias[2],2),')'),
    #    paste('rmse:( 538 =',round(rmse[1],2),', RCP =',round(rmse[2],2),')'),
    #    paste('pearson:( 538 =',round(corr[1],2),', RCP =',round(corr[2],2),')'),
    #    paste('coverage:( 538 =',round(cover[1],2),', RCP =',round(cover[2],2),')')
    #  ),
    #  bty = 'n'
    #)
  } }
dev.off()


pdf(file = 'plots/preds_v_538_temporal.pdf',width = 15,height = 7.5)
par(mfrow = c(3,6),mar = c(1,1,1,1),oma = c(3,3,2,0))
for(c in levels(nat.pred_list$choice)){
  
for(t in levels(nat.pred_list$training_data) ){

  if(c!='R.D_margin'){
    ylab = substitute(paste(bold(paste('choice: ',value))),list(value = c))
  }else{
      ylab = substitute(paste(bold(value)),list(value = c))
      }
  
    
  if(c==levels(nat.pred_list$choice)[1]){main = substitute(paste(bold(value)),list(value = t))}else{main = ''}
  
  
  y = nat.pred_list[choice == c & training_data==t ][['observed_538']]
  
  yhat = nat.pred_list[choice == c & training_data==t ][['pred_50%']]
  yhat_lo = nat.pred_list[choice == c & training_data==t ][['pred_5%']]
  yhat_hi = nat.pred_list[choice == c & training_data==t ][['pred_95%']]
  
  x = nat.pred_list[choice == c & training_data==t][['dte']]
  
  plot(x = yhat,y = y,xaxt = 'n',yaxt = 'n',ylab = '',xlab = '',
       pch = 16,
       ylim = c(min(c(y,yhat_lo)),max(c(y,yhat_hi))),
       xlim = c(min(c(y,yhat_lo)),max(c(y,yhat_hi))),
       col = adjustcolor(col[[c]],0.1),
       main = '')
  
    axis(side = 1,at = c(min(c(y,yhat_lo)),max(c(y,yhat_hi))),
         labels = round(c(min(c(y,yhat_lo)),max(c(y,yhat_hi))),2),
         tick = FALSE,line = -1) 
    
    axis(side = 4,at = c(min(c(y,yhat_lo)),max(c(y,yhat_hi))),
         labels = round(c(min(c(y,yhat_lo)),max(c(y,yhat_hi))),2),
         tick = FALSE,line = -1,las = 3) 
        
    if(t==levels(nat.pred_list$training_data)[1]){
      axis(side = 2,
           at =median(c(y,yhat_hi)),
           labels = ylab,
           tick = FALSE,las = 3,cex = 1.2) 
    }
    if(c==levels(nat.pred_list$choice)[1]){
      axis(side = 3,
           at =median(c(y,yhat_hi)),
           labels = main ,
           tick = FALSE) 
    }
    
  
  segments(x0 = yhat_lo , x1 =yhat_hi, y0 = y, y1 = y,
           col = adjustcolor(col[[c]],0.1))
  text(x = yhat,y = y,pch = x,cex = 0.8)
  abline(0,1,lty = 2)
  
  bias = mean(yhat -y)
  rmse = sqrt(mean((y-yhat)^2))
  corr = cor(x = yhat, y = y)
  cover = mean(y>=yhat_lo &  y<=yhat_hi)
  
  legend(
    'topleft',
    legend = c(
      paste('bias:',round(bias,2)),
      paste('rmse:',round(rmse,2)),
      paste('pearson:',round(corr,2)),
      paste('coverage:',round(cover,2))
    ),
    bty = 'n'
  )
} }
mtext('observed',side = 2,line = 1.5,outer = TRUE)
mtext('predicted',side = 1,line = 1.5,outer = TRUE)

dev.off()






