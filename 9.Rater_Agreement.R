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
# ensure warnings are not converted into errors
options(warn = 1)
# set work directory
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")

# load useful package
library(data.table)
library(dplyr)
library(mltools)
library(RColorBrewer)

# load stan and options
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# # # Prepare reliability data according to Krippendorf
tweets_map <-
    fread(file = 'data_generated/ThreeDatasetsSystem/MAP/Twitter_Map.csv')
humans <-
    fread(file = 'data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_Turks_Labels.csv')
humans <- merge(humans,unique(tweets_map),by = c('user_id'),all.x = TRUE)
humans$white <- ifelse(humans$ethnicity=='European',1,0)
# factor-order income
humans[
    commercial_estimated_hh_income=='[min, 25000)'
    ]$commercial_estimated_hh_income = '1.[min, 25000)'
humans[
    commercial_estimated_hh_income=='[25000, 50000)'
    ]$commercial_estimated_hh_income = '2.[25000, 50000)'
humans[
    commercial_estimated_hh_income=='[50000, 75000)'
    ]$commercial_estimated_hh_income = '3.[50000, 75000)'
humans[
    commercial_estimated_hh_income=='[75000, 100000)'
    ]$commercial_estimated_hh_income = '4.[75000, 100000)'
humans[
    commercial_estimated_hh_income=='[100000, max]'
    ]$commercial_estimated_hh_income = '5.[100000, max]'
humans$commercial_estimated_hh_income <-
    as.factor(humans$commercial_estimated_hh_income)
# factor order age
humans[
    age_bins=='18-24'
    ]$age_bins = '1.18-24'
humans[
    age_bins=='25-34'
    ]$age_bins = '2.25-34'
humans[
    age_bins=='35-44'
    ]$age_bins = '3.35-44'
humans[
    age_bins=='45-54'
    ]$age_bins = '4.45-54'
humans[
    age_bins=='55-64'
    ]$age_bins = '5.55-64'
humans[
    age_bins=='65+'
    ]$age_bins = '6.65+'
humans$age_bins <-
    as.factor(humans$age_bins)
# drop `other` class for vbote 2020 on humans -
# this was not in the gpt classification
humans <- humans[humans$vote2020!='other']

gpt_10 <- fread(file = 'data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_GPT_Labels_10_Tweets.csv')
gpt_10 <- merge(gpt_10,unique(tweets_map),by = c('user_id'),all.x = TRUE)
gpt_10$white <- ifelse(gpt_10$ethnicity=='European',1,0)
# factor-order income
gpt_10[
    commercial_estimated_hh_income=='[min, 25000)'
    ]$commercial_estimated_hh_income = '1.[min, 25000)'
gpt_10[
    commercial_estimated_hh_income=='[25000, 50000)'
    ]$commercial_estimated_hh_income = '2.[25000, 50000)'
gpt_10[
    commercial_estimated_hh_income=='[50000, 75000)'
    ]$commercial_estimated_hh_income = '3.[50000, 75000)'
gpt_10[
    commercial_estimated_hh_income=='[75000, 100000)'
    ]$commercial_estimated_hh_income = '4.[75000, 100000)'
gpt_10[
    commercial_estimated_hh_income=='[100000, max]'
    ]$commercial_estimated_hh_income = '5.[100000, max]'
gpt_10$commercial_estimated_hh_income <-
    as.factor(gpt_10$commercial_estimated_hh_income)
# factor order age
gpt_10[
    age_bins=='18-24'
    ]$age_bins = '1.18-24'
gpt_10[
    age_bins=='25-34'
    ]$age_bins = '2.25-34'
gpt_10[
    age_bins=='35-44'
    ]$age_bins = '3.35-44'
gpt_10[
    age_bins=='45-54'
    ]$age_bins = '4.45-54'
gpt_10[
    age_bins=='55-64'
    ]$age_bins = '5.55-64'
gpt_10[
    age_bins=='65+'
    ]$age_bins = '6.65+'
gpt_10$age_bins <-
    as.factor(gpt_10$age_bins)


tweets_map <- fread(file = 'data_generated/ThreeDatasetsSystem/MAP/Large_Twitter_Map.csv')
gpt_5 <- fread(file = 'data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_GPT_Labels_5_Tweets.csv')
gpt_5 <- merge(gpt_5,unique(tweets_map),by = c('user_id'),all.x = TRUE)
gpt_5$white <- ifelse(gpt_5$ethnicity=='European',1,0)
# factor-order income
gpt_5[
    commercial_estimated_hh_income=='[min, 25000)'
    ]$commercial_estimated_hh_income = '1.[min, 25000)'
gpt_5[
    commercial_estimated_hh_income=='[25000, 50000)'
    ]$commercial_estimated_hh_income = '2.[25000, 50000)'
gpt_5[
    commercial_estimated_hh_income=='[50000, 75000)'
    ]$commercial_estimated_hh_income = '3.[50000, 75000)'
gpt_5[
    commercial_estimated_hh_income=='[75000, 100000)'
    ]$commercial_estimated_hh_income = '4.[75000, 100000)'
gpt_5[
    commercial_estimated_hh_income=='[100000, max]'
    ]$commercial_estimated_hh_income = '5.[100000, max]'
gpt_5$commercial_estimated_hh_income <-
    as.factor(gpt_5$commercial_estimated_hh_income)
# factor order age
gpt_5[
    age_bins=='18-24'
    ]$age_bins = '1.18-24'
gpt_5[
    age_bins=='25-34'
    ]$age_bins = '2.25-34'
gpt_5[
    age_bins=='35-44'
    ]$age_bins = '3.35-44'
gpt_5[
    age_bins=='45-54'
    ]$age_bins = '4.45-54'
gpt_5[
    age_bins=='55-64'
    ]$age_bins = '5.55-64'
gpt_5[
    age_bins=='65+'
    ]$age_bins = '6.65+'
gpt_5$age_bins <-
    as.factor(gpt_5$age_bins)

# define variables of interest
V.vec =
    c(
        'vote2020',
        'state_simple_abbreviation',
        'gender',
        'ethnicity',
        'white',
        'commercial_estimated_hh_income',
        'age_bins',
        'party_code',
        'vote2016',
        'modeled_college_grad',
        'marital_status_code'
        )


# ready empty list for alpha
krip.list <- list()
# ready empty list of JAGS data
V.list <- list()

# loop through each variable to calculate alpha and generate JAGS-ready data
for(v in V.vec){

# assign names to identify raters
tmp.humans <- humans[,c('Input.user_id',v),with=F]
names(tmp.humans)[names(tmp.humans)==v] <- paste(v,'humans',sep = '.')

tmp.gpt_10 <- gpt_10[,c('Input.user_id',v),with=F]
names(tmp.gpt_10)[names(tmp.gpt_10)==v] <- paste(v,'gpt_10',sep = '.')

tmp.gpt_5 <- gpt_5[,c('Input.user_id',v),with=F]
names(tmp.gpt_5)[names(tmp.gpt_5)==v] <- paste(v,'gpt_5',sep = '.')

# merge to create reliability data
tmp.V <-
merge(
    merge(
        tmp.humans,tmp.gpt_10,
        by = c('Input.user_id'),
        all = TRUE
        ),
        tmp.gpt_5,
        by = c('Input.user_id'),
        all.x = TRUE
        )

# clean empty levels and assign missing values
if(class(tmp.V[[2]])=='factor'){
if(any(levels(tmp.V[[2]])=='')){
    levels(tmp.V[[2]])[levels(tmp.V[[2]])==''] <- NA
}
if(any(levels(tmp.V[[3]])=='')){
    levels(tmp.V[[3]])[levels(tmp.V[[3]])==''] <- NA
}
if(any(levels(tmp.V[[4]])=='')){
    levels(tmp.V[[4]])[levels(tmp.V[[4]])==''] <- NA
} }else{

if(any(tmp.V[[2]][!is.na(tmp.V[[2]])]=='')){ tmp.V[tmp.V[[2]]==''][[2]] <- NA }
if(any(tmp.V[[3]][!is.na(tmp.V[[3]])]=='')){ tmp.V[tmp.V[[3]]==''][[3]] <- NA }
if(any(tmp.V[[4]][!is.na(tmp.V[[4]])]=='')){ tmp.V[tmp.V[[4]]==''][[4]] <- NA }

}

# calculate krippendorf alpha
if( v %in%
    c(
        'vote2020',
        'state_simple_abbreviation',
        'gender',
        'ethnicity',
        'white',
        'party_code',
        'vote2016',
        'modeled_college_grad',
        'marital_status_code'
    )){
        metric <- 'nominal'
        }

if( v %in% c('commercial_estimated_hh_income','age_bins')){
    metric <- 'ordinal'
    }

krip_human.gpt10 <-
    icr::krippalpha(
        t(tmp.V[,paste(v,c('humans','gpt_10'),sep = '.'),with=F]),
        metric = metric,
        bootstrap = TRUE,
        bootnp = TRUE
        )
print(krip_human.gpt10)
krip_human.gpt5 <-
    icr::krippalpha(
        t(tmp.V[,paste(v,c('humans','gpt_5'),sep = '.'),with=F]),
        metric = metric,
        bootstrap = TRUE,
        bootnp = TRUE
        )
print(krip_human.gpt5)
krip_gpt10.gpt5 <-
    icr::krippalpha(
        t(tmp.V[,paste(v,c('gpt_10','gpt_5'),sep = '.'),with=F]),
        metric = metric,
        bootstrap = TRUE,
        bootnp = TRUE
        )
print(krip_gpt10.gpt5)

# store bootstrap simulations
krip <-
as.data.table(
    cbind(
        krip_human.gpt10$bootstraps,
        krip_human.gpt5$bootstraps,
        krip_gpt10.gpt5$bootstraps
    ) )
names(krip) <- c('humans.gpt10','humans.gpt5','gpt10.gpt5')
krip <- list(krip)
names(krip) <- v
krip.list <-
    append(
        krip.list,
        krip
    )

# prep JAGS data
tmp.1 <- as.matrix(table(tmp.V[,grepl(v,names(tmp.V)),with=F][,c(1,2)]))

if(any(rownames(tmp.1)!=rownames(tmp.1))){
    warning('Error: column and row names of agreement matrix do not match');
    break
    }

tmp.2 <- as.matrix(table(tmp.V[,grepl(v,names(tmp.V)),with=F][,c(1,3)]))

if(any(rownames(tmp.2)!=rownames(tmp.2))){
    warning('Error: column and row names of agreement matrix do not match');
    break
    }

tmp.3 <- as.matrix(table(tmp.V[,grepl(v,names(tmp.V)),with=F][,c(2,3)]))

if(any(rownames(tmp.3)!=rownames(tmp.3))){
    warning('Error: column and row names of agreement matrix do not match');
    break
    }

tmp.list <-
list(
    tmp.1,
    tmp.2,
    tmp.3
)

names(tmp.list) <-
c(
    'humans_gpt-3.5-turbo (10 tweets)',
    'humans_gpt-3.5-turbo (5 tweets)',
    'gpt-3.5-turbo (10 tweets)_gpt-3.5-turbo (5 tweets)'
)

names(tmp.list) <- paste(names(tmp.list),v,sep='___')

V.list <-
append(
    V.list,
    tmp.list
    )

}

# define colours we want to be associated with each variable we are evaluating
cols <- RColorBrewer::brewer.pal(length(V.vec),'Set3')
names(cols) <- V.vec

plot.list <-
    c(
        'state_simple_abbreviation',
        'gender','ethnicity','age_bins',
        'modeled_college_grad','commercial_estimated_hh_income',
        'vote2020','vote2016'
        )

    # plot denisty of alpha on the same plot for the relevant avriables
    for(k in 1:3){
        main = c(
            'human v. gpt-3.5-turbo (10 tweets)',
            'human v. gpt-3.5-turbo (5 tweets)',
            'gpt-3.5-turbo (10 tweets) v. gpt-3.5-turbo (5 tweets)'
            )[k]
    pdf(
        file = paste('plots/krippendorff.',make.names(main),'.pdf',sep = ''),
        width = 10,height = 3.5
        )
    dense <- density(krip.list[[plot.list[1]]][[k]])
    max.y <- 80#max(dense$y)
    plot(
        dense,
        xlim = c(min(unlist(krip.list)),1),
        ylim = c(0,max.y),
        zero.line = FALSE,
        main = main,
        xlab = "Krippendorff's alpha",
        col = NA
        )
    for(i in 1:length(plot.list)){

        par(new = TRUE)

        dense <- density(krip.list[[plot.list[i]]][[k]])

        plot(
            dense,
            xlim = c(min(unlist(krip.list)),1),
            ylim = c(0,max( max.y)),
            zero.line = FALSE,
            bty = 'n',
            xaxt = 'n',
            yaxt = 'n',
            main = '',
            xlab = '',
            ylab = '',
            col = adjustcolor('black',0.35)
        )

        min.x <- median(krip.list[[plot.list[i]]][[k]])

text.y = 5 + max( dense$y)

        text(
            x = min.x,
            y = ifelse(i==1,70,text.y),
            srt = -45,
            label =
            gsub('modeled_','',
            gsub('_simple_abbreviation','',
            gsub('commercial_estimated_','',plot.list[i]) ) ),
            adj = 1,
            cex = 0.9
            )

        polygon(
            dense,
            col = adjustcolor(cols[plot.list[i]],0.5)
            )

        segments(
            x0 = dense$x[which(dense$y ==  max(dense$y))],
            x1 = dense$x[which(dense$y ==  max(dense$y))],
            y0 = 0,
            y1 = max(dense$y),
            lty = 1,
            col = adjustcolor('black',0.35)
            )
    }
    abline(v = 0,lty = 2)
    dev.off()
    }


# # # Contingency Matrices
library(RColorBrewer)
library(grDevices)
library(xtable)

print(xtable(V.list[[1]]),include.row.names = FALSE)

# estimate incidence probability
library(R2jags)

jags.list <- list()

for(l in 1:length(V.list)){

jags.data <-
    list(
        L = dim(V.list[[l]])[1],
        A = V.list[[l]],
        contrast = names(V.list)[l]
    )

jags.model <-
"model{

    for(i in 1:L){
        for(j in 1:L){

            A[i,j] ~ dpois(lambda[i,j])

            lambda[i,j] <- exp(mu[i,j])

            mu[i,j] <- log.C + log.sigma[i] + log.tau[j] + log(1 + r*B[i,j])

            B[i,j] ~ dbern(pi[i,j])

            pi[i,j] ~ dbeta(1/2,1/2)
    } }

    log.C ~ dnorm(0,0.01)

    for(ij in 1:L){
        log.sigma[ij] ~ dnorm(0,prec.sigma)
        log.tau[ij] ~ dnorm(0,prec.tau)
    }

    prec.sigma <- 1/(sd.sigma^2)
    prec.tau <- 1/(sd.tau^2)

    sd.sigma ~ dunif(0,5)
    sd.tau ~ dunif(0,5)

    r ~ dexp(0.01)

# # # Generate networks

    for(i in 1:L){
        for(j in 1:L){
    B.star[i,j] ~ dbern(pi[i,j])
    } }
}"

tmpf=tempfile()
tmps=file(tmpf,"w")
cat(jags.model,file=tmps)
close(tmps)

params.to.store <-
    c(
        'log.C',
        'log.sigma',
        'log.tau',
        'r',
        'B',
        'B.star',
        'pi'
    )

jags.fit <-
jags.parallel(
    data = jags.data,
    parameters.to.save = params.to.store,
    n.iter = 20000,
    model.file = tmpf,
    n.chains = 8,
    n.cluster = 8,
    n.burnin = 15000,
    n.thin = 8
    )

jags.fit$jags.data <- jags.data

jags.list <- append(jags.list,list(jags.fit))

# print stat
print(paste('completed',round(100*l/length(V.list),2),'%...'))

}




for(l in 1:length(V.list)){

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "vote2020"){
        cex = 3; width = 0.81*3; height = 0.865*3.5; main = '2020 vote';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = rep(0.5,4); line.lab = 3.5; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "ethnicity"){
        cex = 3; width = 0.945*3.5; height = 0.975*4; main = 'ethnicity';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = c(5,5,0,0); line.lab = 8; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "age_bins"){
        cex = 3; width = 0.725*3.5; height = 0.775*4; main = 'age bins';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = rep(0,4); line.lab = 3; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "commercial_estimated_hh_income"){
        cex = 3;  width = 0.79*3.5; height = 0.82*4; main = 'household income';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = c(2.5,2.5,0,0); line.lab = 5.5; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "state_simple_abbreviation"){
        cex = 3; width = 12; height = 12.5; main = 'state';
        line.pct = 0.035; cex.main = 2 ; cex.lab = 1.5;
        cex.axis = 1; oma = rep(0,4); line.lab = 3; line.main = 1}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "gender"){
        cex = 10; width = 0.895*3; height = 0.925*3.5; main = 'gender';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = rep(0,4); line.lab = 3; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "party_code"){
        cex = 3; width = 0.895*3; height = 0.935*3.5; main = 'party reg.';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = c(4,4,0,0); line.lab = 7; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "vote2016"){
        cex = 3; width = 0.775*3; height = 0.815*3.5; main = '2016 vote';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = c(1,1,0,0); line.lab = 4; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "modeled_college_grad"){
        cex = 5; width = 0.85*3; height = 0.9*3.5; main = 'education';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = c(3,3,0,0); line.lab = 6.5; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "white"){
        cex = 10; width = 0.895*3; height = 0.925*3.5; main = 'white ethnicity';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = rep(0,4); line.lab = 3; line.main = 1.5}

    if(sub('.*___', '',jags.list[[l]]$jags.data$contrast) == "marital_status_code"){
        cex = 5; width = 0.85*3; height = 0.9*3.5; main = 'marital status';
        line.pct = 0.05; cex.main = 1 ; cex.lab = 0.75; cex.axis = 0.65;
        oma = c(3,3,0,0); line.lab = 6.5; line.main = 1.5}

    pdf(
        file = paste('plots/network.',make.names(names(V.list)[l]),'.pdf',sep = ''),
        width = width,
        height = height
        )

    # plot incidence matrices average
    B.star <- jags.list[[l]]$BUGSoutput$mean$B.star

    # Define pastel blue, white, and pastel orange
    colours <- c('dodgerblue4','white','orangered1')
    #colours <- c('white','dodgerblue4')
    # Create a palette function with the specified colors
    palette_function <- colorRampPalette(colours)
    # cut up Bstar mean
    cut.Bstar <-
    cut(
        as.numeric(B.star),
        c(-0.01,seq(0.05,1,by=0.05))
        )
    palette <- palette_function(nlevels(cut.Bstar))
    palette.Bstar <- palette[as.integer(cut.Bstar)]
    palette.Bstar.array <-
    array(
        palette.Bstar,
        dim(B.star)
        )

par(oma = oma)

    plot(
        x = 0:(dim(B.star)[1]),
        y = 0:(dim(B.star)[1]),
        pch = NA,
        xlab = '',
        ylab = '',
        xaxt = 'n',
        yaxt = 'n',
        bty = 'n',
        main = '',
        cex.lab = cex.lab
        )

    mtext(main,side = 3,outer = FALSE,cex = cex.main,line = line.main,font = 1)
    mtext(sub('.*_', '', sub("\\___.*", "",jags.list[[l]]$jags.data$contrast)),side = 1,outer = FALSE,cex = cex.lab,line = line.lab)
    mtext(sub("\\_.*", "",jags.list[[l]]$jags.data$contrast),side = 2,outer = FALSE,cex = cex.lab,line = line.lab,las = 3)

    axis(
        side = 2,
        at = seq(0.5,dim(B.star)[1]-0.5,by = 1),
        label = rev(sub('Modeled ', '\\1', sub('.+\\.(.+)', '\\1', rownames(jags.list[[l]]$jags.data$A)))),
        las = 2,
        col.ticks = NA,
        lty = 0,
        line = -line.pct*jags.list[[l]]$jags.data$L,
        cex.axis = cex.axis
        )

    axis(
        side = 1,
        at = seq(0.5,dim(B.star)[1]-0.5,by = 1),
        label = sub('Modeled ', '\\1',sub('.+\\.(.+)', '\\1', colnames(jags.list[[l]]$jags.data$A))),
        las = 2,
        col.ticks = NA,
        lty = 0,
        line = -line.pct*jags.list[[l]]$jags.data$L,
        cex.axis = cex.axis
        )


    for(i in 1:dim(B.star)[1]){
        for(j in 1:dim(B.star)[1]){
    points(
        x = seq(0.5,(dim(B.star)[1]-0.5),by = 1)[j],
        y = rev(seq(0.5,(dim(B.star)[1]-0.5),by = 1))[i],
        pch = 22,
        bg = palette.Bstar.array[i,j],
        cex = cex,
        col = 'black'
    ) } }

    dev.off()
}


# # # Perfect agreement plot

cex = 3; width = 3.33; height = 2.9;
line.pct = 0.05; cex.main = 1 ; cex.lab = 1;
cex.axis = 1; line.lab = 3;
line.main = 1.5

pdf(
    file = paste('plots/network.perfect.pdf',sep = ''),
    width = 2*width,
    height = height
    )

par(mfrow =c (1,2),oma = c(0,0,0,10))

for(perfect in c(TRUE,FALSE)){

    # plot incidence matrices average
if(perfect){
    B.star <- array(0,dim(V.list[['humans_gpt-3.5-turbo (10 tweets)___vote2020']]))
    diag(B.star) <- 1
     main = 'perfect agreement'
}else{
    B.star <- array(1,dim(V.list[['humans_gpt-3.5-turbo (10 tweets)___vote2020']]))
    diag(B.star) <- 0
    main = 'perfect disagreement'
}

    # Define pastel blue, white, and pastel orange
    colours <- c('dodgerblue4','white','orangered1')
    # Create a palette function with the specified colors
    palette_function <- colorRampPalette(colours)
    # cut up Bstar mean
    cut.Bstar <-
    cut(
        as.numeric(B.star),
        c(-0.01,seq(0.05,1,by=0.05))
        )
    palette <- palette_function(nlevels(cut.Bstar))
    palette.Bstar <- palette[as.integer(cut.Bstar)]
    palette.Bstar.array <-
    array(
        palette.Bstar,
        dim(B.star)
        )


    plot(
        x = 0:(dim(B.star)[1]),
        y = 0:(dim(B.star)[1]),
        pch = NA,
        xlab = '',
        ylab = '',
        xaxt = 'n',
        yaxt = 'n',
        bty = 'n',
        main = '',
        cex.lab = cex.lab
        )

    mtext(main,side = 3,outer = FALSE,cex = cex.main,line = line.main)
    mtext('rater 2',side = 1,outer = FALSE,cex = cex.lab,line = line.lab)
    mtext('rater 1',side = 2,outer = FALSE,cex = cex.lab,line = line.lab,las = 3)

    axis(
        side = 2,
        at = seq(0.5,dim(B.star)[1]-0.5,by = 1),
        label = rev(c('A','B','C','D','E')),
        las = 2,
        col.ticks = NA,
        lty = 0,
        line = -line.pct*5,
        cex.axis = cex.axis
        )

    axis(
        side = 1,
        at = seq(0.5,dim(B.star)[1]-0.5,by = 1),
        label = c('A','B','C','D','E'),
        las = 2,
        col.ticks = NA,
        lty = 0,
        line = -line.pct*5,
        cex.axis = cex.axis
        )


    for(i in 1:dim(B.star)[1]){
        for(j in 1:dim(B.star)[1]){
    points(
        x = seq(0.5,(dim(B.star)[1]-0.5),by = 1)[j],
        y = rev(seq(0.5,(dim(B.star)[1]-0.5),by = 1))[i],
        pch = 22,
        bg = palette.Bstar.array[i,j],
        cex = cex,
        col = 'black'
    ) } }
}

n.legend = 10
cut.min = 0
cut.max = 1

pal.leg <- palette_function(n.legend)

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
      x = 10, y = 7,
      title = expression(bar(B)^" *"),
      legend = legend,
      pch = 15,
      col = pal.leg,
      xpd = NA,
      bty = 'n',
      pt.cex = 2.5,
      cex = 0.75
    )


dev.off()

# plot r for every variable
plot.list <-
    c(
        'state_simple_abbreviation',
        'gender','ethnicity','age_bins',
        'modeled_college_grad','commercial_estimated_hh_income',
        'vote2020','vote2016'
        )

    pdf(
        file = paste('plots/network.preference.pdf',sep = ''),
        width = 10,height = 5
        )
    par(oma = c(0,3,0,0))

    # plot denisty of alpha on the same plot for the relevant avriables
    for(k in 1:3){
        main = c(
            'humans v. gpt-3.5-turbo (10 tweets)',
            'humans v. gpt-3.5-turbo (5 tweets)',
            'gpt-3.5-turbo (10 tweets) v. gpt-3.5-turbo (5 tweets)'
            )[k]

    L <- which(
        grepl(
            make.names(gsub(' v. ','_',main)),
            make.names(sub("\\___.*", "", names(V.list)))
            ) &
        grepl(
            paste0(plot.list[-1],collapse = '|'),
            make.names(sub('.+___(.+)', '\\1', names(V.list)))
            )
    )

    r <- sapply(L,function(j){jags.list[[j]]$BUGSoutput$sims.list$r})
    var.names <- sub('.+___(.+)', '\\1', names(V.list))[L]

    r.summary <- apply(r,2,function(x){quantile(x,c(0.05,0.5,0.95))})
    max.x = 100 #max(r.summary)

    if(k == 1){
    order.r <- order(r.summary['50%',])
    plot(
        x = r.summary['50%',order.r],
        y = 1:dim(r)[2],
        xlim = c(0,max.x),
        ylim = c(0.5,max.y),
        pch = NA,
        ylab = '',
        yaxt = 'n',
        bty = 'n',
        main = 'incident preference',
        xlab = 'r',
        xaxt = 'n'
        )
    axis(side = 1, at = seq(0,max.x,by = 5),las = 2,cex.axis = 0.85)
    abline(v = 0,lty = 2)
    axis(
        side = 2,
        at = 1:dim(r)[2],
        labels =
         gsub('modeled_','',
            gsub('_simple_abbreviation','',
            gsub('commercial_estimated_','',
            var.names[order.r]
            ) ) ),
        las = 2
        )
    }
    points(
        x = r.summary['50%',order.r],
        y = 1:dim(r)[2]-c(0.1,0.2,0.3)[k],
        pch = 16,
        col = c('black','purple','orange')[k]
        )
    segments(
        x0 = r.summary['5%',order.r],
        y0 = 1:dim(r)[2]-c(0.1,0.2,0.3)[k],
        x1 = r.summary['95%',order.r] ,
        y1 = 1:dim(r)[2]-c(0.1,0.2,0.3)[k],
        col = adjustcolor(c('black','purple','orange')[k],0.25)
        )

legend(
    'topright',
    legend = c(
        'humans v. gpt-3.5-turbo (10 tweets)',
        'humans v. gpt-3.5-turbo (5 tweets)',
        'gpt-3.5-turbo (10 tweets) v. gpt-3.5-turbo (5 tweets)'
        ),
    col = c('black','purple','orange'),
    lty = 1,
    pch = 16,
    cex = 0.8
        )

    }
    dev.off()


