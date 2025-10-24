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
library(R2jags)

# ---------- Part (1): Prepare Data ---------- #

# Load the tweet<->user map and MTurk human labels.
# Merge on user_id to carry map info onto human labels.
# Construct a 'white' indicator from 'ethnicity'.
# Recode ordered factors for income and age by prefixing with 1., 2., ... so they sort naturally.
# Drop 'other' from vote2020 (not present in GPT classifications).

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

# Load GPT labels based on 10 tweets per user; merge map info.
# Create the same 'white' indicator and apply the *same* ordered recodes for income and age.
# Keep categories aligned with humans so reliability matrices are comparable.
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

# Reload an expanded map for broader coverage and merge it into the 5-tweet GPT labels.
# Mirror the recoding for income and age to ensure consistent category ordering.
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

# Define the set of variables (V.vec) to analyze for agreement/reliability.
# These will be looped over to build rater-by-rater contingency (agreement) matrices.
V.vec =
  c(
    'vote2020',
    'state_simple_abbreviation',
    'gender',
    'ethnicity',
    'commercial_estimated_hh_income',
    'age_bins',
    'vote2016',
    'modeled_college_grad'
  )


# Prepare containers:
# - V.list will hold the three LxL contingency matrices per variable (human vs GPT10, human vs GPT5, GPT10 vs GPT5).
V.list <- list()

# For each variable v:
# 1) Extract (Input.user_id, v) from humans, GPT(10), and GPT(5), renaming to tag the rater.
# 2) Merge by Input.user_id to align the three "ratings" for each user.
# 3) Normalize blanks to NA and force a shared level set across the three columns (see FIX block).
# 4) Build three LxL contingency tables: (human,GPT10), (human,GPT5), and (GPT10,GPT5).
# 5) Sanity-check that row/col names match (shared levels) before proceeding.
# 6) Store the three matrices in V.list with names 'contrast___variable' for downstream modeling/plots.
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
  
  # prep JAGS data
  tmp.1 <- as.matrix(table(tmp.V[,grepl(v,names(tmp.V)),with=F][,c(1,2)]))
  
  if (any(rownames(tmp.1) != colnames(tmp.1))) {
    warning('Error: column and row names of agreement matrix do not match'); break
  }
  
  tmp.2 <- as.matrix(table(tmp.V[,grepl(v,names(tmp.V)),with=F][,c(1,3)]))
  
  if(any(rownames(tmp.2)!=colnames(tmp.2))){
    warning('Error: column and row names of agreement matrix do not match');
    break
  }
  
  tmp.3 <- as.matrix(table(tmp.V[,grepl(v,names(tmp.V)),with=F][,c(2,3)]))
  
  if(any(rownames(tmp.3)!=colnames(tmp.3))){
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





# ---------- Part (2): JAGS Model ---------- #

# For each stored agreement matrix A:
# - Build JAGS data with L = number of categories and A = LxL counts.
# - Specify a Poisson log-linear model with row/column effects (sigma, tau),
#   a global intensity (C), and a Bernoulli 'network' B[i,j] with Beta(1/2,1/2) prior for pi[i,j].
# - r ~ Exp(0.01) modulates how much extra mass is added when B[i,j]=1 via log(1 + r*B[i,j]).
# - B.star is a posterior predictive draw of the network for later diagnostics/plots.
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

    # Generate networks

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
  
  # Fit via jags.parallel(): 8 chains, 20k iter, 15k burn-in, thin=8.
  # Save posterior draws of {log.C, log.sigma, log.tau, r, B, B.star, pi}.
  # Attach jags.data back onto the fit for easy access to A and labels.
  # Print a simple percent-complete progress line across contrasts/variables.
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




# ---------- Part (3): Plotting ---------- #

# Load plotting stack 
suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
  library(scales)
  library(ggnewscale)
})

# Define:
# - Which variables are ordinal (age, income) for weighted metrics,
# - Color palette and displayed labels for the three comparisons,
# - Small numeric constants for stylistic tweaks in later figures.
ORDINAL_VARS <- c("age_bins", "commercial_estimated_hh_income")  # ordered vars to weight
WEIGHT_POWER <- 2L                # quadratic penalty for off-diagonals (1L=linear, 3L=cubic…)
LOGIT_EPS    <- 1e-6              # clamp to avoid +/-Inf in logit
OUT_DIR <- "plots"
VARS_MAIN <- # variables for the multi-facet plot
  c("gender","ethnicity","age_bins","modeled_college_grad","commercial_estimated_hh_income","vote2020","vote2016")
ROW1_VARS <- c('gender',"vote2016","vote2020","modeled_college_grad")
ROW2_VARS <- c("age_bins","ethnicity","commercial_estimated_hh_income")
VAR_STATE <- "state_simple_abbreviation" # state will be plotted separately
CONTRASTS <- c(
  "humans_gpt-3.5-turbo (10 tweets)",
  "humans_gpt-3.5-turbo (5 tweets)",
  "gpt-3.5-turbo (10 tweets)_gpt-3.5-turbo (5 tweets)"
)

# Desired within-variable order (top -> bottom in each row)
COMP_LEVELS <- c(
  "Human vs GPT-3.5 (10)",
  "Human vs GPT-3.5 (5)",
  "GPT-3.5 (10) vs (5)"
)

# Colors for comparisons (match order above)
COMP_COLS <- c(
  "Human vs GPT-3.5 (10)" = "dodgerblue",
  "Human vs GPT-3.5 (5)"  = "orangered",
  "GPT-3.5 (10) vs (5)"   = "darkolivegreen"
)



# build_tilde_w(): Construct symmetric pair weights over category pairs.
# - For ordinal variables, farther-apart categories get larger penalties (|i-j| scaled to [0,1] and ^power).
# - For nominal variables, all off-diagonal pairs are weighted equally.
# - Normalizes weights over the upper triangle so metrics are comparable across L.
build_tilde_w <- function(L, variable, power = WEIGHT_POWER, ordinal_vars = ORDINAL_VARS) {
  W <- matrix(0, L, L)
  if (L > 1) {
    d <- abs(row(W) - col(W)) / (L - 1)
    if (variable %in% ordinal_vars) {
      W <- d^power
      diag(W) <- 0
    } else {
      W[,] <- 1
      diag(W) <- 0
    }
  }
  upper <- upper.tri(W)
  denom <- sum(W[upper])
  if (denom == 0) return(W)          # trivial L<=1 case
  W / denom                          # this is \tilde w, symmetric and globally normalised
}



# pretty_var()/agree_var_label*: Clean internal variable names into human-friendly labels.
pretty_var <- function(x) {
  x <- gsub("_simple_abbreviation", "", x)
  x <- gsub("commercial_estimated_", "", x)
  x <- gsub("modeled_", "", x)
  x <- gsub("_", " ", x)
  trimws(x)
}



# comp_label(): Map internal contrast names (with underscores) to readable legend labels.
comp_label <- function(x) {
  out <- x
  out[grepl("^humans_gpt-3\\.5-turbo \\(10 tweets\\)$", out)] <- "Human vs GPT-3.5 (10)"
  out[grepl("^humans_gpt-3\\.5-turbo \\(5 tweets\\)$",  out)] <- "Human vs GPT-3.5 (5)"
  out[grepl("^gpt-3\\.5-turbo \\(10 tweets\\)_gpt-3\\.5-turbo \\(5 tweets\\)$", out)] <- "GPT-3.5 (10) vs (5)"
  out
}



# agree_var_label(): clean variable names to readable labels
agree_var_label <- function(vname) {
  switch(vname,
         "age_bins"                       = "age bins",
         "vote2020"                       = "vote 2020",
         "vote2016"                       = "vote 2016",
         "commercial_estimated_hh_income" = "household income",
         "modeled_college_grad"           = "education",
         "state_simple_abbreviation"      = "state",
         vname)
}



# vectorised wrapper (so switch() only ever sees length-1)
agree_var_label_vec <- function(v) vapply(v, agree_var_label, FUN.VALUE = character(1))



# add global left-Y and bottom-X labels to an existing plot (no legend inside)
add_global_axes <- function(p, xlab, ylab, x_size = 14, y_size = 14,left_pad = 0.04, bottom_pad = 0.06) {
  ggdraw() +
    draw_label(ylab, x = 0.015, y = 0.5, angle = 90, vjust = 0.5, size = y_size) +
    draw_plot(p, x = left_pad, y = bottom_pad, width = 1 - left_pad, height = 1 - bottom_pad) +
    draw_label(xlab, x = 0.5, y = 0.02, vjust = 0.5, size = x_size)
}



# add a title and subtitle above a plot
add_title_sub <- function(plot, title, subtitle = NULL,
                          title_size = 16, subtitle_size = 12,
                          title_x = 0.03, subtitle_x = 0.03,    # << new knobs
                          title_y = 0.98, subtitle_y = 0.94,
                          plot_top_pad = 0.16) {
  g <- cowplot::ggdraw()
  g <- g + cowplot::draw_plot(plot, 0, 0, 1, 1 - plot_top_pad)
  g <- g + cowplot::draw_label(
    title, x = title_x, y = title_y, hjust = 0, vjust = 1,
    fontface = "bold", size = title_size
  )
  if (!is.null(subtitle)) {
    g <- g + cowplot::draw_label(
      subtitle, x = subtitle_x, y = subtitle_y, hjust = 0, vjust = 1,
      size = subtitle_size
    )
  }
  g
}



# combine the body with a legend on the right, with an adjustable spacer between them
with_right_legend <- function(body, legend, gap_rel = 0.04, legend_rel = 0.12) {
  plot_grid(body, plot_spacer(), legend, ncol = 3, rel_widths = c(1, gap_rel, legend_rel), align = "h")
}



# display_labels(): Collapse near-duplicate category text (e.g., "Hispanic and Portuguese" -> "Hispanic"),
display_labels <- function(vals, var) {
  x <- trimws(as.character(vals))
  if (identical(var, "ethnicity")) {
    x <- gsub("East and South Asian", "Asian", x, ignore.case = TRUE)
    x <- gsub("Hispanic(?:\\s*(?:and)?\\s*\\s*Portuguese)?", "Hispanic", x, ignore.case = TRUE, perl = TRUE)
    x <- ifelse(tolower(x) %in% c("likely african-american","african american"), "African American", x)
    x <- ifelse(tolower(x) == "european","European",
                ifelse(tolower(x) == "asian","Asian",
                       ifelse(tolower(x) == "hispanic","Hispanic",
                              ifelse(tolower(x) == "other","Other", x))))
  }
  x <- sub("^\\s*\\d+\\.?\\s*", "", x)  # remove enumeration prefixes like "1.18-24"
  if (identical(var, "modeled_college_grad")) x <- sub(' Degree','',sub("^\\s*Modeled\\s+", "", x))
  if (identical(var, "commercial_estimated_hh_income")) {
    x <- gsub("\\s", "", x)
    x <- gsub("\\)$", "]", x)
    x <- ifelse(x %in% c("[min,25000]", "[min,25000)", "[min,25000]"), "less than 25k",
                ifelse(x %in% c("[25000,50000]", "[25000,50000)"), "25k to 50k",
                       ifelse(x %in% c("[50000,75000]", "[50000,75000)"), "50k to 75k",
                              ifelse(x %in% c("[75000,100000]", "[75000,100000)"), "75k to 100k",
                                     ifelse(x %in% c("[100000,max]", "[100000,max)"), "100k or more", x)))))
  }
  x
}



# order_indices_from_display(): Produce stable sort order per variable so heatmaps/plots are legible and consistent.
order_indices_from_display <- function(orig_labels, var) {
  disp <- display_labels(orig_labels, var)
  
  if (identical(var, "vote2020")) {
    target <- c("stay home","D","R","G","L")
    key <- match(tolower(disp), tolower(target))
    return(order(key, na.last = TRUE))
    
  } else if (identical(var, "vote2016")) {
    target <- c("stay home","D","R","other")
    key <- match(tolower(disp), tolower(target))
    return(order(key, na.last = TRUE))
    
  } else if (identical(var, "age_bins")) {
    # FIX: make "65+" sort FIRST pre-reversal so it appears LAST after our global rev()
    lows <- suppressWarnings(as.numeric(sub("^\\s*(\\d+).*", "\\1", disp)))
    is_plus <- grepl("\\+\\s*$", disp)  # be tolerant of trailing spaces
    score <- lows
    if (any(is_plus, na.rm = TRUE)) score[is_plus] <- -Inf
    return(rev(order(score, na.last = TRUE)))
    
  } else if (identical(var, "commercial_estimated_hh_income")) {
    order_target <- c("less than 25k","25k to 50k","50k to 75k","75k to 100k","100k or more")
    key <- match(disp, order_target)
    return(rev(order(key, na.last = TRUE)))
    
  } else if (identical(var, "ethnicity")) {
    target <- c("European","African American","Hispanic","Asian","Other")
    key <- match(disp, target)
    return(order(is.na(key), key, disp))
    
  } else {
    return(order(disp))
  }
}





# ======== GLOBAL METRIC Φ from B.star DIFFERENCES (no rescaling) ========

# For each posterior draw t of B.star (an LxL 0/1 matrix):
# 1) Form per-row differences D_ij = B_ii - B_ij (and set D_ii = 0).
# 2) Symmetrise to a pairwise score M_ij = 0.5 * {(B_ii - B_ij) + (B_jj - B_ji)}, M_ii = 0.
# 3) Collapse to a single scalar φ^(t) by a normalised weight matrix \tilde w over i<j:
#        φ^(t) = sum_{i<j} \tilde w_{ij} * M_ij.
#    (Ordinal variables use distance-based weights; nominal use equal weights.)
# 4) Return the full posterior {φ^(t)} plus summaries (mean, 5%, 95%).


phi_from_bstar <- function(jfit, variable) {
  Bdraws <- jfit$BUGSoutput$sims.list$B.star
  n_iter <- dim(Bdraws)[1]
  L <- dim(Bdraws)[2]; stopifnot(dim(Bdraws)[3] == L)
  wtil <- build_tilde_w(L, variable)
  upper <- upper.tri(wtil)
  phi_draws <- numeric(n_iter)
  for (t in seq_len(n_iter)) {
    B <- Bdraws[t, , ]
    diagB <- diag(B)
    D <- matrix(0, L, L); for (i in seq_len(L)) D[i, ] <- diagB[i] - B[i, ]
    diag(D) <- 0
    M <- 0.5 * (D + t(D)); diag(M) <- 0
    phi_draws[t] <- sum(wtil[upper] * M[upper])
  }
  list(
    phi_draws = phi_draws,
    phi_mean  = mean(phi_draws),
    phi_q05   = as.numeric(quantile(phi_draws, 0.05, names = FALSE)),
    phi_q10   = as.numeric(quantile(phi_draws, 0.10, names = FALSE)),  # <-- add
    phi_q90   = as.numeric(quantile(phi_draws, 0.90, names = FALSE)),  # <-- add
    phi_q95   = as.numeric(quantile(phi_draws, 0.95, names = FALSE)),
    L         = L,
    weighted  = variable %in% ORDINAL_VARS
  )
}


phi_bstar_summary <- lapply(seq_along(jags.list), function(l) {
  variable <- sub(".*\\___",  "", names(V.list)[l])
  contrast <- sub("\\___.*", "", names(V.list)[l])
  out <- phi_from_bstar(jags.list[[l]], variable)
  
  data.frame(
    contrast    = contrast, variable = variable, 
    L = out$L,
    phi_mean    = out$phi_mean, 
    phi_q05 = out$phi_q05, 
    phi_q10 = out$phi_q10, 
    phi_q90 = out$phi_q90,
    phi_q95 = out$phi_q95,
    p_phi_gt_0  = mean(out$phi_draws > 0), weighted = out$weighted,
    stringsAsFactors = FALSE
  )
})


# Calcylate and clean summaries 
phi_bstar_summary <- do.call(rbind, phi_bstar_summary)
phi_bstar_summary <- subset(phi_bstar_summary, !variable %in% c("marital_status_code","white","party_code"))
phi_bstar_summary$variable_lab <- agree_var_label_vec(phi_bstar_summary$variable)
phi_bstar_summary$comp         <- comp_label(phi_bstar_summary$contrast)
phi_bstar_summary$comp         <- factor(phi_bstar_summary$comp, levels = COMP_LEVELS)
target_comp <- "Human vs GPT-3.5 (10)"
idx <- phi_bstar_summary$comp == target_comp & !is.na(phi_bstar_summary$variable_lab)

if (any(idx)) {
  agg <- tapply(phi_bstar_summary$phi_mean[idx],
                as.character(phi_bstar_summary$variable_lab[idx]),
                function(z) mean(z, na.rm = TRUE))
  ord <- names(sort(agg, decreasing = TRUE))
  all_vars <- unique(as.character(phi_bstar_summary$variable_lab))
  y_levels_for_plot_phi <- c(ord, setdiff(all_vars, ord))
} else {
  y_levels_for_plot_phi <- unique(as.character(phi_bstar_summary$variable_lab))
}



# Build a dot+interval plot of φ per variable and comparison:
# - Central point = posterior mean, line = 90% interval, vertical zero reference,
# - Variables ordered by the "Human vs GPT-3.5 (10)" mean for readability,
# - Annotate each with Pr(φ>0). Save to plots/coefplot_global_phi_bstar.pdf.
# Softer / pastel comparison colours

# --- Palettes / levels (same as before) ---
COMP_COLS_SOFT <- c(
  "Human vs GPT-3.5 (10)" = "#8EC5FF",
  "Human vs GPT-3.5 (5)"  = "#FFB59E",
  "GPT-3.5 (10) vs (5)"   = "#B9D8A2"
)
COMP_COLS_DARK <- c(
  "Human vs GPT-3.5 (10)" = "#4EA2FF",
  "Human vs GPT-3.5 (5)"  = "#FF8A68",
  "GPT-3.5 (10) vs (5)"   = "#8FBD64"
)
COMP_LEVELS <- names(COMP_COLS_SOFT)

# --- Order variables by the "Human vs GPT-3.5 (10)" mean (base R) ---
tmp_idx <- which(phi_bstar_summary$comp == "Human vs GPT-3.5 (10)")
ord <- order(-phi_bstar_summary$phi_mean[tmp_idx])
y_levels_for_plot_phi <- phi_bstar_summary$variable_lab[tmp_idx][ord]

# --- Symmetric x-limits around zero ---
x_max_phi <- max(abs(c(phi_bstar_summary$phi_q10, phi_bstar_summary$phi_q90)), na.rm = TRUE)
x_lim_phi <- c(-x_max_phi, x_max_phi)

# ===== Manual vertical dodge (works on any ggplot2) =====
# One knob to change spacing between comparisons within each row:
SPACING <- 0.28   # try 0.20 (tighter) … 0.40 (looser)


# --- Legend helper (only if it's not already defined) ---
  draw_key_errorbarh <- function(data, params, size) {
    lwd <- data$linewidth * tryCatch(ggplot2::.pt, error = function(...) 2.845276)
    gp  <- grid::gpar(col = scales::alpha(data$colour %||% "black", data$alpha %||% 1),
                      lwd = lwd, lty = data$linetype %||% 1, lineend = "butt")
    grid::grobTree(
      grid::segmentsGrob(x0 = grid::unit(0.1, "npc"), x1 = grid::unit(0.9, "npc"),
                         y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"), gp = gp),
      grid::segmentsGrob(x0 = grid::unit(c(0.1, 0.9), "npc"), x1 = grid::unit(c(0.1, 0.9), "npc"),
                         y0 = grid::unit(c(0.35, 0.35), "npc"), y1 = grid::unit(c(0.65, 0.65), "npc"), gp = gp)
    )
  }


# Offsets per comparison (centered at 0). Order must match COMP_LEVELS.
offset_map <- c(-SPACING, 0, +SPACING)     # three groups left/center/right
names(offset_map) <- COMP_LEVELS

# Build plotting data with precomputed y positions (no dplyr)
df <- phi_bstar_summary
df$comp <- factor(df$comp, levels = COMP_LEVELS)
# base row index for each variable (1..K), according to ordered levels
y_base <- match(df$variable_lab, y_levels_for_plot_phi)
# add offset by comparison
df$y <- y_base + offset_map[as.character(df$comp)]

# Axis breaks/labels at the base row positions (not at offset positions)
y_breaks <- seq_along(y_levels_for_plot_phi)
y_labels <- y_levels_for_plot_phi

# --- MAIN: intervals + squares ---
y_min <- 1L
y_max <- length(y_levels_for_plot_phi)

# --- MAIN: intervals + squares (only change: add limits= and tighten expand) ---
p_phi_main <- ggplot(df, aes(x = y)) +
  geom_errorbar(
    aes(ymin = phi_q10, ymax = phi_q90, colour = comp),
    width = 0.15, linewidth = 0.9, lineend = "butt", show.legend = FALSE
  ) +
  geom_point(
    aes(y = phi_mean, fill = comp),
    shape = 22, size = 2.8, stroke = 0.5, colour = "black", show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
  scale_y_continuous(
    limits = x_lim_phi, breaks = pretty(x_lim_phi, n = 9),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_continuous(
    limits = c(y_min-0.5, y_max+0.5),
    breaks = y_breaks, labels = y_labels,
    expand = expansion(mult = c(0.00, 0.02))
  ) +
  scale_colour_manual(values = COMP_COLS_SOFT, limits = COMP_LEVELS, breaks = COMP_LEVELS) +
  scale_fill_manual  (values = COMP_COLS_SOFT, limits = COMP_LEVELS, breaks = COMP_LEVELS) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey85"),
    panel.grid.major.x = element_line(linewidth = 0.3, colour = "grey90"),
    axis.text          = element_text(colour = "black"),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),  # <-- diagonal x labels
    plot.margin        = margin(5.5, 5.5, 5.5, 5.5)
  )
p_phi_main <- p_phi_main +
  labs(y = expression(phi)) +
  theme(
    axis.title.y = element_text(
      angle = 0, vjust = 0.5, size = 12, margin = margin(r = 6)
    )
  )

# --- PR(φ>0) STRIP placed ABOVE the main plot, with diagonal numbers ---
p_phi_pr_top <- ggplot(df, aes(x = y)) +
  geom_text(
    aes(y = 0.55, label = sprintf("%.2f", p_phi_gt_0), colour = comp, alpha = p_phi_gt_0),
    angle = 45, hjust = 0, vjust = 0.5, size = 3.6, show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(y_min - 0.5, y_max + 0.5),
    breaks = y_breaks, labels = NULL,
    expand = expansion(mult = c(0.00, 0.02))
  ) +
  # Keep a small headroom so diagonals aren’t clipped
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.05, 0.08))) +
  scale_colour_manual(values = COMP_COLS_DARK, limits = COMP_LEVELS, breaks = COMP_LEVELS) +
  scale_alpha(range = c(0.30, 1.00), limits = c(0, 1), guide = "none") +
  labs(
    y = expression("Pr(" * phi * ">0)")
  ) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 15) +
  theme(
    # Make the strip title diagonal and position it on the same baseline as the numbers
    axis.title.y = element_text(
      angle  = 0,      # diagonal label
      size   = 11,      # slightly smaller
      hjust  = 0,       # left-justify along angled baseline
      vjust  = 1,   # <-- aligns to the same vertical fraction as y_top
      margin = margin(r = -30)  # small nudge toward the panel
    ),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin  = margin(5.5, 5.5, 0, 5.5)
  )

# --- Build a tiny plot ONLY to generate the legend ---
p_phi_for_legend <- ggplot(df, aes(x = y)) +
  geom_errorbar(
    aes(ymin = phi_q10, ymax = phi_q90, colour = comp),
    width = 0.15, linewidth = 0.9, lineend = "butt",
    show.legend = TRUE, key_glyph = draw_key_errorbarh
  ) +
  geom_point(
    aes(y = phi_mean, fill = comp),
    shape = 22, size = 3.2, stroke = 0.5, colour = "black", show.legend = TRUE
  ) +
  scale_colour_manual(name = "", values = COMP_COLS_SOFT, limits = COMP_LEVELS, breaks = COMP_LEVELS) +
  scale_fill_manual  (name = "", values = COMP_COLS_SOFT, limits = COMP_LEVELS, breaks = COMP_LEVELS) +
  guides(
    fill   = guide_legend(order = 1, override.aes = list(shape = 22, size = 3.2, stroke = 0.5)),
    colour = guide_legend(order = 1, override.aes = list(linetype = 1, linewidth = 0.9))
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.key.width = grid::unit(1.8, "lines"),
        legend.key.height = grid::unit(1.4, "lines"),
        legend.position = "right")

leg <- cowplot::get_legend(p_phi_for_legend)

# --- Stack top strip ABOVE the main plot (legend stays on the right as before) ---
body_vert    <- cowplot::plot_grid(p_phi_pr_top, p_phi_main, nrow = 2, rel_heights = c(0.1, 1.00), align = "v")
body_plus_leg<- cowplot::plot_grid(body_vert, leg, ncol = 2, rel_widths = c(1, 0.35), align = "h")

# --- Title/Sub stays the same ---
p_phi_bstar <- add_title_sub(
  body_plus_leg,
  title    = expression("Global Agreement"),
  subtitle = "MC Mean + 80% Credible Interval",
  title_size = 16, subtitle_size = 12,
  title_x = 0.06, subtitle_x = 0.06,   # << more left
  title_y = 0.985, subtitle_y = 0.915,
  plot_top_pad = 0.15
)

# Save & print as before
ggsave("plots/coefplot_global_phi_bstar.pdf", p_phi_bstar, width = 10, height = 4.5, device = "pdf", useDingbats = FALSE)
print(p_phi_bstar)















# ======== Δ HEATMAPS (B.star differences, symmetrised per draw then averaged) ========
# For each posterior draw t:
#   1) D^(t)_ij = B^(t)_ii - B^(t)_ij  with D^(t)_ii = 0
#   2) M^(t)    = 0.5 * ( D^(t) + t(D^(t)) ), so M^(t)_ij = 0.5 * {(Bii-Bij) + (Bjj-Bji)}, M^(t)_ii=0
# We then return Δ_ij = mean_t M^(t)_ij, formatted and ordered for plotting.

delta_bstar_table_for_l <- function(l) {
  jfit <- jags.list[[l]]
  var  <- sub(".*\\___","", names(V.list)[l])
  A    <- jfit$jags.data$A
  rn0  <- rownames(A); cn0 <- colnames(A)
  stopifnot(!is.null(rn0), !is.null(cn0))
  
  sims_B <- jags.list[[l]]$BUGSoutput$sims.list$B.star  # [iter, L, L]
  L      <- dim(sims_B)[2]; stopifnot(dim(sims_B)[3] == L)
  n_iter <- dim(sims_B)[1]
  
  # --- symmetrised per-draw difference, then average over draws ---
  sum_M <- matrix(0, L, L)
  for (t in seq_len(n_iter)) {
    Bt <- sims_B[t, , ]
    D  <- diag(Bt) - Bt
    diag(D) <- 0
    M  <- 0.5 * (D + t(D))
    diag(M) <- 0
    sum_M <- sum_M + M
  }
  Delta <- sum_M / n_iter
  dimnames(Delta) <- list(rn0, cn0)
  
  # --- order rows/cols using display-driven order ---
  ord_r  <- order_indices_from_display(rn0, var)
  ord_c  <- order_indices_from_display(cn0, var)
  rn_ord <- rn0[ord_r]
  cn_ord <- cn0[ord_c]
  Delta  <- Delta[rn_ord, cn_ord, drop = FALSE]
  
  # --- tidy; keep raw keys to mark diagonal; then pretty labels ---
  DT <- as.data.table(as.table(Delta))
  setnames(DT, c("human_raw","rater2_raw","delta"))
  DT[, is_diag := (human_raw == rater2_raw)]
  
  y_levels_raw <- rev(rn_ord)
  x_levels_raw <- rev(cn_ord)
  y_disp <- display_labels(y_levels_raw, var)
  x_disp <- display_labels(x_levels_raw, var)
  
  DT[, human        := factor(human_raw,  levels = y_levels_raw, labels = y_disp)]
  DT[, rater2       := factor(rater2_raw, levels = x_levels_raw, labels = x_disp)]
  DT[, variable     := var]
  DT[, variable_lab := agree_var_label_vec(var)]
  
  DT[, .(variable, variable_lab, human, rater2, delta, is_diag)]
}

build_contrast_dt_delta_bstar <- function(target_contrast, variables) {
  rbindlist(lapply(variables, function(v){
    l <- which(sub("\\___.*","", names(V.list)) == target_contrast &
                 sub(".*\\___","", names(V.list)) == v)[1]
    if (is.na(l)) return(NULL)
    delta_bstar_table_for_l(l)
  }), use.names = TRUE)
}

plot_contrast_faceted_delta_bstar <- function(DT, contrast_label, file_stub) {
  stopifnot(nrow(DT) > 0)
  DT$row <- ifelse(DT$variable %in% ROW1_VARS, "row1",
                   ifelse(DT$variable %in% ROW2_VARS, "row2", NA))
  DT <- DT[!is.na(DT$row), ]
  max_abs <- 1 
  lims <- c(-max_abs, max_abs)
  
  make_row_plot <- function(dsub, show_legend = TRUE) {
    wanted <- if (unique(dsub$row) == "row1") ROW1_VARS else ROW2_VARS
    dsub$variable_lab <- factor(dsub$variable_lab, levels = agree_var_label_vec(wanted))
    
    order_for <- function(vals, var) { vals <- as.character(vals); vals[ order_indices_from_display(vals, var) ] }
    by_var <- split(dsub, dsub$variable)
    
    x_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$rater2), v); paste(v, lv, sep = " :: ")
    }), use.names = FALSE)
    y_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$human), v); paste(v, rev(lv), sep = " :: ")
    }), use.names = FALSE)
    
    dsub$rater2_plot <- factor(paste(dsub$variable, dsub$rater2, sep = " :: "), levels = unique(x_levels_all))
    dsub$human_plot  <- factor(paste(dsub$variable, dsub$human,  sep = " :: "), levels = unique(y_levels_all))
    
    d_diag <- dsub[ is_diag == TRUE ]
    d_off  <- dsub[ is_diag == FALSE ]
    
    ggplot() +
      # OFF-DIAGONAL heat
      geom_tile(data = d_off,
                aes(x = rater2_plot, y = human_plot, fill = delta),
                color = "grey65", linewidth = 0.25, show.legend = TRUE) +
      # DIAGONAL tiles: fixed light grey, no text
      geom_tile(data = d_diag,
                aes(x = rater2_plot, y = human_plot),
                fill = "grey90", color = "grey65", linewidth = 0.25, show.legend = FALSE) +
      # Numbers only for OFF-DIAGONAL
      geom_text(data = d_off,
                aes(x = rater2_plot, y = human_plot, label = sprintf("%.2f", delta)), size = 3) +
      scale_fill_gradient2(
        low = "orangered", mid = "white", high = "springgreen",
        midpoint = 0, limits = lims,
        name = expression(E( 0.5~"["~Delta[ij]+Delta[ji]~"]"))) +
      scale_x_discrete(labels = function(z) sub("^.* :: ", "", z)) +
      scale_y_discrete(labels = function(z) sub("^.* :: ", "", z)) +
      facet_wrap(~ variable_lab, scales = "free", nrow = 1) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = if (show_legend) "right" else "none",
        plot.margin = margin(2, 32, 2, 2)
      )
  }
  
  p_top    <- make_row_plot(DT[DT$row=="row1", ], show_legend = TRUE)
  p_bottom <- make_row_plot(DT[DT$row=="row2", ], show_legend = FALSE)
  leg <- cowplot::get_legend(p_top)
  p_top_noleg <- p_top + theme(legend.position = "none")
  
  title <- ggdraw() + draw_label(
    bquote("B"^"*" * " Posterior Difference (symmetrised, avg. over draws)"),
    fontface = "bold", x = 0.01, hjust = 0, size = 18
  )
  subtitle <- ggdraw() + draw_label(
    paste0("", gsub('\\_',' v. ',contrast_label)),
    x = 0.01, hjust = 0, size = 16
  )
  
  plot_spacer <- function(){ ggplot() + theme_void() +
      theme(panel.background = element_rect(fill = NA, color = NA),
            plot.background  = element_rect(fill = NA, color = NA)) }
  
  stacked_rows <- cowplot::plot_grid(p_top_noleg, p_bottom, ncol = 1, align = "v")
  stacked      <- cowplot::plot_grid(title, subtitle, plot_spacer(), stacked_rows, ncol = 1,
                                     rel_heights = c(0.05, 0.05, 0.01, 1), align = "v")
  body_plus_leg <- cowplot::plot_grid(stacked, plot_spacer(), leg, ncol = 3,
                                      rel_widths = c(1, 0.01, 0.125), align = "h")
  
  xlab <- strsplit(contrast_label,'\\_')[[1]][2]
  ylab <- strsplit(contrast_label,'\\_')[[1]][1]
  final <- add_global_axes(body_plus_leg, xlab = xlab, ylab = ylab, left_pad = 0.06, bottom_pad = 0.08)
  
  out_file <- file.path(OUT_DIR, sprintf("delta_Bstar_heatmaps__TWO_ROWS__%s.pdf",
                                         gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, final, width = 15, height = 8)
  message("Saved: ", out_file)
  final
}

plot_state_single_delta_bstar <- function(DT, contrast_label, file_stub) {
  stopifnot(nrow(DT) > 0, unique(DT$variable) == VAR_STATE)
  max_abs <- max(abs(DT$delta), na.rm = TRUE)
  lims <- c(-max_abs, max_abs)
  
  d_diag <- DT[ is_diag == TRUE ]
  d_off  <- DT[ is_diag == FALSE ]
  
  p <- ggplot() +
    # OFF-DIAGONAL heat
    geom_tile(data = d_off,
              aes(x = rater2, y = human, fill = delta),
              color = "grey65", linewidth = 0.25, show.legend = TRUE) +
    # DIAGONAL tiles: fixed light grey, no text
    geom_tile(data = d_diag,
              aes(x = rater2, y = human),
              fill = "grey90", color = "grey65", linewidth = 0.25, show.legend = FALSE) +
    # Numbers only for OFF-DIAGONAL
    geom_text(data = d_off,
              aes(x = rater2, y = human, label = sprintf("%.2f", delta)), size = 2.7) +
    scale_fill_gradient2(
      low = "orangered", mid = "white", high = "springgreen",
      midpoint = 0, limits = lims,
      name = expression(E( 0.5~"["~Delta[ij]+Delta[ji]~"]"))) +
    scale_x_discrete(limits = function(x) rev(x)) +
    coord_cartesian(clip = "off") +
    labs(x = strsplit(contrast_label,'\\_')[[1]][2],
         y = strsplit(contrast_label,'\\_')[[1]][1],
         title    = expression("State: B"^"*" * " Posterior Difference (symmetrised, avg. over draws)"),
         subtitle = paste0("", gsub('\\_',' v. ',contrast_label))) +
    theme_minimal(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  out_file <- file.path(OUT_DIR, sprintf("delta_Bstar_heatmap__STATE__%s.pdf",
                                         gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, p, width = 13, height = 10)
  message("Saved: ", out_file)
  p
}

for (ctr in CONTRASTS) {
  DT_main_db <- build_contrast_dt_delta_bstar(ctr, VARS_MAIN)
  if (nrow(DT_main_db) > 0) print(plot_contrast_faceted_delta_bstar(DT_main_db, ctr, paste0(ctr, "_MAIN_DELTA_BSTAR")))
  DT_state_db <- build_contrast_dt_delta_bstar(ctr, VAR_STATE)
  if (nrow(DT_state_db) > 0) print(plot_state_single_delta_bstar(DT_state_db, ctr, paste0(ctr, "_STATE_DELTA_BSTAR")))
}






# ===================== Expected-change heatmaps ======================

# Simulate expected joint counts under the fitted Human–Machine model (HM) and a Human–Human baseline (HH),
# with baseline mixture parameter κ (default 0.8) and notional sample size n (default 1000).
# Plot |HM-HH| as the main fill; overlay a small black/white mini-square encoding certainty
# (Pr(sign of mean difference)), power-transformed for contrast. Save PDFs into plots/.
CERT_POWER   <- 8
power_trans <- function(p) {
  scales::trans_new(
    name      = paste0("pow-", p),
    transform = function(x) x^p,
    inverse   = function(x) x^(1/p),
    domain    = c(0, Inf)
  )
}

MINI_SIZE    <- 1/4
MINI_OFFSET  <- 0.5 - MINI_SIZE/2
TEXT_MARGIN  <- 0.06

expected_joint_hm_draw <- function(jfit, it, use_mean_bstar = FALSE) {
  sims <- jfit$BUGSoutput$sims.list
  s    <- exp(sims$log.sigma[it, ])
  t    <- exp(sims$log.tau[it, ])
  r    <- as.numeric(sims$r[it])
  BSTAR <- if (use_mean_bstar) apply(sims$B.star, c(2, 3), mean) else sims$B.star[it, , ]
  HM <- outer(s, t) * (1 + r * BSTAR)
  HM <- HM / sum(HM)
  rn <- rownames(jfit$jags.data$A); cn <- colnames(jfit$jags.data$A)
  dimnames(HM) <- list(rn, cn)
  HM
}

expected_joint_hh_from_marg <- function(p, dimnames_like, kappa = 0.8) {
  p <- as.numeric(p); stopifnot(abs(sum(p) - 1) < 1e-8)
  D <- diag(p)
  J <- kappa * D + (1 - kappa) * (p %o% p)
  dimnames(J) <- dimnames_like
  J
}


summarize_diff_for_l <- function(l, kappa = 0.8, n = 1000L) {
  jfit <- jags.list[[l]]
  sims <- jfit$BUGSoutput$sims.list
  n_iter <- dim(sims$pi)[1]
  rn0 <- rownames(jfit$jags.data$A); cn0 <- colnames(jfit$jags.data$A)
  Lr <- length(rn0); Lc <- length(cn0); stopifnot(Lr == Lc)
  
  sum_D   <- matrix(0, Lr, Lc, dimnames = list(rn0, cn0))
  pos_cnt <- matrix(0L, Lr, Lc, dimnames = list(rn0, cn0))
  neg_cnt <- matrix(0L, Lr, Lc, dimnames = list(rn0, cn0))
  zero_cnt<- matrix(0L, Lr, Lc, dimnames = list(rn0, cn0))    # NEW
  
  col_pos_cnt <- setNames(rep(0L, Lc), cn0)
  col_neg_cnt <- setNames(rep(0L, Lc), cn0)
  col_zero_cnt<- setNames(rep(0L, Lc), cn0)                   # NEW
  
  for (it in seq_len(n_iter)) {
    HM <- expected_joint_hm_draw(jfit, it)
    p_row <- rowSums(HM)
    HH <- expected_joint_hh_from_marg(p_row, dimnames(HM), kappa = kappa)
    D  <- n * (HM - HH)
    
    sum_D   <- sum_D + D
    pos_cnt <- pos_cnt + (D > 0)
    neg_cnt <- neg_cnt + (D < 0)
    zero_cnt<- zero_cnt + (D == 0)                            
    
    col_sums <- colSums(D)
    col_pos_cnt  <- col_pos_cnt  + (col_sums > 0)
    col_neg_cnt  <- col_neg_cnt  + (col_sums < 0)
    col_zero_cnt <- col_zero_cnt + (col_sums == 0)            
  }
  
  mean_D <- sum_D / n_iter
  
  # ---- CHANGED: use the median direction instead of mean direction ----
  # If more draws are positive than negative, median > 0; if more negative, median < 0;
  # ties (including many zeros) are treated as non-negative direction (>= 0), matching your prior >= rule.
  dir_is_nonneg <- (pos_cnt >= neg_cnt)                       
  cert_med      <- ifelse(dir_is_nonneg,                      
                          pos_cnt / n_iter,
                          neg_cnt / n_iter)
  # --------------------------------------------------------------------
  
  var  <- sub(".*\\___","", names(V.list)[l])
  ord_r <- order_indices_from_display(rn0, var)
  ord_c <- order_indices_from_display(cn0, var)
  rn_ord <- rn0[ord_r]; cn_ord <- cn0[ord_c]
  y_levels_raw <- rev(rn_ord)
  x_levels_raw <- rev(cn_ord)
  
  mean_D <- mean_D[y_levels_raw, x_levels_raw, drop = FALSE]
  cert   <- cert_med[y_levels_raw, x_levels_raw, drop = FALSE]  
  
  DT <- as.data.table(as.table(mean_D)); setnames(DT, c("human_raw","rater2_raw","mean_diff"))
  DT[, certainty := as.vector(cert)]                              
  DT[, abs_mean  := abs(mean_diff)]
  DT[, human  := factor(human_raw,  levels = y_levels_raw, labels = display_labels(y_levels_raw, var))]
  DT[, rater2 := factor(rater2_raw, levels = x_levels_raw, labels = display_labels(x_levels_raw, var))]
  DT[, variable     := var]
  DT[, variable_lab := agree_var_label_vec(var)]
  
  ## TOTAL row (column-sum):
  col_mean <- colSums(mean_D)
  # CHANGED: median direction for column sums
  dir_col_nonneg <- (col_pos_cnt >= col_neg_cnt)                  
  col_cert_med   <- ifelse(dir_col_nonneg,                        
                           col_pos_cnt / n_iter,
                           col_neg_cnt / n_iter)
  col_cert_med <- col_cert_med[x_levels_raw]                      
  
  y_levels_tot <- c("TOTAL", y_levels_raw)
  y_labels_tot <- c("TOTAL", display_labels(y_levels_raw, var))
  
  DT_tot <- data.table(
    human_raw  = "TOTAL",
    rater2_raw = x_levels_raw,
    mean_diff  = as.numeric(col_mean),
    certainty  = as.numeric(col_cert_med),                        # CHANGED
    abs_mean   = abs(as.numeric(col_mean)),
    human      = factor("TOTAL", levels = y_levels_tot, labels = y_labels_tot),
    rater2     = factor(display_labels(x_levels_raw, var), levels = display_labels(x_levels_raw, var)),
    variable   = var,
    variable_lab = agree_var_label_vec(var)
  )
  
  DT[, human := factor(as.character(human), levels = y_labels_tot)]
  out <- rbindlist(list(DT, DT_tot), use.names = TRUE)
  out[]
}

build_diff_dt <- function(target_contrast, variables, kappa = 0.8, n = 1000L) {
  out <- lapply(variables, function(v){
    l <- which(sub("\\___.*","", names(V.list)) == target_contrast &
                 sub(".*\\___","", names(V.list)) == v)[1]
    if (is.na(l)) return(NULL)
    summarize_diff_for_l(l, kappa = kappa, n = n)
  })
  rbindlist(Filter(Negate(is.null), out), use.names = TRUE)
}

plot_expected_change_faceted <- function(DT, contrast_label, file_stub, n = 1000L, kappa = 0.8) {
  stopifnot(nrow(DT) > 0)
  DT$row <- ifelse(DT$variable %in% ROW1_VARS, "row1",
                   ifelse(DT$variable %in% ROW2_VARS, "row2", NA))
  DT <- DT[!is.na(DT$row), ]
  max_abs <- max(DT$abs_mean, na.rm = TRUE)
  
  make_row_plot <- function(dsub, show_legend = TRUE) {
    wanted <- if (unique(dsub$row) == "row1") ROW1_VARS else ROW2_VARS
    dsub$variable_lab <- factor(dsub$variable_lab, levels = agree_var_label_vec(wanted))
    
    order_for <- function(vals, var) { 
      vals <- as.character(vals)
      vals[ order_indices_from_display(vals, var) ] 
    }
    
    by_var <- split(dsub, dsub$variable)
    
    # X levels (rater2): flip only income
    x_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$rater2), v)
      if (identical(v, "commercial_estimated_hh_income")) lv <- rev(lv)
      paste(v, lv, sep = " :: ")
    }), use.names = FALSE)
    
    # Y levels (human): include "TOTAL" on top; keep income orientation rule for non-TOTAL levels
    y_levels_all <- unlist(lapply(names(by_var), function(v) {
      base <- order_for(unique(by_var[[v]]$human), v)
      has_tot <- "TOTAL" %in% base
      base_no_tot <- setdiff(base, "TOTAL")
      lv <- if (identical(v, "commercial_estimated_hh_income")) base_no_tot else rev(base_no_tot)
      lv <- c("TOTAL", lv)  # TOTAL always first
      paste(v, lv, sep = " :: ")
    }), use.names = FALSE)
    
    dsub$rater2_plot <- factor(paste(dsub$variable, dsub$rater2, sep = " :: "), levels = unique(x_levels_all))
    dsub$human_plot  <- factor(paste(dsub$variable, dsub$human,  sep = " :: "), levels = unique(y_levels_all))
    
    # Pretty labels: strip "var :: " and render TOTAL as expression(sum)
    lab_strip_char <- function(z) sub("^.* :: ", "", z)
    
    # robust: returns an expression vector; "TOTAL" -> Sigma, others safely quoted
    lab_strip_expr <- function(z) {
      raw <- lab_strip_char(z)
      esc <- function(s) {
        s2 <- gsub("\\\\", "\\\\\\\\", s)  # escape backslashes
        s2 <- gsub("'",  "\\\\'",    s2)   # escape single quotes
        paste0("'", s2, "'")
      }
      txt <- ifelse(raw == "TOTAL", "Sigma", vapply(raw, esc, character(1)))
      parse(text = txt)
    }
    
    max_abs <- max(dsub$abs_mean, na.rm = TRUE)
    
    ggplot(dsub, aes(x = rater2_plot, y = human_plot)) +
      geom_tile(aes(fill = abs_mean), color = "grey65", linewidth = 0.25) +
      scale_fill_gradient(low = "springgreen", high = "orangered", limits = c(0, max_abs),
                          name = expression("|"~Delta~"|")) +
      ggnewscale::new_scale_fill() +
      geom_tile(aes(fill = certainty),
                width = MINI_SIZE, height = MINI_SIZE,
                position = position_nudge(x = MINI_OFFSET, y = MINI_OFFSET), color = NA) +
      scale_fill_gradient(name = expression(Pr(sign)),
                          low = "white", high = "black", limits = c(0, 1),
                          breaks = c(0.70, 0.80, 0.90, 0.97, 0.99),
                          labels = scales::percent_format(accuracy = 1),
                          trans  = power_trans(CERT_POWER)) +
      geom_text(aes(label = sprintf("%+d", round(mean_diff))),
                position = position_nudge(x = -0.5 + TEXT_MARGIN, y = -0.5 + TEXT_MARGIN),
                hjust = 0, vjust = 0, size = 3) +
      scale_x_discrete(labels = lab_strip_char) +
      scale_y_discrete(labels = lab_strip_expr) +
      facet_wrap(~ variable_lab, scales = "free", nrow = 1) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position  = if (show_legend) "right" else "none",
        plot.margin = margin(2, 32, 2, 2)
      )
  }
  
  
  p_top    <- make_row_plot(DT[DT$row=="row1", ], show_legend = TRUE)
  p_bottom <- make_row_plot(DT[DT$row=="row2", ], show_legend = FALSE)
  leg <- cowplot::get_legend(p_top)
  p_top_noleg <- p_top + theme(legend.position = "none")
  
  # Title/subtitle (kept as-is)
  title <- ggdraw() + draw_label(
    bquote("Expected change in counts when replacing a human with the machine"~(n==.(n))),
    fontface = "bold", x = 0.02, hjust = 0, size = 18
  )
  subtitle <- ggdraw() + draw_label(
    bquote("Human–Machine model vs. Human–Human baseline with " * kappa == .(kappa)),
    x = 0.02, hjust = 0, size = 16
  )
  
  
  # Transparent spacer (no panel, no plot background)
  plot_spacer <- 
    function(){
      ggplot() +
        theme_void() +
        theme(
          panel.background = element_rect(fill = NA, color = NA),
          plot.background  = element_rect(fill = NA, color = NA)
        ) }
  stacked_rows <- cowplot::plot_grid(p_top_noleg, p_bottom, ncol = 1, align = "v")
  stacked      <- cowplot::plot_grid(title, subtitle, plot_spacer(), stacked_rows, ncol = 1,
                              rel_heights = c(0.05, 0.05,0.01, 1), align = "v")
  body_plus_leg <- cowplot::plot_grid(stacked, plot_spacer(), leg, ncol = 3,
                                      rel_widths = c(1, 0.01, 0.1), align = "h")

  xlab <- strsplit(contrast_label,'\\_')[[1]][2]
  ylab <- strsplit(contrast_label,'\\_')[[1]][1]
  final <- add_global_axes(body_plus_leg, xlab = xlab, ylab = ylab, left_pad = 0.03, bottom_pad = 0.04)
  
  dir.create("plots", showWarnings = FALSE)
  out_file <- file.path("plots", sprintf("expected_change_heatmaps__TWO_ROWS__%s.pdf",
                                         gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, final, width = 15, height = 8)
  message("Saved: ", out_file)
  final
}

plot_expected_change_state <- function(DT, contrast_label, file_stub, n = 1000L, kappa = 0.8) {
  stopifnot(nrow(DT) > 0, unique(DT$variable) == "state_simple_abbreviation")
  
  # Ensure TOTAL appears first on y
  y_lev <- levels(DT$human); if (is.null(y_lev)) y_lev <- unique(DT$human)
  y_lev <- unique(c("TOTAL", setdiff(as.character(y_lev), "TOTAL")))
  DT$human <- factor(DT$human, levels = y_lev)
  
  max_abs <- max(DT$abs_mean, na.rm = TRUE)
  lab_y_expr <- function(z) parse(text = ifelse(z == "TOTAL", "Sigma", paste0("'", z, "'")))
  
  p <- ggplot(DT, aes(x = rater2, y = human)) +
    geom_tile(aes(fill = abs_mean), color = "grey65", linewidth = 0.25) +
    scale_fill_gradient(low = "springgreen", high = "orangered",
                        limits = c(0, max_abs), name = expression("|"~Delta~"|")) +
    ggnewscale::new_scale_fill() +
    geom_tile(aes(fill = certainty),
              width = MINI_SIZE, height = MINI_SIZE,
              position = position_nudge(x = MINI_OFFSET, y = MINI_OFFSET), color = NA) +
    scale_fill_gradient(name = expression(Pr(Sign)),
                        low = "white", high = "black", limits = c(0, 1),
                        breaks = c(0.70, 0.80, 0.90, 0.97, 0.99),
                        labels = percent_format(accuracy = 1),
                        trans  = power_trans(CERT_POWER)) +
    geom_text(aes(label = sprintf("%+d", round(mean_diff))),
              position = position_nudge(x = -0.5 + TEXT_MARGIN, y = -0.5 + TEXT_MARGIN),
              hjust = 0, vjust = 0, size = 2.7) +
    scale_x_discrete(limits = function(x) rev(x)) +
    scale_y_discrete(labels = lab_y_expr) +
    coord_cartesian(clip = "off") +
    labs(x = strsplit(contrast_label,'\\_')[[1]][2],
         y = strsplit(contrast_label,'\\_')[[1]][1],
         title    = bquote("State: expected change in counts when replacing a human with the machine"~(n==.(n))),
         subtitle = bquote("Human-Machine model vs. Human–Human baseline with " * kappa == .(kappa))) +
    theme_minimal(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  dir.create("plots", showWarnings = FALSE)
  out_file <- file.path("plots", sprintf("expected_change_heatmap__STATE__%s.pdf",
                                         gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, p, width = 13, height = 10)
  message("Saved: ", out_file)
  p
}

for (ctr in c("humans_gpt-3.5-turbo (10 tweets)",
              "humans_gpt-3.5-turbo (5 tweets)")) {
  DT_main <- build_diff_dt(ctr, VARS_MAIN, kappa = 0.8, n = 1000L)
  if (nrow(DT_main) > 0) print(plot_expected_change_faceted(DT_main, ctr, paste0(ctr, "_MAIN_DIFF"), n = 1000L, kappa = 0.8))
  DT_state <- build_diff_dt(ctr, VAR_STATE, kappa = 0.8, n = 1000L)
  if (nrow(DT_state) > 0) print(plot_expected_change_state(DT_state, ctr, paste0(ctr, "_STATE_DIFF"), n = 1000L, kappa = 0.8))
}


# ---------- Simple B.star mean heatmaps ---------- #

# 1) Build tidy table of posterior mean B.star for list index l (keeps your ordering/labels)
bstar_mean_table_for_l <- function(l) {
  jfit <- jags.list[[l]]
  var  <- sub(".*\\___","", names(V.list)[l])
  A    <- jfit$jags.data$A
  rn0  <- rownames(A); cn0 <- colnames(A)
  stopifnot(!is.null(rn0), !is.null(cn0))
  
  sims_B <- jfit$BUGSoutput$sims.list$B.star  # [iter, L, L]
  Bbar   <- apply(sims_B, c(2, 3), mean)      # posterior mean ∈ [0,1]
  dimnames(Bbar) <- list(rn0, cn0)
  
  # Order rows/cols using your display-driven rules
  ord_r  <- order_indices_from_display(rn0, var)
  ord_c  <- order_indices_from_display(cn0, var)
  rn_ord <- rn0[ord_r]; cn_ord <- cn0[ord_c]
  Bbar   <- Bbar[rn_ord, cn_ord, drop = FALSE]
  
  # Tidy with display labels (reverse y/x like your other heatmaps)
  y_levels_raw <- rev(rn_ord)
  x_levels_raw <- rev(cn_ord)
  
  DT <- as.data.table(as.table(Bbar))
  setnames(DT, c("human_raw","rater2_raw","bstar_mean"))
  DT[, human  := factor(human_raw,  levels = y_levels_raw, labels = display_labels(y_levels_raw, var))]
  DT[, rater2 := factor(rater2_raw, levels = x_levels_raw, labels = display_labels(x_levels_raw, var))]
  DT[, is_diag := (as.character(human_raw) == as.character(rater2_raw))]
  DT[, variable     := var]
  DT[, variable_lab := agree_var_label_vec(var)]
  DT[]
}

# 2) Build DT across variables for a given contrast
build_contrast_dt_bstar_mean <- function(target_contrast, variables) {
  rbindlist(lapply(variables, function(v){
    l <- which(sub("\\___.*","", names(V.list)) == target_contrast &
                 sub(".*\\___","", names(V.list)) == v)[1]
    if (is.na(l)) return(NULL)
    bstar_mean_table_for_l(l)
  }), use.names = TRUE)
}

# 3) Two-row faceted plot for main variables (like your Δ and expected-change figures)
plot_contrast_faceted_bstar_mean <- function(DT, contrast_label, file_stub) {
  stopifnot(nrow(DT) > 0)
  
  DT$row <- ifelse(DT$variable %in% ROW1_VARS, "row1",
                   ifelse(DT$variable %in% ROW2_VARS, "row2", NA))
  DT <- DT[!is.na(DT$row), ]
  
  make_row_plot <- function(dsub, show_legend = TRUE) {
    wanted <- if (unique(dsub$row) == "row1") ROW1_VARS else ROW2_VARS
    dsub$variable_lab <- factor(dsub$variable_lab, levels = agree_var_label_vec(wanted))
    
    order_for <- function(vals, var) { vals <- as.character(vals); vals[ order_indices_from_display(vals, var) ] }
    by_var <- split(dsub, dsub$variable)
    
    x_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$rater2), v); paste(v, lv, sep = " :: ")
    }), use.names = FALSE)
    y_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$human), v); paste(v, rev(lv), sep = " :: ")
    }), use.names = FALSE)
    
    dsub$rater2_plot <- factor(paste(dsub$variable, dsub$rater2, sep = " :: "), levels = unique(x_levels_all))
    dsub$human_plot  <- factor(paste(dsub$variable, dsub$human,  sep = " :: "), levels = unique(y_levels_all))
    
    ggplot(dsub, aes(x = rater2_plot, y = human_plot)) +
      geom_tile(aes(fill = bstar_mean), color = "grey65", linewidth = 0.25, show.legend = TRUE) +
      geom_text(aes(label = sprintf("%.2f", bstar_mean)), size = 3) +  # label ALL cells, including diagonal
      scale_fill_gradientn(
        colours = c("dodgerblue", "white", "orangered"),
        values  = scales::rescale(c(0, 0.5, 1)),
        limits  = c(0, 1),
        name    = expression(E(B^"*"[ij]))
      ) +
      scale_x_discrete(labels = function(z) sub("^.* :: ", "", z)) +
      scale_y_discrete(labels = function(z) sub("^.* :: ", "", z)) +
      facet_wrap(~ variable_lab, scales = "free", nrow = 1) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = if (show_legend) "right" else "none",
        plot.margin = margin(2, 32, 2, 2)
      )
  }
  
  p_top    <- make_row_plot(DT[DT$row=="row1", ], show_legend = TRUE)
  p_bottom <- make_row_plot(DT[DT$row=="row2", ], show_legend = FALSE)
  leg <- cowplot::get_legend(p_top)
  p_top_noleg <- p_top + theme(legend.position = "none")
  
  title <- ggdraw() + draw_label(
    bquote("Posterior mean of " * B^"*" * ""),
    fontface = "bold", x = 0.01, hjust = 0, size = 18
  )
  subtitle <- ggdraw() + draw_label(
    paste0(gsub("\\_"," v. ", contrast_label)),
    x = 0.01, hjust = 0, size = 16
  )
  
  plot_spacer <- function(){
    ggplot() + theme_void() +
      theme(panel.background = element_rect(fill = NA, color = NA),
            plot.background  = element_rect(fill = NA, color = NA))
  }
  
  stacked_rows <- cowplot::plot_grid(p_top_noleg, p_bottom, ncol = 1, align = "v")
  stacked      <- cowplot::plot_grid(title, subtitle, plot_spacer(), stacked_rows, ncol = 1,
                                     rel_heights = c(0.05, 0.05, 0.01, 1), align = "v")
  body_plus_leg <- cowplot::plot_grid(stacked, plot_spacer(), leg, ncol = 3,
                                      rel_widths = c(1, 0.01, 0.125), align = "h")
  
  xlab <- strsplit(contrast_label,'\\_')[[1]][2]
  ylab <- strsplit(contrast_label,'\\_')[[1]][1]
  final <- add_global_axes(body_plus_leg, xlab = xlab, ylab = ylab, left_pad = 0.06, bottom_pad = 0.08)
  
  dir.create(OUT_DIR, showWarnings = FALSE)
  out_file <- file.path(OUT_DIR,
                        sprintf("bstar_mean_heatmaps__TWO_ROWS__%s.pdf",
                                gsub("[^A-Za-z0-9]+","_", paste0(contrast_label, "_MAIN_BSTAR_MEAN"))))
  ggsave(out_file, final, width = 15, height = 8)
  message("Saved: ", out_file)
  final
}

# 4) State-only panel (separate figure)
plot_state_single_bstar_mean <- function(DT, contrast_label, file_stub) {
  stopifnot(nrow(DT) > 0, unique(DT$variable) == VAR_STATE)
  
  p <- ggplot(DT, aes(x = rater2, y = human)) +
    geom_tile(aes(fill = bstar_mean), color = "grey65", linewidth = 0.25, show.legend = TRUE) +
    geom_text(aes(label = sprintf("%.2f", bstar_mean)), size = 2.7) +  # label ALL cells, including diagonal
    scale_fill_gradientn(
      colours = c("dodgerblue", "white", "orangered"),
      values  = scales::rescale(c(0, 0.5, 1)),
      limits  = c(0, 1),
      name    = expression(E(B^"*"[ij]))
    ) +
    scale_x_discrete(limits = function(x) rev(x)) +
    coord_cartesian(clip = "off") +
    labs(
      x = strsplit(contrast_label,'\\_')[[1]][2],
      y = strsplit(contrast_label,'\\_')[[1]][1],
      title    = expression("State: posterior mean of " * B^"*"),
      subtitle = paste0(gsub("\\_"," v. ", contrast_label),
                        "")
    ) +
    theme_minimal(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  dir.create(OUT_DIR, showWarnings = FALSE)
  out_file <- file.path(OUT_DIR,
                        sprintf("bstar_mean_heatmap__STATE__%s.pdf",
                                gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, p, width = 13, height = 10)
  message("Saved: ", out_file)
  p
}

# 5) Generate figures for all contrasts
for (ctr in CONTRASTS) {
  # two-row main facets
  DT_main_bm <- build_contrast_dt_bstar_mean(ctr, VARS_MAIN)
  if (nrow(DT_main_bm) > 0) {
    print(plot_contrast_faceted_bstar_mean(DT_main_bm, ctr, paste0(ctr, "_MAIN_BSTAR_MEAN")))
  }
  # state separate
  DT_state_bm <- build_contrast_dt_bstar_mean(ctr, VAR_STATE)
  if (nrow(DT_state_bm) > 0) {
    print(plot_state_single_bstar_mean(DT_state_bm, ctr, paste0(ctr, "_STATE_BSTAR_MEAN")))
  }
}





















# ======== Posterior histograms for the global metric φ ========

# 1) Collect all φ draws into one long data.table
collect_phi_draws <- function() {
  stopifnot(length(jags.list) == length(V.list))
  out_list <- vector("list", length(jags.list))
  for (l in seq_along(jags.list)) {
    variable <- sub(".*\\___",  "", names(V.list)[l])
    contrast <- sub("\\___.*", "", names(V.list)[l])
    # get draws from B.star and convert to φ as in phi_from_bstar()
    Bdraws <- jags.list[[l]]$BUGSoutput$sims.list$B.star
    L <- dim(Bdraws)[2]; stopifnot(dim(Bdraws)[3] == L)
    wtil <- build_tilde_w(L, variable)
    upper <- upper.tri(wtil)
    n_iter <- dim(Bdraws)[1]
    phi <- numeric(n_iter)
    for (t in seq_len(n_iter)) {
      B <- Bdraws[t, , ]
      diagB <- diag(B)
      D <- matrix(0, L, L); for (i in seq_len(L)) D[i, ] <- diagB[i] - B[i, ]
      diag(D) <- 0
      M <- 0.5 * (D + t(D)); diag(M) <- 0
      phi[t] <- sum(wtil[upper] * M[upper])
    }
    out_list[[l]] <- data.table::data.table(
      contrast = contrast,
      variable = variable,
      comp     = comp_label(contrast),
      variable_lab = agree_var_label_vec(variable),
      phi      = phi
    )
  }
  data.table::rbindlist(out_list, use.names = TRUE)
}

DT_phi <- collect_phi_draws()

# Ensure the EXACT column order you want
DT_phi$comp <- factor(
  DT_phi$comp,
  levels = c("Human vs GPT-3.5 (10)", "Human vs GPT-3.5 (5)", "GPT-3.5 (10) vs (5)")
)

# 2) Per-panel summaries (mean, equal-tail 10–90% CI, Pr(>0))
summ_phi <- function(d) {
  data.frame(
    phi_mean = mean(d$phi),
    phi_q10  = as.numeric(quantile(d$phi, 0.10)),
    phi_q90  = as.numeric(quantile(d$phi, 0.90)),
    p_gt0    = mean(d$phi > 0)
  )
}
SUM <- do.call(rbind, by(DT_phi, list(DT_phi$variable_lab, DT_phi$comp), summ_phi))
SUM <- cbind(expand.grid(
  variable_lab = levels(factor(DT_phi$variable_lab)),
  comp = levels(DT_phi$comp)
), SUM)
SUM <- SUM[!is.na(SUM$phi_mean), ]

# --- Order rows by the size of the Human vs GPT-3.5 (10) posterior mean ---
key_comp <- "Human vs GPT-3.5 (10)"

ord_tbl <- subset(SUM, comp == key_comp, 
                  select = c("variable_lab", "phi_mean"))

# If you want largest -> smallest, use decreasing = TRUE
ord_levels <- ord_tbl$variable_lab[order(ord_tbl$phi_mean, decreasing = TRUE)]

# Apply the ordering to both DT and SUM so facets stay aligned
DT_phi$variable_lab  <- factor(DT_phi$variable_lab,  levels = ord_levels)
SUM$variable_lab     <- factor(SUM$variable_lab,     levels = ord_levels)

# 3) Plot function: coloured histogram (fill only), plus top-left Pr(>0)
plot_phi_histograms <- function(DT, SUM, file) {
  # ensure factor/ordering matches the global φ plot
  DT$comp  <- factor(DT$comp,  levels = COMP_LEVELS)
  SUM$comp <- factor(SUM$comp, levels = COMP_LEVELS)
  
  p_body <- ggplot2::ggplot(DT, ggplot2::aes(x = phi)) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = comp),
      bins = 40, color = NA, show.legend = FALSE   # no borders
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_vline(
      data = SUM, ggplot2::aes(xintercept = phi_mean),
      linewidth = 0.6
    ) +
    ggplot2::geom_segment(
      data = SUM,
      ggplot2::aes(x = phi_q10, xend = phi_q90, y = 0, yend = 0),
      linewidth = 2.2, lineend = "butt"
    ) +
    ggplot2::geom_text(
      data = SUM,
      ggplot2::aes(x = -Inf, y = Inf, label = sprintf("Pr(>0)=%.2f", p_gt0)),
      hjust = -0.05, vjust = 1.1, size = 3.8
    ) +
    ggplot2::facet_grid(variable_lab ~ comp, scales = "free_y") +
    ggplot2::labs(
      x = expression(phi),
      y = "n. posterior draws",
      title = expression("Posterior Distribution of Global Agreement Metric " * phi),
      subtitle = "MC Mean                 80% Credible Interval  —"
    ) +
    ggplot2::scale_fill_manual(
      values = COMP_COLS_SOFT,   # match the other φ plot exactly
      limits = COMP_LEVELS,
      breaks = COMP_LEVELS
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, colour = "grey90"),
      strip.background   = ggplot2::element_rect(fill = "grey95", colour = NA),
      axis.text          = ggplot2::element_text(colour = "black")
    )
  
  # ---- draw an actual solid black line next to the subtitle text ----
  # We'll overlay a short horizontal line just after "MC Mean"
  # using cowplot to place vector graphics in the title/subtitle area.
  p_final <- cowplot::ggdraw(p_body) +
    # position is in [0,1] relative to the figure; tweak if needed
    cowplot::draw_line(
      x = c(0.195, 0.245)+0.25,  # horizontal span of the line
      y = c(0.975, 0.975)-0.02,  # vertical position (near subtitle baseline)
      linewidth = 1.3, colour = "black", lineend = "butt"
    ) + 
    cowplot::draw_line(
      x = c(0.195, 0.245)-0.025,  # horizontal span of the line
      y = c(0.975, 0.975)-0.02,  # vertical position (near subtitle baseline)
      linewidth = 0.35, colour = "black", lineend = "butt"
    )
  
  dir.create(OUT_DIR, showWarnings = FALSE)
  ggplot2::ggsave(filename = file.path(OUT_DIR, file), plot = p_final, width = 10, height = 12.5)
  message("Saved: ", file.path(OUT_DIR, file))
  p_final
}

# 4) Make the figure
# One big faceted PDF across all variables+comparisons (columns in the requested order)
print(plot_phi_histograms(DT_phi, SUM, file = "phi_posterior_histograms__ALL.pdf"))




# ======== Δ_bias (human perspective) HEATMAPS ========
# Δ_bias_ij = E[B*_{ij} - B*_{ii}] per posterior draw, averaged; no symmetrisation.

delta_bias_human_table_for_l <- function(l) {
  jfit <- jags.list[[l]]
  var  <- sub(".*\\___","", names(V.list)[l])
  A    <- jfit$jags.data$A
  rn0  <- rownames(A); cn0 <- colnames(A)
  stopifnot(!is.null(rn0), !is.null(cn0))
  
  sims_B <- jfit$BUGSoutput$sims.list$B.star  # [iter, L, L]
  L      <- dim(sims_B)[2]; stopifnot(dim(sims_B)[3] == L)
  n_iter <- dim(sims_B)[1]
  
  # accumulate mean of (B_ij - B_ii), with zeroed diagonal for display
  sum_D <- matrix(0, L, L)
  for (t in seq_len(n_iter)) {
    Bt <- sims_B[t, , ]
    D  <- Bt - diag(Bt)         # <-- bias (away-from-i toward j)
    diag(D) <- 0
    sum_D <- sum_D + D
  }
  Delta <- sum_D / n_iter
  dimnames(Delta) <- list(rn0, cn0)
  
  # order rows/cols using your display-driven rules
  ord_r  <- order_indices_from_display(rn0, var)
  ord_c  <- order_indices_from_display(cn0, var)
  rn_ord <- rn0[ord_r]
  cn_ord <- cn0[ord_c]
  Delta  <- Delta[rn_ord, cn_ord, drop = FALSE]
  
  # tidy: reverse y/x like your other heatmaps; keep raw keys for diagonal flag
  y_levels_raw <- rev(rn_ord)
  x_levels_raw <- rev(cn_ord)
  
  DT <- as.data.table(as.table(Delta))
  setnames(DT, c("human_raw","rater2_raw","delta_bias"))
  DT[, is_diag := (human_raw == rater2_raw)]
  
  DT[, human  := factor(human_raw,  levels = y_levels_raw, labels = display_labels(y_levels_raw, var))]
  DT[, rater2 := factor(rater2_raw, levels = x_levels_raw, labels = display_labels(x_levels_raw, var))]
  DT[, variable     := var]
  DT[, variable_lab := agree_var_label_vec(var)]
  DT[]
}

build_contrast_dt_delta_bias <- function(target_contrast, variables) {
  rbindlist(lapply(variables, function(v){
    l <- which(sub("\\___.*","", names(V.list)) == target_contrast &
                 sub(".*\\___","", names(V.list)) == v)[1]
    if (is.na(l)) return(NULL)
    delta_bias_human_table_for_l(l)
  }), use.names = TRUE)
}

perspective_title <- function(contrast_label) {
  # If both sides are GPT (10 vs 5) and there's no human, switch perspective
  is_llm_vs_llm <-
    grepl("gpt", contrast_label, ignore.case = TRUE) &&
    grepl("10",  contrast_label) &&
    grepl("5",   contrast_label) &&
    !grepl("human|humans", contrast_label, ignore.case = TRUE)
  
  if (is_llm_vs_llm) {
    "Machine Incidence Bias — Large-context LLM Perspective"
  } else {
    "Machine Incidence Bias — Human Perspective"
  }
}

plot_contrast_faceted_delta_bias <- function(DT, contrast_label, file_stub) {
  stopifnot(nrow(DT) > 0)
  DT$row <- ifelse(DT$variable %in% ROW1_VARS, "row1",
                   ifelse(DT$variable %in% ROW2_VARS, "row2", NA))
  DT <- DT[!is.na(DT$row), ]
  
  max_abs <- 1
  lims <- c(-max_abs, max_abs)
  
  make_row_plot <- function(dsub, show_legend = TRUE) {
    wanted <- if (unique(dsub$row) == "row1") ROW1_VARS else ROW2_VARS
    dsub$variable_lab <- factor(dsub$variable_lab, levels = agree_var_label_vec(wanted))
    
    order_for <- function(vals, var) { vals <- as.character(vals); vals[ order_indices_from_display(vals, var) ] }
    by_var <- split(dsub, dsub$variable)
    
    x_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$rater2), v); paste(v, lv, sep = " :: ")
    }), use.names = FALSE)
    y_levels_all <- unlist(lapply(names(by_var), function(v) {
      lv <- order_for(unique(by_var[[v]]$human), v); paste(v, rev(lv), sep = " :: ")
    }), use.names = FALSE)
    
    dsub$rater2_plot <- factor(paste(dsub$variable, dsub$rater2, sep = " :: "), levels = unique(x_levels_all))
    dsub$human_plot  <- factor(paste(dsub$variable, dsub$human,  sep = " :: "), levels = unique(y_levels_all))
    
    d_diag <- dsub[ is_diag == TRUE ]
    d_off  <- dsub[ is_diag == FALSE ]
    
    ggplot() +
      # OFF-DIAGONAL heat
      geom_tile(data = d_off,
                aes(x = rater2_plot, y = human_plot, fill = delta_bias),
                color = "grey65", linewidth = 0.25, show.legend = TRUE) +
      # DIAGONAL tiles: fixed light grey, no text
      geom_tile(data = d_diag,
                aes(x = rater2_plot, y = human_plot),
                fill = "grey90", color = "grey65", linewidth = 0.25, show.legend = FALSE) +
      # Numbers for OFF-DIAGONAL
      geom_text(data = d_off,
                aes(x = rater2_plot, y = human_plot, label = sprintf("%.2f", delta_bias)), size = 3) +
      scale_fill_gradient(
        high = "orangered", low = "springgreen",
        limits = lims,
        name = expression(E(B^"*"[ij] - B^"*"[ii]))  # <-- legend reflects bias
      ) +
      scale_x_discrete(labels = function(z) sub("^.* :: ", "", z)) +
      scale_y_discrete(labels = function(z) sub("^.* :: ", "", z)) +
      facet_wrap(~ variable_lab, scales = "free", nrow = 1) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = if (show_legend) "right" else "none",
        plot.margin = margin(2, 32, 2, 2)
      )
  }
  
  p_top    <- make_row_plot(DT[DT$row=="row1", ], show_legend = TRUE)
  p_bottom <- make_row_plot(DT[DT$row=="row2", ], show_legend = FALSE)
  leg <- cowplot::get_legend(p_top)
  p_top_noleg <- p_top + theme(legend.position = "none")
  
  title <- ggdraw() + draw_label(
    perspective_title(contrast_label),
     x = 0.01, hjust = 0, size = 18
  )
  subtitle <- ggdraw() + draw_label(
    paste0(gsub("\\_"," v. ", contrast_label)),
    x = 0.01, hjust = 0, size = 16
  )
  
  plot_spacer <- function(){
    ggplot() + theme_void() +
      theme(panel.background = element_rect(fill = NA, color = NA),
            plot.background  = element_rect(fill = NA, color = NA))
  }
  
  stacked_rows <- cowplot::plot_grid(p_top_noleg, p_bottom, ncol = 1, align = "v")
  stacked      <- cowplot::plot_grid(title, subtitle, plot_spacer(), stacked_rows, ncol = 1,
                                     rel_heights = c(0.05, 0.05, 0.01, 1), align = "v")
  body_plus_leg <- cowplot::plot_grid(stacked, plot_spacer(), leg, ncol = 3,
                                      rel_widths = c(1, 0.01, 0.125), align = "h")
  
  xlab <- strsplit(contrast_label,'\\_')[[1]][2]
  ylab <- strsplit(contrast_label,'\\_')[[1]][1]
  final <- add_global_axes(body_plus_leg, xlab = xlab, ylab = ylab, left_pad = 0.06, bottom_pad = 0.08)
  
  dir.create(OUT_DIR, showWarnings = FALSE)
  out_file <- file.path(OUT_DIR, sprintf("delta_bias_heatmaps__TWO_ROWS__%s.pdf",
                                         gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, final, width = 15, height = 8)
  message("Saved: ", out_file)
  final
}


plot_state_single_delta_bias <- function(DT, contrast_label, file_stub) {
  stopifnot(nrow(DT) > 0, unique(DT$variable) == VAR_STATE)
  max_abs <- 1
  lims <- c(-max_abs, max_abs)
  
  d_diag <- DT[ is_diag == TRUE ]
  d_off  <- DT[ is_diag == FALSE ]
  
  p <- ggplot() +
    geom_tile(data = d_off,
              aes(x = rater2, y = human, fill = delta_bias),
              color = "grey65", linewidth = 0.25, show.legend = TRUE) +
    geom_tile(data = d_diag,
              aes(x = rater2, y = human),
              fill = "grey90", color = "grey65", linewidth = 0.25, show.legend = FALSE) +
    geom_text(data = d_off,
              aes(x = rater2, y = human, label = sprintf("%.2f", delta_bias)), size = 2.7) +
    scale_fill_gradient(
      high = "orangered", low = "springgreen",
       limits = lims,
      name = expression(E(B^"*"[ij] - B^"*"[ii]))
    ) +
    scale_x_discrete(limits = function(x) rev(x)) +
    coord_cartesian(clip = "off") +
    labs(
      x = strsplit(contrast_label,'\\_')[[1]][2],
      y = strsplit(contrast_label,'\\_')[[1]][1],
      title    = perspective_title(contrast_label),
      subtitle = paste0(gsub("\\_"," v. ", contrast_label))
    ) +
    theme_minimal(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  dir.create(OUT_DIR, showWarnings = FALSE)
  out_file <- file.path(OUT_DIR, sprintf("delta_bias_heatmap__STATE__%s.pdf",
                                         gsub("[^A-Za-z0-9]+","_", file_stub)))
  ggsave(out_file, p, width = 13, height = 10)
  message("Saved: ", out_file)
  p
}

# --- Produce the figures for all three contrasts (main facets + state) ---
for (ctr in CONTRASTS) {
  DT_main_bias <- build_contrast_dt_delta_bias(ctr, VARS_MAIN)
  if (nrow(DT_main_bias) > 0)
    print(plot_contrast_faceted_delta_bias(DT_main_bias, ctr, paste0(ctr, "_MAIN_DELTA_BIAS")))
  DT_state_bias <- build_contrast_dt_delta_bias(ctr, VAR_STATE)
  if (nrow(DT_state_bias) > 0)
    print(plot_state_single_delta_bias(DT_state_bias, ctr, paste0(ctr, "_STATE_DELTA_BIAS")))
}

