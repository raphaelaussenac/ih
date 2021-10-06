################################################################################
# create environment variable and species
################################################################################

envSp <- function(envMu, envSigma, nbSp, minSigma, maxSigma, minMu, maxMu){
  # create env variable
  df <- data.frame(env = rnorm(500, envMu, envSigma))

  # create species
  sigmas <- runif(nbSp, minSigma, maxSigma)
  mus <- runif(nbSp, minMu, maxMu)
  spNb <- 1
  for (i in 1:length(sigmas)){
    sp <- data.frame(sp = ( 1 / (sigmas[i] * sqrt(2 * pi)) ) * exp( -(1/2) * ( (df$env - mus[i]) / sigmas[i] ) ^2 ))
    colnames(sp) <- paste0(colnames(sp), spNb)
    df <- cbind(df, sp)
    spNb <- spNb + 1
  }

  # plot environmental variable distribution
  curve(( 1 / (envSigma * sqrt(2 * pi)) ) * exp( -(1/2) * ( (x - envMu) / envSigma ) ^2 ), from = 0, to = 18, ylim = c(0, 0.8), lwd = 2.5)

  # add sp response function
  for (i in 1:length(sigmas)){
    curve(( 1 / (sigmas[i] * sqrt(2 * pi)) ) * exp( -(1/2) * ( (x - mus[i]) / sigmas[i] ) ^2 ), from = 0, to = 18, add = TRUE, col = i)
    # print(i)
  }

  return(df)

}


################################################################################
# mix species
################################################################################

mixSp <- function(maxNbSp, df, climName){

  # wide to long
  df <- df %>% mutate(timeStep = c(1:nrow(df))) %>% pivot_longer(cols = starts_with('sp'), names_to = 'sp', values_to = 'growth')

  # create list of sp combinations
  mix <- list()
  for (i in 1:maxNbSp){
    mix1 <- combn(unique(df$sp), i, simplify = FALSE)
    mix <- c(mix, mix1)
  }

  # calculate stands mean(prod) and sd(prod)
  mixStand <- function(df, i){
    # select species
    test <- df %>% dplyr::filter(sp %in% mix[[i]])
    # calculate stand productivity
    test <- test %>% pivot_wider(names_from = sp, values_from = growth) %>% select(-env, -timeStep)
    test$stand <- rowSums(test) / length(mix[[i]])
    # calculate mean and sd of productivity
    bla <- data.frame(mean = mean(test$stand), sd = sd(test$stand), nbSp = length(mix[[i]]))
    return(bla)
  }

  # set cluster
  cl <- makeCluster(3)
  registerDoParallel(cl)
  stand <- foreach(i = 1:length(mix), .combine = 'rbind', .packages = c('dplyr', 'tidyr')) %dopar% {mixStand(i = i, df = df)}
  stopCluster(cl)

  stand <- stand %>% pivot_longer(cols = c('mean', 'sd')) %>% mutate(clim = climName)

  return(stand)

}
