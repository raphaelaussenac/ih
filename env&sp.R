# Delete all objects in the work space
rm(list=ls(all=TRUE))

# set work directory
setwd('/Users/raphaelaussenac/Documents/GitHub/IH')

# load library
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(doParallel)

# load functions
source('./functions.R')

# set seed
set.seed(1245)

################################################################################
# insurance hypothesis under 'standard' climate
################################################################################

# create environment variable and species
df <- envSp(envMu = 20, envSigma = 2, nbSp = 20, minSigma = 0.5, maxSigma = 2, minMu = 5, maxMu = 35)
# mix species together
stand <- mixSp(maxNbSp = 6, df = df, climName = 'standard')
# plot mean = f(div) and sd = f(div)
pl1 <- ggplot(data = stand) +
geom_boxplot(aes(x = nbSp, y = value, group = nbSp)) +
facet_wrap(.~ name) +
theme_bw()
ggsave(file = './figures/meanSdSp.pdf', plot = pl1, width = 20, height = 10)


################################################################################
# insurance hypothesis under climates differing in their variability
################################################################################

# very low variability
df <- envSp(envMu = 10, envSigma = 1.5 * 0.25, nbSp = 20, minSigma = 0.5, maxSigma = 1.5, minMu = 5, maxMu = 15)
standVeryLow <- mixSp(maxNbSp = 6, df = df, climName = '0.25')

# low variability
df <- envSp(envMu = 10, envSigma = 1.5 * 0.75, nbSp = 20, minSigma = 0.5, maxSigma = 1.5, minMu = 5, maxMu = 15)
standLow <- mixSp(maxNbSp = 6, df = df, climName = '0.75')

# high variability
df <- envSp(envMu = 10, envSigma = 1.5 * 2, nbSp = 20, minSigma = 0.5, maxSigma = 1.5, minMu = 5, maxMu = 15)
standHigh <- mixSp(maxNbSp = 6, df = df, climName = '2')

# very high variability
df <- envSp(envMu = 10, envSigma = 1.5 * 4, nbSp = 20, minSigma = 0.5, maxSigma = 1.5, minMu = 5, maxMu = 15)
standVeryHigh <- mixSp(maxNbSp = 6, df = df, climName = '4')

# very very high variability
df <- envSp(envMu = 10, envSigma = 1.5 * 6, nbSp = 20, minSigma = 0.5, maxSigma = 1.5, minMu = 5, maxMu = 15)
standVeryVeryHigh <- mixSp(maxNbSp = 6, df = df, climName = '6')

# rbind
standClim <- rbind(standVeryLow, standLow, stand, standHigh, standVeryHigh, standVeryVeryHigh)
standClim$clim <- factor(standClim$clim, levels = c('0.25', '0.75', 'standard', '2', '4', '6'))

# plot mean = f(div) and sd = f(div)
pl2 <- ggplot(data = standClim) +
geom_boxplot(aes(x = nbSp, y = value, group = nbSp)) +
facet_grid(name ~ clim) +
theme_bw()
ggsave(file = './figures/meanSdClimSp.pdf', plot = pl2, width = 20, height = 10)


################################################################################
# insurance hypothesis under climates differing in their variability
# and with specialist species
################################################################################

# standard
df <- envSp(envMu = 10, envSigma = 1.5, nbSp = 20, minSigma = 0.2, maxSigma = 0.8, minMu = 5, maxMu = 15)
stand <- mixSp(maxNbSp = 6, df = df, climName = 'standard')
# plot mean = f(div) and sd = f(div)
ggplot(data = stand) +
geom_boxplot(aes(x = nbSp, y = value, group = nbSp)) +
facet_wrap(.~ name) +
theme_bw()

# very low variability
df <- envSp(envMu = 10, envSigma = 1.5 * 0.25, nbSp = 20, minSigma = 0.2, maxSigma = 0.8, minMu = 5, maxMu = 15)
standVeryLow <- mixSp(maxNbSp = 6, df = df, climName = '0.25')

# low variability
df <- envSp(envMu = 10, envSigma = 1.5 * 0.75, nbSp = 20, minSigma = 0.2, maxSigma = 0.8, minMu = 5, maxMu = 15)
standLow <- mixSp(maxNbSp = 6, df = df, climName = '0.75')

# high variability
df <- envSp(envMu = 10, envSigma = 1.5 * 2, nbSp = 20, minSigma = 0.2, maxSigma = 0.8, minMu = 5, maxMu = 15)
standHigh <- mixSp(maxNbSp = 6, df = df, climName = '2')

# very high variability
df <- envSp(envMu = 10, envSigma = 1.5 * 4, nbSp = 20, minSigma = 0.2, maxSigma = 0.8, minMu = 5, maxMu = 15)
standVeryHigh <- mixSp(maxNbSp = 6, df = df, climName = '4')

# very very high variability
df <- envSp(envMu = 10, envSigma = 1.5 * 6, nbSp = 20, minSigma = 0.2, maxSigma = 0.8, minMu = 5, maxMu = 15)
standVeryVeryHigh <- mixSp(maxNbSp = 6, df = df, climName = '6')

# rbind
standClim <- rbind(standVeryLow, standLow, stand, standHigh, standVeryHigh, standVeryVeryHigh)
standClim$clim <- factor(standClim$clim, levels = c('0.25', '0.75', 'standard', '2', '4', '6'))

# plot mean = f(div) and sd = f(div)
pl3 <- ggplot(data = standClim) +
geom_boxplot(aes(x = nbSp, y = value, group = nbSp)) +
facet_grid(name ~ clim) +
theme_bw()
pl3
ggsave(file = './figures/meanSdClimSpSpecialist.pdf', plot = pl3, width = 20, height = 10)




################################################################################
standMean <- stand %>% select(-clim) %>% filter(name == 'mean') %>% select(-name) %>% rename(mean = value)
standSd <- stand %>% select(-clim) %>% filter(name == 'sd') %>% select(-name, -nbSp) %>% rename(sd = value)

test <- cbind(standMean, standSd)



ggplot(data = test, aes(x = sd, y = mean)) +
geom_point() +
geom_density_2d_filled(bins = 20, alpha = 0.8) +
facet_wrap(.~nbSp) +
theme_bw()



ggplot(data = test) +
geom_boxplot(aes(x = nbSp, y = sd, group = nbSp, fill = nbSp)) +
theme_bw()
