#Prepare_SMATT_LL.R

source('/shared/code/brainlab.R')

# Load the SMATT atlases
SMATTS <- load.SMATTS(SMATTS.path = '/shared/Templates/SMATTS_1mm/')

SM <- SMATTS.to.atlas(SMATTS, side = c('Left','Right'))

cohort <- read.csv('/shared/data/individuation/pre_kinematics_2.4.26.csv')



