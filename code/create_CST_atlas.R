# script for CST Load project
# This scripts should be ran in brainlab container

# Create a CST atlas with sub-regions
# Option 1: 16 radial slices
# Option 2: Grid
setwd('/shared/')
source('code/brainlab.R')

cat(sprintf('Loading base images...\n'))
img <- rn('Templates_Atlases/mni_icbm152_t1_tal_nlin_asym_09c_bet.nii.gz')   
L <- rn('Templates_Atlases/HCPA/HCPA_CST_Left_MNI.nii')
R <- rn('Templates_Atlases/HCPA/HCPA_CST_Right_MNI.nii')

# zero out the data but keep the dimensions:
img[TRUE] <- 0
radial <- img
grid <- img

# Build Look Up Table (LUT) for the atlas:
#index	Label	Side	Label_NoSide
LUT <- list()
LUT[[1]] <- data.frame(index = 1:2,
                       Label = c('CST_L','CST_R'),
                       Side = c('L', 'R'),
                       Label_NoSide = c('CST', 'CST'))
LUT[[2]] <- data.frame(index = 1:32,
                       Label = c(paste0('sector', 0:15,'_L'), paste0('sector', 0:15,'_R')),
                       Side = rep(c('L', 'R'), each = 16),
                       Label_NoSide = rep(paste0('sector', 0:15),2))


img[L > 0] <- 1 
img[R > 0] <- 2
wn(img, 'Templates_Atlases/HCPA_CST_all')
write.csv(LUT[[1]], 'Templates_Atlases/HCPA_CST_all_LUT.csv', row.names = FALSE)

# Load radial slices and grid masks
radial_masks <- list.files('Templates_Atlases/HCPA/radial_16ths/', full.names = TRUE)
grid_masks <- list.files('Templates_Atlases/HCPA/grid_16ths/', full.names = TRUE)

# Loop through the slices and assign values to the atlas image
for (i in 1:16) {
  cat(sprintf('Processing slice %d...\n', i))
  radial_L <- rn(radial_masks[i])
  radial_R <- rn(radial_masks[i + 16])
  grid_L <- rn(grid_masks[i])
  grid_R <- rn(grid_masks[i + 16])
  
  cat(sprintf('Radial sector %d: %d voxels\n', i, sum(radial_L > 0) + sum(radial_R > 0)))
  radial[radial_L > 0] <- i 
  radial[radial_R > 0] <- i + 16
  grid[grid_L > 0] <- i 
  grid[grid_R > 0] <- i + 16
}

# output the image and LUT:
cat(sprintf('Saving atlas and LUT...\n'))
wn(radial, 'Templates_Atlases/HCPA_CST_radial')
wn(grid, 'Templates_Atlases/HCPA_CST_grid')
write.csv(LUT[[2]], 'Templates_Atlases/HCPA_CST_sectors_LUT.csv', row.names = FALSE)