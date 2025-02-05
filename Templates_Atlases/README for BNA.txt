###########################################################
# Brainnetome Atlas files description 
# 
# Citation: 
# Lingzhong Fan, Hai Li, Junjie Zhuo, Yu Zhang, Jiaojian Wang, Liangfu Chen, Zhengyi Yang, Congying Chu, # Sangma Xie, Angela R. Laird, Peter T. Fox, Simon B. Eickhoff, Chunshui Yu, and Tianzi Jiang, "The Human # Brainnetome Atlas: A New Brain Atlas Based on Connectional Architecture", Cerebral Cortex, 2016
# doi:10.1093/cercor/bhw157
###########################################################


# BNA_MPM_thr25_1.25mm.nii.gz
> The Maximum Probabilistic Map (MPM) of Brainnetome Atlas, including 246 subregions (210 cortical and 36 subcortical subregions).
> ID: 1~246, odd number for subregions in left hemisphere and even number for those in right hemisphere. 
> Resolution: 1.25mm isotropic, 145*173*145.
> Threshold for the probabilistic maps of subregion: 25%  

# BNA_subregions.xlsx
> Detailed information (name, description, MNI coordinates, .etc) for each subregion

# BNA_matrix_binary_246x246.csv
> Binary undirected adjacency matrix denoting structural connectivity (SC) for 246 subregions
> nonparametric 1-tailed sign test, P<0.001, bonferroni corrected
> value: 1 denotes adjacent, 0 denotes not adjacent

# BNA_PM_4D.nii.gz
> Probabilistic maps (PM) for each subregion in one 4D nii file
> Range: 0~1

# BNA_PM_3D_246.zip
> Zipped file containing 246 3D probabilistic map files
> Range: 0~1

# BNA_SC_4D.nii.gz
> Probabilistic structural connectivity (SC) for each subregion in one 4D nii file
> Range: 0~1

# BNA_SC_3D_246.zip
> Zipped file containing 246 3D probabilistic SC files
> Range: 0~1

# BNA_FC_4D.nii.gz
> Probabilistic functional connectivity (FC) for each subregion in one 4D nii file
> Range: 0~1

# BNA_FC_3D_246.zip
> Zipped file containing 246 3D probabilistic FC files
> Range: 0~1

# HCP40_MNI152_1.25mm.nii.gz
> Template file derived from 40 HCP subjects in MNI152 space

# BN_Atlas_freesurfer.zip
> Brainnetome Atlas cortical surface files in freesurfer and CIFTI format

# lh.BN_Atlas.gcs, rh.BN_Atlas.gcs
> freesurfer format, used to generate Brannetome Atlas annotation (parcellation) for subjects

# BN_Atlas_210_LUT.txt
> color table (look up table) for 210 Brainnetome Atlas cortical subregions in freesurfer format

