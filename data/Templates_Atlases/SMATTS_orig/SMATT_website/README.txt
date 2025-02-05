  ____  __  __    _  _____ _____  
 / ___||  \/  |  / \|_   _|_   _| 
 \___ \| |\/| | / _ \ | |   | |   
  ___) | |  | |/ ___ \| |   | |   
 |____/|_|  |_/_/   \_\_|   |_|   
                                  
If using this template for publication, please cite (article found in folder):

Archer, D.B., Vaillancourt, D.E., Coombes, S.A. A Template and Probabilistic Atlas of the Human Sensorimotor Tracts using Diffusion MRI. Cerebral Cortex, 2017.

####################
###SMATT Template###
####################
This folder contains a total of 13 files.  The below 12 files are binarized files which contain a single tract. Voxels included in each tract are represented with a value of 1. 

Left-M1-SMATT.nii.gz
Left-PMd-SMATT.nii.gz
Left-PMv-SMATT.nii.gz
Left-SMA-SMATT.nii.gz
Left-preSMA-SMATT.nii.gz
Left-S1-SMATT.nii.gz
Right-M1-SMATT.nii.gz
Right-PMd-SMATT.nii.gz
Right-PMv-SMATT.nii.gz
Right-SMA-SMATT.nii.gz
Right-preSMA-SMATT.nii.gz
Right-S1-SMATT.nii.gz

#################################
####SMATT Probabilistic Atlas####
#################################
This is a single file (shown below) which includes all tracts overlaid onto one another.  We have used arbitrary values that range from 1-130, with the value of each voxel corresponding to the combination of tracts which traverse this voxel (shown in the below key).

SMATT.nii.gz

Example 1:  Voxel value = 23
If the voxel value is equal to 23, then this means that the voxel contains Right M1, PMd, PMv, and SMA.  Therefore, there is a 25% chance that this voxel is unique to one of these tracts.  

Example 2: Voxel value = 114
If the voxel value is equal to 114, then this means that the voxel contains Left PMd and preSMA.  Therefore, there is a 50% chance that this voxel is unique to one of these tracts.

KEY
1	Right-M1
2	Right-PMd
3	Right-PMv
4	Right-SMA
5	Right-preSMA
6	Right-S1
7	Right-M1,PMd
8	Right-M1,PMv
9	Right-M1,SMA
10	Right-M1,S1
11	Right-PMd,PMv
12	Right-PMd,SMA
13	Right-PMv,SMA
14	Right-PMd,preSMA
15	Right-SMA,preSMA
16	Right-M1,PMd,SMA
17	Right-M1,PMv,SMA
18	Right-M1,PMv,S1
19	Right-M1,SMA,S1
20	Right-PMd,PMv,SMA
21	Right-M1,PMd,SMA,S1
22	Right-M1,PMv,SMA,S1
23	Right-M1,PMd,PMv,SMA
24	Right-PMd,SMA,preSMA
25	Right-M1,PMd,PMv,SMA,S1
26	Right-M1,PMd,SMA,preSMA
27	Right-PMd,PMv,SMA,preSMA
28	Right-M1,PMd,SMA,preSMA,S1
29	Right-M1,PMd,PMv,SMA,preSMA
30	Right-M1,PMd,PMv,SMA,preSMA,S1
101	Left-M1
102	Left-PMd
103	Left-PMv
104	Left-SMA
105	Left-preSMA
106	Left-S1
107	Left-M1,PMd
108	Left-M1,PMv
109	Left-M1,SMA
110	Left-M1,S1
111	Left-PMd,PMv
112	Left-PMd,SMA
113	Left-PMv,SMA
114	Left-PMd,preSMA
115	Left-SMA,preSMA
116	Left-M1,PMd,SMA
117	Left-M1,PMv,SMA
118	Left-M1,PMv,S1
119	Left-M1,SMA,S1
120	Left-PMd,PMv,SMA
121	Left-M1,PMd,SMA,S1
122	Left-M1,PMv,SMA,S1
123	Left-M1,PMd,PMv,SMA
124	Left-PMd,SMA,preSMA
125	Left-M1,PMd,PMv,SMA,S1
126	Left-M1,PMd,SMA,preSMA
127	Left-PMd,PMv,SMA,preSMA
128	Left-M1,PMd,SMA,preSMA,S1
129	Left-M1,PMd,PMv,SMA,preSMA
130	Left-M1,PMd,PMv,SMA,preSMA,S1