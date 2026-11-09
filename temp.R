source("/shared/code/brainlab.R")
norm.brain(patient.folder = "/shared/data/tms/all/206/", p.fname = "ds.206.x.nii.gz", lesion.fname = "206.lesion.nii.gz", file.format = "nii", atlas.name = "BNA1mm", is.ct = 1, temp.op = "atlas", heal.op = 1, smooth.op = 1.5, deskull = 0, realign = "Rigid", reg_method = "SyN", interim.output = 1)
