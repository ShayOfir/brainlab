source("/shared/code/brainlab.R")
overlay_lesions(behavior_df = "/shared/data/individuation/kinetics_flip_31.5.24.csv", folder = "/shared/data/individuation/norm.lesions.1mm_flipped/", fn_out = "/shared/data/individuation/overlay_flipped2/overlay", subject_column = "pcode", interim = 0, side_column = "Side", which_side = "L", threshold = 0)
