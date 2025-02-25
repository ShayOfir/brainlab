source("/shared/code/brainlab.R")
wrap_overlay(behavior_df = "/shared/data/individuation/kinetics_flip_31.5.24.csv", folder = "/shared/data/individuation/flipped/", fn_out = "/shared/data/individuation/overlay_flipped/overlay", subject_column = "pcode", interim = 0, side_column = "Side", which_side = NULL, threshold = 0.99)
