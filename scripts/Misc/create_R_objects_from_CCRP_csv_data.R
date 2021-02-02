#################################################
###     CREATE R OBJECTS FROM CCRP .CSV DATA  ###
#################################################

rm(list = ls())

# FLI FBI Table

fli_fbi <- read.csv('./data/raw-data/FLI FBI table.csv')
saveRDS(fli_fbi, file = "./data/general-data/FLI_FBI.Rds")

# D_AET points

d_aet <- read.csv('./data/raw-data/D_AET_points.csv')
saveRDS(d_aet, file = './data/general-data/D_AET.Rds')

# Audubon 

birds <- read.csv('./data/raw-data/Audubon-NPS_study-all_species_trends_filtered.csv')
saveRDS(birds, file = './data/general-data/Audubon.Rds')
