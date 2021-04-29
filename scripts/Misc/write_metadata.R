##################################################
##    METADATA    ################################
##################################################

# This script prints analysis metadata into the sessionInfo.txt file
# NOTE: Information on Water Balance package version is included in sessionInfo() just like any other package from CRAN (or elsewhere)



sink("sessionInfo.txt") # Create sessionInfo text file
sessionInfo() # add information on package and R versions
cat("")

# Input files

inputs <- list.files(path = './data/park-specific/input') # Input files
cat(paste("input files:", inputs))

# Water Balance model

cat("WATER BALANCE MODEL INFORMATION")
cat(paste("Models used in Water Balance analysis:", WB_GCMs)) 
cat("")

# Drought analysis

cat("DROUGHT ANALYSIS")
cat(paste("SPEI aggregation period (in months):", SPEI_per))
cat(paste("SPEI truncation value:", truncation))
cat(paste("SPEI baseline starts in", SPEI_start), "and ends in", SPEI_end)
cat("")












sink()

# copy to local folder and delete

txt <- list.files(project_root_dir, pattern = '.txt')
file.copy(file.path(project_root_dir, txt), local_rss_dir)



unlink("sessionInfo.txt")