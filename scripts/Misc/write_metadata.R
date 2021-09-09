##################################################
##    METADATA    ################################
##################################################

# This script prints analysis metadata into the sessionInfo.txt file


sink("sessionInfo.txt") # Create sessionInfo text file
sessionInfo() # add information on package and R versions
cat("\n")

# Input files

cat("\n")
inputs <- list.files(path = './data/park-specific/parsed-data') # RData files created from parsed data

cat("input data files:")

for(i in 1:length(inputs)){
  cat(paste0(inputs[i], "; "))
}
cat("\n")

#inputs[1], inputs[2], inputs[3], inputs[4], inputs[5], sep = " "))

# Water Balance model
# NOTE: Information on Water Balance package version is included in sessionInfo() just like any other package from CRAN (or elsewhere)

cat("\n")
cat("WATER BALANCE MODEL INFORMATION")
cat("\n")
cat(paste("Models used in Water Balance analysis:", WB_GCMs[1], WB_GCMs[2])) 
cat("\n")

# Drought analysis

cat("\n")
cat("DROUGHT ANALYSIS")
cat("\n")
cat(paste("SPEI aggregation period (in months):", SPEI_per))
cat("\n")
cat(paste("SPEI truncation value:", truncation))
cat("\n")
cat(paste("SPEI baseline starts in", SPEI_start), "and ends in", SPEI_end)
cat("\n")

# Bias correction text
# NOTE: This will probably be moved to a Methods document at some point

cat("\n")
cat("BIAS CORRECTION METHODS")
cat("\n")
cat(paste("Long-term timeseries plots use PRISM historical data - bias-corrected against gridMET from", BC.min, "to", BC.max, "using the delta method. Projections use MACA data."))

sink()

# copy to local folder and delete

txt <- list.files(project_root_dir, pattern = '.txt')
file.copy(file.path(project_root_dir, txt), local_rss_dir) # If this prints 'FALSE', you may have an existing 'sessionInfo.txt' file in your directory. Remove and re-run. 


unlink("sessionInfo.txt")
