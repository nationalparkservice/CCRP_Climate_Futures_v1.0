#######################################################
##    COPIES FILES FROM CF REPO INTO LOCAL FOLDER   ###
###             AND DELETES FROM REPO             #####
#######################################################


# Create folder structure locally

# Scripts

scripts_local_dir = paste(local_rss_dir, "scripts", sep = '/')
if(dir.exists(scripts_local_dir) == FALSE){
  dir.create(scripts_local_dir)
}

scripts_repo_dir <- paste(project_root_dir, "scripts", sep = '/')

copyDirectory(from = scripts_repo_dir, to = scripts_local_dir, recursive = TRUE)

# Figures

figs_local_dir = paste(local_rss_dir, "figures", sep = '/')
if(dir.exists(figs_local_dir) == FALSE){
  dir.create(figs_local_dir)
}

figs_repo_dir <- paste(project_root_dir, "figures", sep = '/')

copyDirectory(from = figs_repo_dir, to = figs_local_dir, recursive = TRUE)

# Data

data_local_dir = paste(local_rss_dir, "data", sep = '/') # create local directory for data
if(dir.exists(data_local_dir) == FALSE){
  dir.create(data_local_dir)
}

data_repo_dir <- paste(project_root_dir, "data", sep = '/')

copyDirectory(from = data_repo_dir, to = data_local_dir, recursive = TRUE)

# Remove spatial files

spatial_dir <- paste(data_local_dir, "spatial-data", sep = "/")
general_dir <- paste(data_local_dir, "general", sep = "/")

unlink(spatial_dir, recursive = TRUE)
unlink(general_dir, recursive = TRUE)

# Rmd file

rmd <- list.files(project_root_dir, pattern = '.Rmd') 
file.copy(file.path(project_root_dir, rmd), local_rss_dir)

# Session Info

# Create session info

inputs <- list.files(path = './data/park-specific-data')

sink("sessionInfo.txt")
sessionInfo()
cat(paste("input file:", inputs))
sink()

# copy to local folder and delete

txt <- list.files(project_root_dir, pattern = '.txt')
file.copy(file.path(project_root_dir, txt), local_rss_dir)

# Remove figures and derived data from repo

deriv_data <- paste(data_repo_dir, "derived-data", sep = '/')

unlink(deriv_data, recursive = TRUE)
unlink(figs_repo_dir, recursive = TRUE)
unlink("sessionInfo.txt")