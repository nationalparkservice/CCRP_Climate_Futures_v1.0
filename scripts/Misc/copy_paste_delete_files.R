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

data_local_dir = paste(local_rss_dir, "data/", sep = '/') # create local directory for data
if(dir.exists(data_local_dir) == FALSE){
  dir.create(data_local_dir)
}

data_repo_dir <- paste(project_root_dir, "data", sep = '/')

copyDirectory(from = data_repo_dir, to = data_local_dir, recursive = TRUE)

# Remove spatial files from local folder

general_dir <- paste(data_local_dir, "general", sep = "/")
unlink(general_dir, recursive = TRUE)

# Rmd file

rmd <- list.files(project_root_dir, pattern = '.Rmd') 
file.copy(file.path(project_root_dir, rmd), local_rss_dir)

# Remove figures and park-specific data from repo

park_specific_data_repo_dir <- paste(data_repo_dir, "park-specific", sep = '/')

unlink(park_specific_data_repo_dir, recursive = TRUE)
unlink(figs_repo_dir, recursive = TRUE)
