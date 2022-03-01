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

# Figures and data
copyDirectory(from = OutDir, to = local_rss_dir, recursive = TRUE)

rmd <- list.files(project_root_dir, pattern = '.Rmd') 
file.copy(file.path(project_root_dir, rmd), local_rss_dir)

# Remove figures and park-specific data from repo
unlink(OutDir, recursive = TRUE)
# f <- list.files(OutDir, include.dirs = F, full.names = T, recursive = T)
# file.remove(f)

