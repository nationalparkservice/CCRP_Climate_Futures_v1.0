

### Instructions for running Combine-Climate_Lite_scripts.Rmd

Maintained by: Climate Change Response Program

*This script was created using R version >= 4.0.2 and RStudio version >= 1.3.1056*
*These instructions assume the user has already connected RStudio to GitHub*

**Step 1. Clone CCRP_Climate_Futures_dev into your local repository folder and connect to RStudio**

    - In RStudio, click on File --> New Project. Select 'Version Control' and then 'Git'.
    - Enter the URL for the repo [CCRP_Climate_Futures_dev] (https://github.com/nationalparkservice/CCRP_Climate_Futures_dev.git)
    - Enter the name of the repo for the Project directory name (CCRP_Climate_Futures_dev)
    - Create the project as a subdirectory of the local folder in which you store your respositories (example: ~/Documents/Repos)
    - Select Create Project. 
    - In the lower right window of RStudio, you should see the files and folders from the repo under the 'Files' tab. 
        - These should be the same files and folders found on (https://github.com/nationalparkservice/CCRP_Climate_Futures_dev)

    
**Step 2: Create a new folder for data and make sure GitHub ignores it**

The parsing scripts are not functional in version 1 (due to remote work) so data must be parsed and placed appropriately before running the .Rmd script

*A note on file structure: The notation './' is used to refer to the project root directory. This is where your .RProj file is located.*

    - Create a folders within your repo for data by clicking on the 'New Folder' button at the top left of the lower right window. 
    - Create a new folder called 'data'. If it is incorrectly named the script will not run. 
    - Select the 'Git' tab in the upper right window of RStudio
    - Make sure your new 'data' folder does not appear
    
    IF IT DOES: 
    - Check the white box to 'stage' the new data folder you  have just created
    - Right-click on the blue box beneath the 'status' bar
    - Select 'ignore'
    - Save. Git will now ignore all contents of the '/data' folder including subfolders
    - Contact Annie at anne_dillon@nps.gov or Amber at amber_runyon@nps.gov because this means something is amiss

**Step 4: Leave RStudio and use File Explorer to create data subfolders and add necessary data files**
*To request parsed data, see Amber Runyon at amber_runyon@nps.gov. To request other files see Annie Kellner Dillon (anne_dillon@nps.gov) or Amber Runyon*
*A zipped folder of all data located at [Link](https://doimspp.sharepoint.com/sites/NPS-CCRP-FCScienceAdaptation/Shared%20Documents/Forms/AllItems.aspx?viewid=54c972dc%2D7b2e%2D4eb7%2Da737%2D42792988c0b3&id=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents%2FRCF%2Fscript%20rewrites)

File names must be spelled exactly as they are written here or the scripts will not work. The notation '/' refers to a folder with multiple files (i.e. the .shp, .dbf, .prj etc. that comprise an ESRI shapefile)
    
    - Create the following subfolders within the ./data folder:
        - spatial-data ('./data/spatial-data')
        - RData ('./data/RData')
        - raw-data ('./data/raw-data)
        - derived-data (./data/derived-data')
        
    - Place the following files in folders as follows (PARK is a placeholder for the four-letter park code - i.e., CONG):
        - './data/RData':
            - PARK_init_parsed.RData
            - PARK_PRISM_PptTminTmax_IntermediateFiles.RData (this file is normally created by the PRISM parsing script: './scripts/PRISM/RSS PRISM AN81 4km crop summarize v01.1.R)
        - './data/raw-data: 
            - GridMet.csv
            - FLI FBI table.csv
            - D_AET_points.csv
            - Audubon-NPS_study-all_species_trends_filtered.csv
        - './data/spatial-data
            - water_storage.tif 
            - elevation_cropped.tif
            - /US_Counties 
            - /State_Shapefile
            - /nps_boundary
            - /nps_boundary_centroids
            - /Climate_grid

**Step 5: Install Java on your machine if you do not already have it. If you have trouble, see document Instructions_for_installing_Java_for_use_with_R.docx**

    - Install Java from the following website: https://java.com/en/download/manual.jsp  
        - Make sure you install the correct version (i.e, 64-bit for 64-bit machines). You want to select the 'offline' version that matches your machine. 
        

**Step 6: Return to RStudio and tell R where to find Java**

    - Enter the following code to point R to the directory into which you installed Java:

            Sys.setenv(JAVA_HOME='path/to/jreX.X.X_XXX')  * check the program file for the jre number.

       * This is an example of where Java might be installed and what the code should look like: 
                Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261')
                
**Step 7: Run the script!**

    - Make sure your new files and folders do not appear beneath the 'Git' tab
    - Enter park data into the user-input chunk in the .Rmd script
    - Run the script! Select Run --> Run All or press Ctrl+Alt+R
    - The script will create all the necessary files and folders from here on out. You can find derived data in the ./data/derived-data folder, and figures in the ./figures folder

**Step 8 : Leave RStudio and create a backup copy of repo (optional)**

    - Navigate to the local copy of your repo
    - Compress entire repo into a zip file with the name of the park and the date
    - Store the copy wherever appropriate (e.g., on your local machine, shared drive or cloud)

## End instructions for running Combine-Climate_Lite_scripts.Rmd
    

## The following describes the set of functions to parse historical (PRISM and GridMET) data and CMIP5 (MACA) projections, create data tables, and plot visualizations.

**1. Historical trends (PRISM)**

    - Script for parsing data; script for plotting data
**2. Climate Futures**

    - Script for parsing MACA; script for parsin GridMET; Plot table creation; scatterplots and diagnostics; plotting bar charts
**3. Water balance (package in own repository)**

    - Daily water balance
**4. Summary plots**

    - Summary plots (bias-corrected timeseries for temp and precip)
    - Summary WB (bias-corrected, run WB (monthly) for all GCMs in CFs; plot timeseries)
**5. Additoinal tables and plots** (these include scripts for plots that are regionally specific as well as query of Audubon results).

    - Drought plots - work with any park, but will mostly be used in western parks
    - Forest vulnerability - (FLI FBI plots) - These only cover eastern parks
    - Audubon - most parks included

