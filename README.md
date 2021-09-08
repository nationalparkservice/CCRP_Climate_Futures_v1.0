

### Instructions for running CCRP_Climate_Futures.Rmd

Maintained by: Climate Change Response Program

*This script was created using R version >= 4.0.2 and RStudio version >= 1.3.1056*
*These instructions assume the user has already connected RStudio to GitHub and has access to the nationalparkservice/CCRP_Climate_Futures repo*

**Step 1. Clone CCRP_Climate_Futures into your local repository folder and connect to RStudio**

    - In RStudio, click on File --> New Project. Select 'Version Control' and then 'Git'.
    - Enter the URL for the repo. Connect using SSH. [CCRP_Climate_Futures] (git@github.com:nationalparkservice/CCRP_Climate_Futures.git)
    - Enter the name of the repo for the Project directory name (CCRP_Climate_Futures)
    - Create the project as a subdirectory of the local folder in which you store your respositories (example: ~/Documents/Repos)
    - Select Create Project. 
    - In the lower right window of RStudio, you should see the files and folders from the repo under the 'Files' tab. 
        - These should be the same files and folders found on 
[CCRP_Climate_Futures](https://github.com/nationalparkservice/CCRP_Climate_Futures)


**Step 2: Leave RStudio and use File Explorer to add spatial data to the './data/general' folder**

*A note on file structure: The notation './' is used to refer to the project root directory. This is where your .RProj file is located.*

*A zipped folder of spatial data is located at [CCRP_Climate_Futures_Data](https://doimspp.sharepoint.com/sites/NPS-CCRP-FCScienceAdaptation/Shared%20Documents/Forms/AllItems.aspx?newTargetListUrl=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents&viewpath=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents%2FForms%2FAllItems%2Easpx&viewid=54c972dc%2D7b2e%2D4eb7%2Da737%2D42792988c0b3&id=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents%2FReproducible%20Climate%20Futures%2Fscript%20rewrites%2Fdata)

File names must be spelled exactly as they are written here or the scripts will not work. The notation '/' refers to a folder with multiple files (i.e. the .shp, .dbf, .prj etc. that comprise an ESRI shapefile)

Spatial data only has to be added one time for all parks in the continental US. Once you add the files to your spatial-data folder, you should not have to do it again. 
    
    - Create the following subfolder within the './data/general' folder:
    
        - spatial-data ('./data/general/spatial-data') 
        
    - Place the following files in the new spatial-data folder:

            - water_storage.tif 
            - elevation_cropped.tif
            - /US_Counties 
            - /State_Shapefile
            - /nps_boundary
            - /nps_boundary_centroids
            - /Climate_grid
            
     - Make sure your new files and folders do not appear beneath the 'Git' tab

**Step 3: Install Java on your machine if you do not already have it. If you have trouble, see document* [Instructions_for_installing_Java_for_use_with_R.docx](https://doimspp.sharepoint.com/:w:/r/sites/NPS-CCRP-FCScienceAdaptation/_layouts/15/Doc.aspx?sourcedoc=%7B2E0D1A3C-7FA7-4E3E-BB4B-7943CCCAD699%7D&file=Instructions_for_installing_Java_for_use_with_R.docx&action=default&mobileredirect=true)

*Note: Java and rJava are only used for creating maps within the get_params script. All analyses can be run without them.*

    - Install Java from the following website: https://java.com/en/download/manual.jsp  
        - Make sure you install the correct version (i.e, 64-bit for 64-bit machines). You want to select the 'offline' version that matches your machine. 
        

**Step 4: Return to RStudio and tell R where to find Java**

    - Enter the following code to point R to the directory into which you installed Java:

            Sys.setenv(JAVA_HOME='path/to/jreX.X.X_XXX')  * check the program file for the jre number.

       * This is an example of where Java might be installed and what the code should look like: 
                Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261')
                
**Step 5: Run the script!**
   
    - Enter park data and all parameters into the user-input chunk in the .Rmd script
    - Run the script! Select Run --> Run All or press Ctrl+Alt+R
    - The script will create all the necessary files and folders from here on out. 


## End instructions for running CCRP_Climate_Futures.Rmd
    

## The following describes the set of functions to parse historical (NOAA and GridMET) data and CMIP5 (MACA) projections, create data tables, and plot visualizations.

**1. Historical trends**

    - Scripts for parsing NOAA and Gridmet data; script for plotting NOAA data
    
**2. Climate Futures**

    - Script for parsing MACA; script for parsin GridMET; Plot table creation; scatterplots and diagnostics; plotting bar charts
    
**3. Water balance (package in own repository)**

    - Daily water balance
    
**4. Summary plots**

    - Summary plots (bias-corrected timeseries for temp and precip)
    - Summary WB (bias-corrected, run WB (monthly) for all GCMs in CFs; plot timeseries)
    
**5. Additional tables and plots** (these include scripts for plots that are regionally specific as well as query of Audubon results).

    - Drought plots - work with any park, but will mostly be used in western parks
    - Forest vulnerability - (FLI FBI plots) - These only cover eastern parks
    - Audubon - most parks included

