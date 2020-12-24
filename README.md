### Instructions for running Combine-Climate_Lite_scripts.Rmd

*Please use R version >= 4.0.2 and RStudio version >= 1.3.1056*

**Step 1. Clone CCRP_Climate_Futures_dev into your local repository folder and connect to RStudio**

    - Go to [GitHub] (https://github.com) and sign in using your credentials
    - Navigate to [nationalparkservice/CCRP_Climate_Futures_dev] (https://github.com/nationalparkservice/CCRP_Climate_Futures_dev)
    - Select the green 'code' button and copy the HTTPS website to your clipboard
    - In RStudio, click on File --> New Project. Select 
    




The parsing scripts are not functional during remote work, so please begin the Climate Lite process with already-parsed data. The following files will need to be copied into the following folders:

*./data/RData*



Set of functions to parse historical (PRISM and GridMET) data and CMIP5 (MACA) projections, create data tables, and plot visualizations.

## Steps of analysis are in different folders:
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

Create branch for conducting analyses on a new park. If make generalizable improvements to script (e.g., bias correction improvements, new plots), commit to master. If embellishments that are specific or one-off, add to Variations repo. 
