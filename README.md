# RSS-Plots
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
