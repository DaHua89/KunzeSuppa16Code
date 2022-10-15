# Replication package

This repository includes the accompanying code to our term paper

**Picking up the Bowling Pins: A Reproduction of 
“Bowling alone or bowling at all? The effect of unemployment on social participation” 
by Kunze and Suppa (2017)**
  
the goal of which is the reproduction of the results obtained by 
L. Kunze, N. Suppa (2017) in "Bowling alone or bowling at all? The effect of unemployment on social participation", 
*[Journal of Economic Behavior & Organization](https://doi.org/10.1016/j.jebo.2016.11.012)*, 133, 213–235.>>

## 1. Data 
The data set was retrieved from the German Socio-Economic Panel Study (SOEP) data base, provided by the German Institute of Economic Research (DIW Berlin). Due to data protection regulations, the data cannot be made publicly available, however, the data are available for scientific research free of charge and any researcher can apply for access of the data. Please contact the SOEP department at DIW Berlin for enquiries. The article used version 37 of the teaching edition, DOI: 10.5684/soep.core.v37t. 


## 2. Software requirements
The research was conducted using R, version 4.2.1. In addition, a number of additional packages were used: 	haven, dplyr, here, labelled, tidyr, ggplot2, Hmisc, stringi, stringr, tidyverse, lmtest, fixest, knitr, kableExtra, mice, miceadds, micemd, reshape, xtable


## 3. Instructions and Content 
1. Download the repository by clicking on the green “Code” button and choose “Download ZIP”. Unzip the downloaded folder and choose a location for it on your computer. The file includes two folders: data and code. The folder code contains of six .R files:
  - **MASTER.R** , which compactly runs all files needed for the analysis. It load all external packages, defines paths and creates the (sub)folders output/figures and output/tables to save all replication output in. 
  - **GetDataset.R** retrieves the main data set from the data folder.
  - **Descriptive.R** outputs descriptive statistical analysis (table 1 and appendix B).
  - **GetFigure.R** outputs figure 1.
  - **Imputation.R** runs multiple imputation analysis .
  - **GetMainTable.R** reproduces linear fixed effects regression results by Kunze and Suppa (2007) and outputs table 2 and table 3.
  - **GetNumber.R** calculates all numbers used within the text. It should be studies individually.

2. Please drag the folder **“Stata”**, provided by DIW into the *data* folder. Make sure the folder “Stata” becomes a **sub**folder of the *data* folder.
3. The replication of the imputation of missing values, done via Imputation.R, is quite a compute-intensive task, which can take up to 75min to finally output the data. Due to possible time constraints of the reader, we chose to provide the imputed data set, **imp_long.RData** file, placed in the *data* folder. With replacing or removing the file from the data folder, the reader chooses to re-run the imputation algorithm, for which about 75min must be taken into account. 
4. Open *Master.R* in e.g. R Studio. Expand the document outline by clicking on the symbol with grey lines next the “Source" button on the header of the R Studio window or by using the shortcut SHIFT+ALT+O (for Mac SHIFT+COMMAND+O). 
5. Go to Section 1.1 in *Master.R*: Check — and if required, update— your R version to the lastest released version 4.2.1. 
6. Go to 1.2 in *Master.R*: Set up your working directory to the downloaded **KunzeSuppa_JEBO2017_ReproductionFiles-main** folder. For help with the setup of your working directory, we suggest the guidelines provided by [Nathaniel D.Philips](https://bookdown.org/ndphillips/YaRrr/the-working-directory.html).
7. Run Section 2 of the *Master.R* file. 
8. Run Section 3 of the *Master.R* line by line. Please note the comments for further assistance. 
9. Check the subfolders *data\tables* and *data\figures* for the generated outputs. 


## Authors
T. Bethge <br>
Humboldt University of Berlin 

I. Fetzer   <br>
Humboldt University of Berlin  <br>
fetzeris@hu-berlin.de

M. Paul  <br>
Humboldt University of Berlin 


  
  
  
  



