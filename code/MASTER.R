# Credits ----------------------------------------------------------------------
'
********************************************************************************
                      A joint assignment as part of the module 
                - Applied survey research with the data of the SOEP -
                            by Prof. Dr. Jan Marcus
                                  at FU Berlin
********************************************************************************      

********************************************************************************
*********************************** MASTER.R ***********************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v30, teaching version [DOI: 10.5684/soep.v30])
Purpose:    This R script is the accompanying code to our term paper, which 
            replicates all figures and tables found in term paper with the 
            attempt to reproduce the results obtained by: 
            <<  L. Kunze, N. Suppa (2017) in "Bowling alone or bowling at all? 
                The effect of unemployment on social participation", 
                Journal of Economic Behavior & Organization, 133, 213–235. >>>
            
Content:    This R script is the main R script that calls upon and runs 
            four other R scripts devoted to our anlysis: 
            - GetDataset.R, which retrieves the data and generates 6 main data sets 
            - Descriptiv.R, which runs and outputs descriptive statistical analysis
            - Regression.R, which runs and outputs regression analysis
            - Imputation.R, which runs and outputs multiple imputation results. 
            
Notes:      - Please read the supplementary README.pdf file before running the code. 
            - Please execude the code chronologically. 
              (To expand the document outline, click on the symbol with grey lines 
              next the "Source" button on the header of the R editor or use the 
              shortcut SHIFT+ALT+O (for Mac SHIFT+COMMAND+O).)
            - Please make sure to read the comments of the chapter 1 (MANUAL SETUP) 
              carefully before running the remaining code of MASTER.R. 
' 

# 1 MANUAL SETUP ---------------------------------------------------------------
# 1.1 Check your R version -----------------------------------------------------
R.Version()$version.string
# If your R version is < version 4.1.2, please update your R by uncommenting this 
# next two lines of code: 
# require(installr) 
# updateR()

# 1.2 Define your working directory  -------------------------------------------
# Please follow the description in the SetWD.pdf file to manually complete 
# the command "setwd(..)" by filling in the path to 
# folder "KunzeSuppa_JEBO2017_ReplicationFiles" for ".........": 
setwd("......... /KunzeSuppa_JEBO2017_ReplicationFiles") 




# 2 FURTHER INITIALIZATION -----------------------------------------------------
## 2.1 Clear environment -------------------------------------------------------
rm(list=ls(all=TRUE))         # clear your environment
graphics.off()                # clear console

## 2.2 Install & load packages -------------------------------------------------
# Please run the next 5 lines of code. 
libraries = c("haven", "dplyr", "here", "labelled", "tidyr", "ggplot2", "Hmisc", 
              "stringi", "stargazer", "lubridate", "todor", "stringr", "fixest")
lapply(libraries, function(x) if (!(x %in% installed.packages())) { 
  install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## 2.3 Define other data folders here -------------------------------------------
# Please do not change. Run the code. 
path_data <- here::here("data")
path_tables <- here::here("output/tables")
path_figures <- here::here("output/figures")
path_code <- here::here("code/")




# 3 RUN SUB SCRIPTS -----------------------------------------------------------
## 3.1 Retrieve data and generate main data set --------------------------------
# The execution of the following code may take up to 1 min. 
source(file = file.path(path_code, "GetDataset.R")) # load in GetDataset.R
# Since eachdependent variables implies a different sample, we receive 6 main 
# data sets, all bundled in "datasets". We save each sub data set to our 
# \data\ folder. 
for (n in names(datasets)) write_dta(datasets[[n]], 
                                     sprintf(file.path(path_data,"%s.dta"), n))

## 3.2 Descriptive Statistics --------------------------------------------------
# The Summary Statistics is generated and saved to our \output\tables folder. 
source(file = file.path(path_code, "Descriptive.R")) # load in Descriptive.R





