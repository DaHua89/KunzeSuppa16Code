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
# If your R version is < version 4.2.1, please update your R by uncommenting 
# the following code: 
# for Windows: 
# require(installr) 
# updateR()
# for Mac: 
# require(updateR) 
# updateR()

# 1.2 Define your working directory  -------------------------------------------
# Please follow the description in the SetWD.pdf file to manually complete 
# the command "setwd(..)" by filling in the path to 
# folder "KunzeSuppa_JEBO2017_ReplicationFiles" for ".........": 
setwd("~/Downloads/KunzeSuppa_JEBO2017_ReplicationFiles")
# setwd("......... /KunzeSuppa_JEBO2017_ReplicationFiles") 




# 2 FURTHER INITIALIZATION -----------------------------------------------------
## 2.1 Clear environment -------------------------------------------------------
rm(list=ls(all=TRUE))         # clear your environment
graphics.off()                # clear console

## 2.2 Install & load packages -------------------------------------------------
# Please run the next 5 lines of code. 
libraries = c("haven", "dplyr", "here", "labelled", "tidyr", "ggplot2", "Hmisc", 
              "stringi", "stringr",  "tidyverse","lmtest", "fixest",  "knitr", 
              "kableExtra","mice", "miceadds", "micemd") # lubridate, "sandwich" "texreg","xtable",

lapply(libraries, function(x) if (!(x %in% installed.packages())) { 
  install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## 2.3 Define other data folders here ------------------------------------------
# Please do not change. Just run the code. 
path_data <- here::here(getwd(),"data")
path_tables <- here::here(getwd(), "output/tables")
path_figures <- here::here(getwd(), "output/figures")
path_code <- here::here(getwd(),"code")
path_output <- here::here(getwd(),"output/")

## 2.4 Create subfolders -------------------------------------------------------
# Please do not change. Just run the code. 
if(dir.exists(path_output)!=TRUE){
  dir.create(file.path(getwd(), "output"))
}
if(dir.exists(path_tables)!=TRUE){ 
  dir.create(file.path(getwd(), "output", paste0("tables")))
}
if(dir.exists(path_figures)!=TRUE){
  dir.create(file.path(getwd(), "output", paste0("figures")))
}
if(dir.exists(path_data)!=TRUE){
  dir.create(file.path(getwd(), "data"))
}
       



# 3 RUN SUB SCRIPTS -----------------------------------------------------------
## 3.1 Retrieve data and generate main data set --------------------------------
# The execution of the following code may take up to 1 min. 
source(file = file.path(path_code, "GetDataset.R")) # load in GetDataset.R
# We receive two datasets:
# data_all := Not filtered for years, includes all years from the PL data set
# data_main:= Filtered for survey years 1991 to 2011, as applied in Kunze & Suppa (2007) 
# We save each data set to the \data folder.
write_dta(our_dataset, file.path(path_data,"our_dataset.dta"))

## 3.2 Descriptive Statistics --------------------------------------------------
# The Summary Statistics is generated and saved to our \output\tables folder. 
source(file = file.path(path_code, "Descriptive.R")) # load in Descriptive.R
# We save our Summary Statistics table as .tex document to \output\tables.
write_file(tex_sumstat, file = paste0(path_tables,'/SummaryStatistics.tex'))

## 3.3 Imputation of missing values  -------------------------------------------
source(file = file.path(path_code, "Imputation.R")) # load in Descriptive.R
# The lists: estimateModel1_imp and estimateModel2_imp are returned AND needed for 3.4

## 4.4 Create main table -------------------------------------------
# This table lists and compares the regression results of Kunze and Suppa (2007), 
# our reproduction regression analysis without imputed missing values (reproduction) 
# and reproduction regression analysis with imputed missing values (extension).
source(file = file.path(path_code, "GetMainTable.R")) # load in Descriptive.R
# We save our Summary of Regression outputs table as .tex document to \output\tables.
write_file(model1_tableA, file = paste0(path_tables,'/Model1_PanelA.tex'))
write_file(model1_tableB, file = paste0(path_tables,'/Model1_PanelB.tex'))
write_file(model2_tableA, file = paste0(path_tables,'/Model2_PanelA.tex'))
write_file(model2_tableB, file = paste0(path_tables,'/Model2_PanelB.tex'))


