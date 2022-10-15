# Credits ----------------------------------------------------------------------
'
********************************************************************************
Master Replication file for: 
********************************************************************************      
                          Picking up the Bowling Pins: 
                                A Reproduction of 
                        <<Bowling alone or bowling at all? 
                The effect of unemployment on social participation>>
                            by Kunze and Suppa (2017)
                          
********************************************************************************
*********************************** MASTER.R ***********************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v37, teaching version [DOI: 10.5684/soep.core.v37t])
Purpose:    This R script is the accompanying code to our term paper, which 
            replicates all figures, tables and numbers, attempting to
            reproduce the results obtained by: 
            <<  L. Kunze, N. Suppa (2017) in "Bowling alone or bowling at all? 
                The effect of unemployment on social participation", 
                Journal of Economic Behavior & Organization, 133, 213–235. >>>
            
Content:    This R script is the main R script that calls upon and runs 
            six other R scripts devoted to our anlysis: 
            - GetDataset.R, which retrieves our main data set
            - Descriptive.R, which outputs descriptive statistical analysis (table 1 and appendix B)
            - GetFigure.R, outputs figure 1 of our term paper
            - Imputation.R, which runs our multiple imputation analysis
            - GetMainTable.R reproduces the linear fixed effects regression results by 
              Kunze and Suppa (2007) and outputs table 2 and table 3
            - GetNumbers.R calculates all numbers used within the text of the term paper. 
              It should be studied individually.
            
Notes:      - Please read the supplementary README.pdf file before running this code. 
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
setwd("......... /KunzeSuppa_JEBO2017_ReproductionFiles-main") 




# 2 FURTHER INITIALIZATION -----------------------------------------------------
## 2.1 Clear environment -------------------------------------------------------
rm(list=ls(all=TRUE))         # clear your environment
graphics.off()                # clear console

## 2.2 Install & load packages -------------------------------------------------
# Please run the next 5 lines of code. 
libraries = c("haven", "dplyr", "here", "labelled", "tidyr", "ggplot2", "Hmisc", 
              "stringi", "stringr",  "tidyverse","lmtest", "fixest",  "knitr", 
              "kableExtra","mice", "miceadds", "micemd", "reshape", "xtable") 

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
# Load in GetDataset.R (takes up to 2min)
source(file = file.path(path_code, "GetDataset.R")) 
# We receive one data frame and two lists:
# main_dataset := Main data set as data frame (all filters of Kunze and Suppa (2007) successfully applied)
# subdatasets:= List of 6 sub data sets, one for each dependent variable, obtained by subsetting main_dataset
# subdatasets_cc:= List of 6 sub data sets in complete case format, one for each dependent variable, obtained by subsetting main_dataset
# We save our main_dataset as .dta file to the \data folder.
write_dta(main_dataset, file.path(path_data,"main_dataset.dta"))
# In order to load it in again, please comment out the following line of code:
# main_dataset <- read_dta(file.path(path_data,"main_dataset.dta"))

## 3.2 Descriptive Statistics --------------------------------------------------
# Load in Descriptive.R (takes up to 1min)
source(file = file.path(path_code, "Descriptive.R")) 
# By the next line of code, we save the Comparison of Summary Statistics table 
# as SumStat_Comparison.tex file to our \output\tables folder:
write_file(tex_sumstat, file = paste0(path_tables,'/SumStat_Comparison.tex'))
# By the next 4 lines of code, we save the Summary Statistics table for each data set
# as SumStat_[name of data set].tex file to our \output\tables folder:
for(i in 1:length(sumtables)){
  x <- print(sumtables[[i]], include.rownames=FALSE, caption.placement = 'top', return.string = TRUE)
  write_file(x, file = paste0(path_tables,'/SumStat_',names(sumtables)[i], '.tex'))
}

## 3.3 Create bar plot ---------------------------------------------------------
# Load in GetFigure.R to
source(file = file.path(path_code, "GetFigure.R"))
# By the next 3 lines of code, we save the figure as Barplot.png file to our 
# \output\figure folder:
png(file = paste0(path_figures,'/Barplot.png'), units = "in", 
    width =8, height = 8, res = 1000, bg = "transparent")
barplot
dev.off()


## 3.3 Imputation of missing values  -------------------------------------------
# Load in Imputation.R (takes up to 1h 06min if you wish to re-run the mice algorithm, 
# if not pls use our mice algorithm- output file "imp_long.RData", 
# which is provided in the data folder.)
source(file = file.path(path_code, "Imputation.R")) 
# The lists: estimateModel1_imp and estimateModel2_imp are returned AND needed for 3.4



## 3.4 Create main table -------------------------------------------------------
# GetMainTable.R generates our main tables (table 2 and table 3), which 
# compares the regression results of Kunze and Suppa (2007) to 
# our reproduction regression analysis without imputed missing values (reproduction) 
# and reproduction regression analysis with imputed missing values (extension).
source(file = file.path(path_code, "GetMainTable.R")) # load in Descriptive.R
# We save our Regression outputs tables as .tex documents to \output\tables.
write_file(model1_tableA, file = paste0(path_tables,'/Model1_PanelA.tex'))
write_file(model1_tableB, file = paste0(path_tables,'/Model1_PanelB.tex'))
write_file(model2_tableA, file = paste0(path_tables,'/Model2_PanelA.tex'))
write_file(model2_tableB, file = paste0(path_tables,'/Model2_PanelB.tex'))



## 3.5 Get numbers used in the text --------------------------------------------
# The GetNumbers.R file will not produce any output, which is displayed to the 
# console here, but only inside the .R file. Please open GetNumbers.R to check yourself. 


