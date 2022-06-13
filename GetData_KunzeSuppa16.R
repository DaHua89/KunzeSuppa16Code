# Credits  ---------------------------------------------------------------------
'                    A joint assignment as part of the module 
             - Applied survey research with the data of the SOEP -
                            by Prof. Dr. Jan Marcus
                                at FU Berlin
                          
                      R Script to replicate the paper: 
  L. Kunze, N. Suppa (2017): "Bowling alone or bowling at all? The effect of
  unemployment on social participation", in: Journal of Economic Behavior &
  Organization, 133, pp. 213-235, https://doi.org/10.1016/j.jebo.2016.11.012.
         
                               written by 
                              (our names!)
' 


# 1 Initialization -------------------------------------------------------------- 
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
# setwd("~/Downloads/Stata/") # Isabell 
setwd("C:/Users/User/Documents/SOEP-CORE.v37teaching_STATA/Stata/") # Till 
# setwd() # Max
# install & load packages
libraries = c("haven", "dplyr", "labelled", "tidyr", "ggplot2", "Hmisc", 
              "stringi", "stargazer", "lubridate", "todor")
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{ install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



# 2 Variable selection: Regressands and Regressors ----------------------------
## 2.1 Regressands ------------------------------------------------------------
# Questionnaire can be found here: 
# https://paneldata.org/soep-core/inst/soep-core-2011-pe/3 
# a) CULTURE:   pli0090_h  (pl dataset; more info at: https://paneldata.org/soep-core/data/pl/pli0090_h)
# b) CINEMA:    pli0091_h  (pl dataset, more info at: https://paneldata.org/soep-core/data/pl/pli0091_h)
# c) SPORTS:    pli0092_h  (pl dataset, more info at: https://paneldata.org/soep-core/data/pl/pli0092_h)
# d) social: pli0094_h  (pl dataset, more info at: https://paneldata.org/soep-core/data/pl/pli0094_h)
# e) help:   pli0095_h  (pl dataset, more info at: https://paneldata.org/soep-core/data/pl/pli0095_h)
# f) VOLUNTEER: pli0096_h  (pl dataset, more info at: https://paneldata.org/soep-core/data/pl/pli0096_h)

# Further optional variables: 
# Involvement in a citizens' group, political party, local government: pli0097_h (pl)
# Attending church, religious events: pli0098_h (pl)
# Artistic or musical activities: pli0093_h (pl)

## 2.2 Regressors --------------------------------------------------------------
# 1) EMPLOYED (EP): pgemplst (pgen dataset, more info at https://paneldata.org/soep-core/data/pgen/pgemplst)
# 2) UNEMPLOYED (UE): derived as 1-OLF-EMPLOYED
# 3) OUT OF LABOUR FORCE (OLF): pglfs (pgen dataset, more info at https://paneldata.org/soep-core/data/pgen/pglfs)
# 4) PLANT CLOSER UNEMPLOYED (UEPC): plb0304_h (pl dataset, more info at: https://paneldata.org/soep-core/data/pl/plb0304_h)
# 5) OTHER UNEMPLOYED (UEO): derived as UNEMPLOYED - PLANT CLOSER UNEMPLOYED
# 6) AGE: 'gebjahr' - 'syear', whereas 'gebjahr' (ppath dataset; https://paneldata.org/soep-core/data/ppath/gebjahr)
# 7) YEARS OF EDUCATION: (NICHT: lb0187 (biol))
# 8) WORK DISABILITY:
# 9) MARRIED:  pgfamstd (pgen dataset; https://paneldata.org/soep-is/data/pgen/pgfamstd)
# 10) NUMBER OF CHILDREN: 
# 11) SHOCK SPOUSE DIED: 
# 12) SHOCK CHILD BORN: 
# 13) SHOCK DIVORCE OR SEPERATED:
# 14) WEST GERMANY: (NICHT: l11102 (pequiv)) 
# 15) PERSON NEEDING CARE IN HH: hlf0291 (hl dataset; https://paneldata.org/soep-core/data/hl/hlf0291)


## 2.3 Others ------------------------------------------------------------------
## Identifier/Merging keys
# pid:= Never Changing Person ID
# hid:= Current Wave HH Number
# cid:= Original Houshold Number, Case ID
# syear:= Survey year
## Other useful variables from ppath dataset: 
# netto:= net variable with information on the interview type 
# phrf:= weighting variable 



# 3 Load in variables from different SEOP-Core datasets  ----------------------
PPATHL<- read_dta(file = file.path('ppathl.dta'), 
                  col_select = c("pid", "hid", "cid", "syear", "netto", "phrf",  # keys and other variables 
                                 "gebjahr")) # age (6)

PL <- read_dta(file = file.path('pl.dta'), 
               col_select = c('pid','cid', 'hid', 'syear', # keys
                              "pli0090_h", # culture (a)
                              "pli0091_h", # cinema (b)
                              "pli0092_h", # sports (c)
                              "pli0094_h", # social (d)
                              "pli0095_h", # help (e)
                              "pli0096_h", # volunteer (f)
                              'plb0304_h')) # UEPC (4)

PGEN <- read_dta(file = file.path('pgen.dta'), 
                   col_select = c('pid', 'cid','hid', 'syear',   
                                  "pgfamstd", # married (9) 
                                  'pgemplst', # EP (1)
                                  'pglfs')) # OLF (3)    

HL <- read_dta(file = file.path('hl.dta'), 
                 col_select = c('cid','hid', 'syear',   
                                "hlf0291" ))  # person needing care in hh (15) 


# 4 Load sub- functions -------------------------------------------------------
# The renaming() sub-function takes a dataframe as its only argument. 
# It takes care of renaming the variables of the input dataframe. 
# The function returns a modified dataframe. 
renaming <- function(df){
  # renaming variables
  var_names <- c( culture =  "pli0090_h", 
                  cinema = "pli0091_h",  
                  sports = "pli0092_h", 
                  social = "pli0094_h", 
                  help = "pli0095_h", 
                  volunteer = "pli0096_h", 
                  # west = "", 
                  married = "pgfamstd", 
                  #yearsedu = "", 
                  needcare = "hlf0291",
                  EP = 'pgemplst',
                  OLF = 'pglfs',
                  UEPC = 'plb0304_h') 
                  # children = ""
  df <- df %>% rename(any_of(var_names))
  return(df)
}

# The labeling() sub-function takes a dataframe as its only argument. 
# It takes care of labeling the variables of the input dataframe. 
# It requires that the renaming() subfunction has been called on the data frame 
# before it is used!!! The function returns a modified dataframe. 
labeling <- function(df){
var_labels <- c(culture =  "1:=never, 2:=less frequently, 3:=monthly, 4:= weekly", 
                  cinema = "1:=never, 2:=less frequently, 3:=monthly, 4:= weekly", 
                  sports = "1:=never, 2:=less frequently, 3:=monthly, 4:= weekly", 
                  social = "1:=never, 2:=less frequently, 3:=monthly, 4:= weekly", 
                  help = "1:=never, 2:=less frequently, 3:=monthly, 4:= weekly", 
                  volunteer = "1:=never, 2:=less frequently, 3:=monthly, 4:= weekly", 
                  west ="1:= West Germany, 2:=East Germany" )
  label(df) <-  as.list(var_labels[match(names(df), names(var_labels))])
}

# The recoding() sub-function takes a dataframe as its only argument. 
# It recodes the values of the variables "culture", "cinema", "sports", "social", 
# "help" and "volunteer": 
# 2(weekly) becomes 4, 
# 3(monthly) stays 3, 
# 4(less frequently) becomes 2, 
# 5(never) becomes 1, and 
# defines negatives values as missing values. 
# The function returns a modified dataframe. 
recoding <- function(df){
  df <- df %>% 
    
    # Modifications on our 6 main variables 
    mutate_at(c("culture", "cinema", "sports", "social", "help", "volunteer"),
                         funs(recode(., '2'=4, '3'=3, '4'=2, '5'=1))) %>% # recode all variables' (.) values, whereas 2 becomes 4, 4 becomes 2 and 5 becomes 1 
    mutate_at(c("culture", "cinema", "sports", "social", "help", "volunteer"),
              funs(ifelse(.<0, NA, .))) %>% # recodes all variables (.) to NA, if they take values below zero (.<0)
    
    # Modifications on our regressors 
    mutate(married = replace(married, married>1, 0),  # Yes(1); Otherwise (0)
           married = replace(married, married < 0, NA), # define negative values as missing values 
           #yearsedu = replace(yearsedu, yearsedu <0, NA), # define negative values as missing values 
           needcare = replace(needcare, needcare <0, NA), # define negative values as missing values 
           needcare = replace(needcare, needcare>1, 0)) %>% # Yes(1); Otherwise (0)
    #children = replace(children, children <0, NA), 
    #children0 = children ==0, 
    #children1 = children ==1, 
    #children2 = children == 2, 
    #children3plus = children > 2)
    mutate(UEPC = as.numeric(EP %in% c(5) & UEPC %in% c(1)), # UEPC variable construction
           EP = as.numeric(EP %in% c(1, 2, 3, 4, 6)), # EP variable construction
         OLF = as.numeric(OLF %in% c(1:5, 7:10, 13))) %>% # OLF variable construction
    mutate(UE = 1-OLF-EP) %>% # UE variable construction
    mutate(UEO = UE-UEPC) %>% # UEO variable construction
    mutate(UEO = replace(UEO, OLF==1, 0))
  
  return(df)
  
}


# 5 Merge to universal dataset ------------------------------------------------
universal <- PPATHL%>% 
  left_join(PL) %>%
  left_join(PGEN) %>%
  left_join(HL) %>%
  arrange(pid, syear, hid) # order



# 6 Apply filters   -----------------------------------------------------------
data_all <- universal %>% 
  drop_na(syear, pid) %>% # exclude all observations with missing values in syear and pid
  mutate( gebjahr = replace(gebjahr, gebjahr<0, NA), # convert negative gebjahr to missing values 
          age = case_when(!is.na(gebjahr) ~ syear - gebjahr), # define age variable as:= sampleyear(year) - year born(gebjahr)
          age = replace (age, age<0, NA)) %>%  # exclude values where syear > gebjahr 
  filter(age>=21, age<=64,   # (1) Filter: Age between 21 and 64
         #l11102 %in% c(1,2), # (2) Filter: Place of living is Germany (either West:1 or East:2)  
         syear %in% c(1991:1998, 2001:2011), # (3) Filter: data from 1991 to 2011 (without 1999 and 2000)
         syear %in% c(1992, 1994, 1996,    # (4) Include only the waves as stated on p.218, Table1 
                        1997, 2001, 2005, 
                        2007, 2009, 2011))



# 7 Data cleaning -------------------------------------------------------------
## 7.1 Rename and recode main variables ----------------------------------------
data_all <- data_all %>% renaming() %>% recoding() 



## 7.2 Subsetting dataset ------------------------------------------------------
datasets_mainvar <- list()
main_vars <- c("culture", "cinema", "sports", "social", "help", "volunteer")
for (i in 1:length(main_vars)){
  currentvar <- main_vars[i] # current main variable (e.g main_vars[1] = "culture", main_vars[2] = "cinema" ...)
  allothermainvars <- main_vars[-(which(main_vars == currentvar))] # all other main variables except the current one 
  df_crop <- data_all %>% 
    select(!allothermainvars) %>%  # exclude all other main variables (e.g. for dataset "culture" exclude: "cinema", "sports", "social", "help" and "volunteer" )
    drop_na(currentvar) # exclude all missing values of the current main variable 
  datasets_mainvar[[i]] <- df_crop # add to list "datasets_mainvar"
  names(datasets_mainvar)[i]<- paste0("d",currentvar) # rename entry of list 
}
rm(df_crop, currentvar, allothermainvars ) # remove irrelevant variables from global console
list2env( datasets_mainvar , .GlobalEnv ) # create 6 dataframes from list "datasets_mainvar"
# checking:
mean(dculture$culture)
mean(dculture$age)
mean(dculture$married, na.rm = TRUE)
mean(dculture$needcare, na.rm = TRUE)


mean(dcinema$cinema)
mean(dcinema$married, na.rm = TRUE)
mean(dcinema$needcare, na.rm = TRUE)


mean(dsports$sports)
mean(dsports$married, na.rm = TRUE)
mean(dsports$needcare, na.rm = TRUE)


mean(dsocial$social)
mean(dvolunteer$volunteer)
mean(dhelp$help)

mean(dculture$EP)
mean(dsocial$UE)
mean(dculture$OLF)
mean(dculture$UEPC)
mean(dculture$UEO)



## "Zwischenoutput" --------------------------------------------------------------
# Please comment out the pair of i and j which an output should be created for:
# i <- dculture
# j <- "culture"

# <- dcinema
# j<- "cinema"

# i <- dsports 
# j <- "sports"

# i <- dsocial
# j <- "social"

# i <- dvolunteer
# j <- "volunteer"

i <- dhelp
j <- "help"
stargazer(data = as.data.frame(i[c(j,"age", "married", "needcare")]), 
          type="latex", summary = TRUE, 
          title = paste("Summary Statistics for \\textbf{", j, "}"),
          digits = 3, median = TRUE,  
          omit.summary.stat = c("N","p25", "p75", "Min", "Max", "Median"), 
          summary.stat = c("Mean", "Sd"), 
          notes.align = "l",
          header = FALSE,
          notes = c(paste("N:", nrow(i)), paste("Individuals:", length(unique(i$pid)))))



