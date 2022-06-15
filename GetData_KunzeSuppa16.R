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
                              (our names)
' 


# 1 Initialization -------------------------------------------------------------- 
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd("~/Downloads/Stata/") # Isabell 
#setwd("C:/Users/User/Documents/SOEP-CORE.v37teaching_STATA/Stata/") # Till 
#setwd() # Max
raw_path <- "raw"
# install & load packages
libraries = c("haven", "dplyr", "labelled", "tidyr", "ggplot2", "Hmisc", 
              "stringi", "stargazer", "lubridate", "todor", "stringr")
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
# 7) YEARS OF EDUCATION: d1110992, d1110994, d1110996, d1110997, d1110901, d1110905, d1110907, d1110909, d1110911
# 8) WORK DISABILITY:
# 9) MARRIED:  pgfamstd (pgen dataset; https://paneldata.org/soep-is/data/pgen/pgfamstd)
# 10) NUMBER OF CHILDREN: d11107 (pequiv dataset; https://paneldata.org/soep-core/data/pequiv/d11107)
# 11) SHOCK SPOUSE DIED: pld0146 (pl dataset https://paneldata.org/soep-core/data/pl/pld0146)
# 12) SHOCK CHILD BORN: 
# 13) SHOCK DIVORCE OR SEPERATED:
# 14) WEST GERMANY: l11102 (pequiv dataset; https://paneldata.org/soep-is/data/pgen/pgfamstd)
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
                              'plb0304_h', # UEPC (4)
                              "pld0146")) # Shock: Spouse died (11)

PGEN <- read_dta(file = file.path('pgen.dta'), 
                 col_select = c('pid', 'cid','hid', 'syear',   
                                "pgfamstd", # married (9) 
                                'pgemplst', # EP (1)
                                'pglfs')) # OLF (3)    

HL <- read_dta(file = file.path('hl.dta'), 
               col_select = c('cid','hid', 'syear',   
                              "hlf0291" ))  # person needing care in hh (15)

PEQUIV <- read_dta(file = file.path('pequiv.dta'), 
                   col_select = c('pid','cid','hid', 'syear',   
                                  "l11102", # West Germany (14)
                                  "d11107" )) # Number of children (10)



# (7) Years of Eductaion:
ed1 = read_dta(file = file.path(raw_path, "ipequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                         "d1110992" ))
ed2 = read_dta(file = file.path(raw_path,"kpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110994" ))
ed3 = read_dta(file = file.path(raw_path,"mpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110996" ))
ed4 = read_dta(file = file.path(raw_path,"npequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110997" ))
ed5 = read_dta(file = file.path(raw_path,"rpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110901" ))
ed6 = read_dta(file = file.path(raw_path,"vpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110905" ))
ed7 = read_dta(file = file.path(raw_path,"xpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110907" ))
ed8 = read_dta(file = file.path(raw_path,"zpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110909" ))
ed9 = read_dta(file = file.path(raw_path,"bbpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                         "d1110911" ))

colnames(ed1) = colnames(ed2) = colnames(ed3) = colnames(ed4) = colnames(ed5) = colnames(ed6) = colnames(ed7) = colnames(ed8) = colnames(ed9)

EDU =  rbind(ed1,ed2,ed3,ed4,ed5,ed6,ed7,ed8,ed9)
rm(ed1, ed2, ed3, ed4, ed5, ed6, ed7, ed8, ed9)

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
                  west = "l11102", 
                  married = "pgfamstd", 
                  yearsedu = "d1110911", 
                  needcare = "hlf0291",
                  EP = 'pgemplst',
                  OLF = 'pglfs',
                  UEPC = 'plb0304_h', 
                  child = "d11107", 
                  shock_partner = "pld0146") 
                  
  
  df <- df %>%  dplyr::rename(any_of(var_names))
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
  df_main <- df %>% filter(syear %in% c(1992, 1994, 1996,   
                                        1997, 1999, 2001, 2005, 
                                        2007, 2009, 2011)) %>%
    # Modifications on our 6 main variables 
    mutate_at(c("culture", "cinema", "sports", "social", "help", "volunteer"),
              funs(recode(., '2'=4, '3'=3, '4'=2, '5'=1))) %>% # recode all variables' (.) values, whereas 2 becomes 4, 4 becomes 2 and 5 becomes 1 
    mutate_at(c("culture", "cinema", "sports", "social", "help", "volunteer"),
              funs(ifelse(.<0, NA, .)))  # recodes all variables (.) to NA, if they take values below zero (.<0)
  df <- df %>% select(!c("culture", "cinema", "sports", "social", "help", "volunteer")) %>% 
    left_join(df_main) 
  
  # Modifications on our regressors 
  df <- df %>% dplyr::mutate(married = replace(married, married>1, 0),  # Yes(1); Otherwise (0)
                             married = replace(married, married < 0, NA), # define negative values as missing values 
                             yearsedu = replace(yearsedu, yearsedu <0, NA), # define negative values as missing values 
                             needcare = replace(needcare, needcare <0, NA), # define negative values as missing values 
                             needcare = replace(needcare, needcare>1, 0),  # Yes(1); Otherwise (0)
                             west = replace(west, west==2, 0), # West(1); East(0)
                             child = replace(child, child<0, NA), # define negative values as missing values 
                             child0 = case_when(child == 0 ~ 1, 
                                                child > 0 ~ 0), 
                             child1 =case_when(child == 1 ~ 1, 
                                               child > 1  ~ 0, 
                                               child < 1  ~ 0),  
                             child2 = case_when(child == 2 ~ 1, 
                                                child > 2  ~ 0, 
                                                child < 2  ~ 0), 
                             child3plus = case_when(child > 2 ~ 1, 
                                                    child <= 2  ~ 0),
                             shock_partner = case_when(shock_partner ==1 ~ 1, # Shock: Spouse died(1)
                                                       shock_partner == -2 ~ 0), # Shock: Spouse did not die (= does not apply) (-2)
                             shock_partner = replace(shock_partner, shock_partner %in% c(-1, -3:-8), NA)) %>% # define rest of negative values as missing values 

    mutate(UEPC = as.numeric(EP %in% c(5) & UEPC %in% c(1)), # UEPC variable construction
           EP = as.numeric(EP %in% c(1, 2, 3, 4, 6)), # EP variable construction
           OLF = as.numeric(OLF %in% c(1:5, 7:10, 13))) %>% # OLF variable construction
    mutate(UE = 1-OLF-EP) %>% # UE variable construction
    mutate(UEO = UE-UEPC) %>% # UEO variable construction
    mutate(UEO = replace(UEO, OLF==1, 0)) # To correct for individuals who are OLF and also lost their job because of plant closure
  
  return(df)
  
}


# 5 Merge to universal dataset ------------------------------------------------
universal <- PPATHL%>% 
  left_join(PL) %>%
  left_join(PGEN) %>%
  left_join(EDU) %>%
  left_join(HL) %>%
  left_join(PEQUIV) %>%
  arrange(pid, syear, hid) # order



# 6 Apply filters   -----------------------------------------------------------
data_all <- universal %>% 
  drop_na(syear, pid) %>% # exclude all observations with missing values in syear and pid
  mutate( gebjahr = replace(gebjahr, gebjahr<0, NA), # convert negative gebjahr to missing values 
          age = case_when(!is.na(gebjahr) ~ syear - gebjahr), # define age variable as:= sampleyear(year) - year born(gebjahr)
          age = replace (age, age<0, NA)) %>%  # exclude values where syear > gebjahr 
  filter(age>=21, age<=64,   # (1) Filter: Age between 21 and 64
         l11102 %in% c(1,2), # (2) Filter: Place of living is Germany (either West:1 or East:2)  
         syear %in% c(1991:2011)) #(3) Filter: data from 1991 to 2011 (without 1999 and 2000)
        #syear %in% c(1991:1998, 2001:2011)) 


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
    select(!allothermainvars) %>%   # exclude all other main variables (e.g. for dataset "culture" exclude: "cinema", "sports", "social", "help" and "volunteer" )
    drop_na(currentvar) %>% # exclude all missing values of the current main variable 
    filter(!syear ==1999)
  datasets_mainvar[[i]] <- df_crop # add to list "datasets_mainvar"
  names(datasets_mainvar)[i]<- paste0("d",currentvar) # rename entry of list 
}
rm(df_crop, currentvar, allothermainvars ) # remove irrelevant variables from global console
list2env( datasets_mainvar , .GlobalEnv ) # create 6 dataframes from list "datasets_mainvar"



# 8 Summary Statistics ---------------------------------------------------------
## 8.1 first overview ----------------------------------------------------------
mean(dculture$culture, na.rm = TRUE)
mean(dculture$age)
mean(dculture$married, na.rm = TRUE)
mean(dculture$needcare, na.rm = TRUE)
mean(dculture$west, na.rm = TRUE)
sum(is.na(dculture$child))
mean(dculture$child0)
mean(dculture$child1)
mean(dculture$child2)
mean(dculture$child3plus)
mean(dculture$shock_partner,na.rm = TRUE)


mean(dcinema$cinema)
mean(dcinema$married, na.rm = TRUE)
mean(dcinema$needcare, na.rm = TRUE)
mean(dcinema$west, na.rm = TRUE)

mean(dsports$sports)
mean(dsports$married, na.rm = TRUE)
mean(dsports$needcare, na.rm = TRUE)

mean(dsocial$social)
mean(dvolunteer$volunteer)
mean(dhelp$help)

mean(dculture$EP)
mean(dculture$UE)
mean(dculture$OLF)
mean(dculture$UEPC)
mean(dculture$UEO)

mean(dculture$yearsedu, na.rm = T)
mean(dcinema$yearsedu, na.rm = T)
mean(dsports$yearsedu, na.rm = T)
mean(dsocial$yearsedu, na.rm = T)
mean(dvolunteer$yearsedu, na.rm = T)
mean(dhelp$yearsedu, na.rm = T)

##  8.2 for Latex -----------------------------------------------
# Please comment out the pair of i and j which an output should be created for:
 i <- dculture
 j <- "culture"

# i <- dcinema
# j<- "cinema"

# i <- dsports 
# j <- "sports"

# i <- dsocial
# j <- "social"

# i <- dvolunteer
# j <- "volunteer"

# i <- dhelp
# j <- "help"
stargazer(data = as.data.frame(i[c(j, "EP", 
                                   "UE", 
                                   "OLF", 
                                   "UEPC", 
                                   "UEO", 
                                   "age", 
                                   "yearsedu",
                                   # Work disability
                                   "married", 
                                   "child0", 
                                   "child1",
                                   "child2",
                                   "child3plus",
                                   "shock_partner",
                                   "west",
                                   "needcare")]), 
          type="latex", summary = TRUE, 
          title = paste("Summary Statistics for \\textbf{", j, "}"),
          digits = 3, median = TRUE,  
          omit.summary.stat = c("N","p25", "p75", "Min", "Max", "Median"), 
          summary.stat = c("Mean", "Sd"), 
          notes.align = "l",
          header = FALSE,
          covariate.labels = c(j,
                               "Employed", 
                               "Unemployed", 
                               "Out of labour force (OLF)", 
                               "Plant closure unemployed", 
                               "Other unemployed", 
                               "Age (in years)", 
                               "Years of education", 
                               # "Work disability", 
                               "Married", 
                               "Number of children: 0", 
                               "Number of children: 1", 
                               "Number of children: 2", 
                               "Number of children: 3$+$", 
                               "Shock: Spouse died", 
                               #"Shock: Child born", 
                               #"Shock: Divorce or separated", 
                               "West Germany", 
                               "Person needing care in HH"),
          notes = c(paste("N:", nrow(i)), paste("Individuals:", length(unique(i$pid)))))




# Figure3 - Versuch ------------------------------------------------------------

d <- data_all
# For efficiency reasons, apply the following for-loop only on individuals(pid), which ...
# (1) ... have at least one time the entry "UE==1".  Consequently excludes all indiviuals and 
# their observations, if they are employed over the whole time period (UE==0 for all entries).
pid_f1 <- d %>% arrange(pid, syear) %>% group_by(pid) %>%  filter(UE ==1 ) %>%
  select(pid) %>% distinct() 
# Save pid_f1 as vector
pid_f1 <- as.vector(pid_f1$pid)
# (2) ... have at least 8 entries that the times t=-4,t=-3, t=-2, t=-1, t=1, t=2, t=3, t=4 
# can be denoted to. Consequently all individuals with less than 8 entries are excluded. 
pid_f2 <- d %>%  group_by(pid) %>% dplyr::count() %>% filter(n >8) %>% select(pid) %>% distinct() 
# Save pid_f2 as vector
pid_f2 <- as.vector(pid_f2$pid)

# Select all entries of individulas(pid) that are element of pid_f1 and pid_f2
pid_select <- intersect(pid_f2, pid_f1)

# Create empty dataframe "dfigure3" which is to be filled in the next loop 
dfigure3 <- data.frame(matrix(ncol = 4, nrow = 0)) 
for(i in 1:length(pid_select)){
  #which(pid_select == "13802")
  pid_nr <- pid_select[i]
  crop <- d  %>% select(pid, syear, EP, UE, OLF, sports, culture, social, volunteer, cinema, help) %>% 
    dplyr::mutate(UE = 2*UE, UE = UE + EP) %>% # recode UE variable (this line could be commented out)
    #mutate(UE = (EP-1)*(-1)) %>% # recode EP variable (ask me, if you would like to know more)
    filter(pid == pid_nr)
  
  # recode NA in "UE" variable to value 9 
  crop$UE[is.na(crop$UE)] <- 9 
  # create empty variable that is to be filled with -4,-3,-2,-1,1,2,3,4
  crop$time <- NA  
  # grasp all UE entries  for each pid, e.g. for firm with pid == "5201" : 
  # "2111112000000" (with recoded UE) / "1000001000000" (without recoded UE)
  entries <- paste0(crop$UE, collapse="")
  entries
  
  # look for specific patterns: (comment out the one you would like to run)
  # (A dot in a regular expression stands for ALL possible entries)
  # a) for recoded UE: 
  #pattern <- "(?<=[1,2]{3})1(?=2{4})" # sss12222, whereas s:= 1 or 2
  #pattern <- "(?<=[1,2]{3})1(?=2{1}[1,2]{3})" # sss12sss, whereas s:= 1 or 2
  #pattern <- "(?<=1{3})1(?=2{1}[1,2]{3})" # 11112sss, whereas x:= 1 or 2 
  pattern <- "(?<=.{3})1(?=2{4})" # xxx12222, whereas x:= 1 or 2 or 0
  #pattern <- "(?<=.{3})1(?=2{1}.{3})" # xxx12xxx, whereas x:= 1 or 2 or 0
  #pattern <- "(?<=1{3})1(?=2{1}.{3})" # 11112xxx, whereas x:= 1 or 2 or 0
  #pattern <- "(?<=1{3})1(?=2{4})" # 11112222
  # b) for original UE: 
  #pattern <- "(?<=.{3})0(?=1{4})" # xxx01111, whereas x:= 1 or 0
  #pattern <- "(?<=.{3})0(?=1{1}.{3})" # xxx01xxx, whereas x:= 1 or 0
  #pattern <- "(?<=0{3})0(?=1{1}.{3})" # 00001xxx, whereas x:= 1 or 0
  #pattern <- "(?<=0{3})0(?=1{4})" # 00001111
  
  p <- str_locate_all(entries, pattern) 
  p_rownr <- do.call(rbind, p)[,1] # save row number of forth position
  p_rownr
  
  # Add values to variable "time": 
  if(length(p_rownr)>0){ 
    for (i in 1:length(p_rownr)) {
      row <- p_rownr[i] 
      crop$time[row] <- -1 # add -1 to forth position
      crop$time[row-3] <- -4 # add -4 to first position 
      crop$time[row-2] <- -3 # add -3 to second position
      crop$time[row-1] <- -2 # add -2 to third position
      crop$time[row+1] <- 1 # add 1 to fifth position
      crop$time[row+2] <- 2 # add 2 to sixth position
      crop$time[row+3] <- 3 # add 3 to seventh position
      crop$time[row+4] <- 4 # add 4 to eighth position
      dfigure3 <- rbind(dfigure3, crop[(row-3):(row+4),]) 
      
    }
  }
}

' 
1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
-4,   -3,   -2,   -1    1,    2,    3,    4
      -4,   -3,   -2,   -1    1,    2,    3,    4
            -4,   -3,   -2,   -1    1,    2,    3,    4
                  -4,   -3,   -2,   -1    1,    2,    3,    4
                         -4,  -3,   -2,   -1    1,    2,    3,    4
                              -4,   -3,   -2,   -1    1,    2,    3,    4
                                    -4,   -3,   -2,   -1    1,    2,    3,    4
                                          -4,   -3,   -2,   -1    1,    2,    3,    4
                                                -4,   -3,   -2,   -1    1,    2,    3,    4
                                                       -4,   -3,   -2,   -1    1,    2,    3,    4
                                                             -4,   -3,   -2,   -1    1,    2,    3,    4
                                                                   -4,   -3,   -2,   -1    1,    2,    3,    4
                                                                         -4,   -3,   -2,   -1    1,    2,    3,    4
                                                                               -4,   -3,   -2,   -1    1,    2,    3,    4
'
# In case the waves are expanded to include the waves c(1992, 1994, 1995, 1996, 1997, 1998,2001, 2003, 2005, 2007, 
# 2008, 2009, 2011), then the next filter could also be added. It makes sure that the transition from 
# employment to unemployment happens in the years 1996-1997 or 1997-1998. In both cases we have enough 
# years to plot t=-4 to t=4. 
# pid_select2 <- figure3 %>% filter(UE==1 & syear %in% c(1997, 1998)) %>% select(pid) %>% distinct()
# pid_select2 <- as.vector(# pid_select2$pid)
# figure3 <- figure3 %>% filter(pid %in% # pid_select2) 


# Plot the mean over time in time-to-event-plot:
# (1) Plot them all together: 
library(gridExtra) # Please DO NOT put into Initialization, as some functions of dplyr will be overwritten (e.g. mutate()) and lead to errors in the code.
n <- c("social", "help", "cinema", "culture", "volunteer", "sports")
g <- lapply(1:6, function(j) {
  if(n[j] %in% c("social", "help")){
    ggplot(dfigure3, aes(x= time), y= get(n[j])) +
      stat_summary(aes(y = get(n[j])  , group=1), fun=mean, colour="red", geom="line", group=1) + 
      scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
      #coord_cartesian(ylim = c(1, 2.4)) + 
      coord_cartesian(ylim = c(2, 3.4)) +
      ylab(n[j]) + labs(title=paste(str_remove_all(pattern, "[\\(\\),<>=?]"),n[j]))
  }else {
    ggplot(dfigure3, aes(x= time), y= get(n[j])) +
      stat_summary(aes(y = get(n[j])  , group=1), fun=mean, colour="red", geom="line", group=1) + 
      scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
      coord_cartesian(ylim = c(1, 2.4)) + 
      #coord_cartesian(ylim = c(2, 3.4)) +
      ylab(n[j]) + labs(title=paste(str_remove_all(pattern, "[\\(\\),<>=?]"),n[j]))
  }
  
})
grid.arrange(grobs = g, ncol = 2)

# (2) Plot them separately: 
# Please comment out the pair of n and d which an output should be created for:
n <- "culture"
# n <- "cinema"
# n <- "sports"
# n <- "social"
# n <- "volunteer"
# n <- "help"
i <- as.integer(unlist(dfigure3[which(names(dfigure3) == n)]))
dfigure3 %>% ggplot(aes(x= time), y=  i) +
  stat_summary(aes(y = i, group=1), fun=mean, colour="red", geom="line", group=1) + 
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
  coord_cartesian(ylim = c(1, 2.4)) + # if social or help is plotted, please change this line with the next line
  # coord_cartesian(ylim = c(2, 3.4)) +
  ylab(n) + labs(title=str_remove_all(pattern, "[\\(\\),<>=?]"))






