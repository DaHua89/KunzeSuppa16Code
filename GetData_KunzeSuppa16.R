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
setwd("~/Downloads/Stata/") # Isabell 
#setwd("C:/Users/User/Documents/SOEP-CORE.v37teaching_STATA/Stata/") # Till 
#setwd() # Max
raw_path <- "raw"
# install & load packages
libraries = c("haven", "dplyr", "labelled", "tidyr", "ggplot2", "Hmisc", 
              "stringi", "stargazer", "lubridate", "todor", "stringr", "fixest")
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
# 8) WORK disabled: harmonized: ple0041 (pl dataset; https://paneldata.org/soep-core/data/pl/ple0041)
#                   raw: ip7802, ip7802, mp7702, np8102, pp9702, rp9702, vp10502, xp9902, zp9602, bbp10102
# 9) MARRIED:  pgfamstd (pgen dataset; https://paneldata.org/soep-is/data/pgen/pgfamstd)
# 10) NUMBER OF CHILDREN: d11107 (pequiv dataset; https://paneldata.org/soep-core/data/pequiv/d11107)
# 11) SHOCK SPOUSE DIED: pld0148 (pl dataset https://paneldata.org/soep-core/data/pl/pld0148)
# 12) SHOCK CHILD BORN:  pld0154 (pl dataset https://paneldata.org/soep-core/data/pl/pld0154)
# 13) SHOCK DIVORCED OR SEPARATED:  harmonized: separated: pld0145(pl dataset https://paneldata.org/soep-core/data/pl/pld0145) 
#                                               divorced: pld0142(pl dataset https://paneldata.org/soep-core/data/pl/pld0142
#                                   raw:        separated: jp10307, lp10307, np11507, op12107, qp14211, sp13311, wp14114, yp15417, bap15918, bcp15021
#                                               divorced: jp10305, lp10305, np11505, op12105, qp14208, sp13308, wp14117, yp15420, bap15921, bcp15024
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
                              "ple0041"))  # Work Disability (harm) (8) 
                              

PL2 <- read_dta(file = file.path('pl.dta'), 
                col_select = c('pid','cid', 'hid', 'syear',
                               "pld0148", # Month Spouse died in PREVIOUS YEAR (-> (11))
                               "pld0145", # Month separated Previous Year (13)
                               "pld0142", # Month divorced Previous Year (13)
                               "pld0154")) # Shock: Child born (12)  

PL2$syear <- PL2$syear - 1 # Make then-year to previous-year e.g. entries of year 1992 become 1991


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

# (7) Years of Eductaion
ed1 = read_dta(file = file.path(raw_path, "ipequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                         "d1110992" )) # 1992
ed2 = read_dta(file = file.path(raw_path,"kpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110994" )) # 1994
ed3 = read_dta(file = file.path(raw_path,"mpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110996" )) # 1996
ed4 = read_dta(file = file.path(raw_path,"npequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110997" )) # 1997
ed5 = read_dta(file = file.path(raw_path,"ppequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110999" )) # 1999
ed6 = read_dta(file = file.path(raw_path,"rpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110901" )) # 2001
ed7 = read_dta(file = file.path(raw_path,"vpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110905" )) # 2005
ed8 = read_dta(file = file.path(raw_path,"xpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110907" )) # 2007
ed9 = read_dta(file = file.path(raw_path,"zpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                        "d1110909" )) # 2009
ed10 = read_dta(file = file.path(raw_path,"bbpequiv.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                         "d1110911" )) # 201
colnames(ed1) = colnames(ed2) = colnames(ed3) = colnames(ed4) = colnames(ed5) = colnames(ed6) = colnames(ed7) = colnames(ed8) = colnames(ed9) = colnames(ed10)
EDU =  rbind(ed1,ed2,ed3,ed4,ed5,ed6,ed7,ed8,ed9, ed10)
rm(ed1, ed2, ed3, ed4, ed5, ed6, ed7, ed8, ed9, ed10)

# (8) Work Disability (raw)
dis1 = read_dta(file = file.path(raw_path,"ip.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "ip7802" )) # 1992
dis2 = read_dta(file = file.path(raw_path,"kp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "kp8402" )) # 1994
dis3 = read_dta(file = file.path(raw_path,"mp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "mp7702" )) # 1996
dis4 = read_dta(file = file.path(raw_path,"np.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "np8102" )) # 1997
dis5 = read_dta(file = file.path(raw_path,"pp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "pp9702" )) # 1999
dis6 = read_dta(file = file.path(raw_path,"rp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "rp9702" )) # 2001
dis7 = read_dta(file = file.path(raw_path,"vp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "vp10502" )) # 2005
dis8 = read_dta(file = file.path(raw_path,"xp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "xp9902" )) # 2007
dis9 = read_dta(file = file.path(raw_path,"zp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "zp9602" )) # 2009
dis10 = read_dta(file = file.path(raw_path,"bbp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                   "bbp10102" )) # 2011
colnames(dis1) = colnames(dis2) = colnames(dis3) = colnames(dis4) = colnames(dis5) = colnames(dis6) = colnames(dis7) = colnames(dis8) = colnames(dis9) = colnames(dis10)   
DIS =  rbind(dis1, dis2, dis3, dis4, dis5, dis6, dis7, dis8, dis9, dis10)
rm(dis1, dis2, dis3, dis4, dis5, dis6, dis7, dis8, dis9, dis10)

# (13) Shock: Separation or divorced_raw (raw)
sepdiv1 = read_dta(file = file.path(raw_path, "jp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                     "jp10307", "jp10305" )) # 1992 + 1
sepdiv2 = read_dta(file = file.path(raw_path,"lp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "lp10307", "lp10305" )) # 1994 + 1
sepdiv3 = read_dta(file = file.path(raw_path,"np.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "np11507", "np11505" )) # 1996 + 1
sepdiv4 = read_dta(file = file.path(raw_path,"op.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "op12107" , "op12105")) # 1997 + 1
sepdiv5 = read_dta(file = file.path(raw_path,"qp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "qp14211", "qp14208" )) # 1999 + 1
sepdiv6 = read_dta(file = file.path(raw_path,"sp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "sp13311" , "sp13308")) # 2001 + 1
sepdiv7 = read_dta(file = file.path(raw_path,"wp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "wp14114" , "wp14117")) # 2005 + 1
sepdiv8 = read_dta(file = file.path(raw_path,"yp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                    "yp15417" , "yp15420")) # 2007 + 1
sepdiv9 = read_dta(file = file.path(raw_path,"bap.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                     "bap15918", "bap15921" )) # 2009 + 1
sepdiv10 = read_dta(file = file.path(raw_path,"bcp.dta"), col_select = c("pid", 'cid','hid', 'syear',   
                                                                      "bcp15021", "bcp15024"  )) # 2011 + 1
colnames(sepdiv1) = colnames(sepdiv2) = colnames(sepdiv3) = colnames(sepdiv4) = colnames(sepdiv5) = colnames(sepdiv6) = colnames(sepdiv7) = colnames(sepdiv8) = colnames(sepdiv9) = colnames(sepdiv10)
SEPDIV =  rbind(sepdiv1,sepdiv2,sepdiv3,sepdiv4,sepdiv5,sepdiv6,sepdiv7,sepdiv8,sepdiv9, sepdiv10)
rm(sepdiv1,sepdiv2,sepdiv3,sepdiv4,sepdiv5,sepdiv6,sepdiv7,sepdiv8,sepdiv9, sepdiv10)



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
                  disabled_raw ="bbp10102", 
                  disabled_harm = "ple0041",
                  shock_partner = "pld0148", 
                  shock_child = "pld0154", 
                  separated_raw = "bcp15024",  
                  divorced_raw = "bcp15021", 
                  separated_harm = "pld0145", 
                  divorced_harm = "pld0142")
  
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
                             
                             shock_partner = case_when(shock_partner %in% c(1:12) ~ 1, shock_partner == -2 ~ 0), 
                             shock_partner = replace(shock_partner, shock_partner %in% c(-1, -3:-8), NA), 
                             
                             shock_child = replace(shock_child, shock_child %in% c(1:12), 1),
                             shock_child = replace(shock_child, shock_child %in% c(-1,-3:-8), NA),
                             shock_child = replace(shock_child, shock_child %in% c(-2), 0),
                             
                             disabled_raw = replace(disabled_raw, disabled_raw %in% c(-1, -3:-8), NA), 
                             disabled_raw_degree = replace(disabled_raw, disabled_raw == -2, 0), 
                             disabled_raw = replace(disabled_raw_degree, disabled_raw_degree > 0, 1),
                             
                             disabled_harm = replace(disabled_harm, disabled_harm %in% c(-1, -3:-8), NA), 
                             disabled_harm_degree = replace(disabled_harm, disabled_harm == -2, 0), 
                             disabled_harm = replace(disabled_harm_degree, disabled_harm_degree > 0, 1), 
                             
                             
                             separated_raw = replace(separated_raw, separated_raw %in% c(1:12), 1),
                             separated_raw = replace(separated_raw, separated_raw %in% c(-1,-3:-8), NA),
                             separated_raw = replace(separated_raw, separated_raw == -2, 0),
                             divorced_raw = replace(divorced_raw, divorced_raw %in% c(1:12), 1),
                             divorced_raw = replace(divorced_raw, divorced_raw %in% c(-1,-3:-8), NA),
                             divorced_raw = replace(divorced_raw, divorced_raw == -2, 0),
                             
                             separated_harm = replace(separated_harm, separated_harm %in% c(1:12), 1),
                             separated_harm = replace(separated_harm, separated_harm %in% c(-1,-3:-8), NA),
                             separated_harm = replace(separated_harm, separated_harm == -2, 0),
                             divorced_harm = replace(divorced_harm, divorced_harm %in% c(1:12), 1),
                             divorced_harm = replace(divorced_harm, divorced_harm %in% c(-1,-3:-8), NA),
                             divorced_harm = replace(divorced_harm, divorced_harm == -2, 0),
                            
                             shock_sepdiv_raw = case_when(divorced_raw == 0 & separated_raw == 0 ~ 0, 
                                                      divorced_raw == 1 & separated_raw == 0 ~ 1,
                                                      divorced_raw == 0 & separated_raw == 1 ~ 1,
                                                      divorced_raw == 1 & is.na(separated_raw) ~ 1, 
                                                      is.na(divorced_raw) & separated_raw == 1 ~ 1, 
                                                      divorced_raw == 1 & separated_raw == 1 ~ 1),
                            
                             
                             shock_sepdiv_harm = case_when(divorced_harm == 0 & separated_harm == 0 ~ 0, 
                                                       divorced_harm == 1 & separated_harm == 0 ~ 1,
                                                       divorced_harm == 0 & separated_harm == 1 ~ 1,
                                                       is.na(divorced_harm) & separated_harm == 0 ~ 0,
                                                       divorced_harm == 0 & is.na(separated_harm) ~ 0, 
                                                       divorced_harm == 1 & is.na(separated_harm) ~ 1, 
                                                       is.na(divorced_harm)& separated_harm == 1 ~ 1, 
                                                       divorced_harm == 1 & separated_harm == 1 ~ 1), 
                             
                             # For regression: 
                             age25_less = ifelse(age <= 25, 1, 0), 
                             age26_30 = ifelse(age >= 26 & age <=30, 1, 0), 
                             age31_35 = ifelse(age >= 31 & age <=35, 1, 0), 
                             age36_40 = ifelse(age >= 36 & age <=40, 1, 0), 
                             age41_45 = ifelse(age >= 41 & age <=45, 1, 0),  
                             age46_50 = ifelse(age >= 46 & age <=50, 1, 0),  
                             age51_55 = ifelse(age >= 51 & age <=55, 1, 0),  
                             age56_60 = ifelse(age >= 56 & age <=60, 1, 0),  
                             age61_65 = ifelse(age >= 61 & age <=65, 1, 0), 
                             age66_more = ifelse(age >= 66, 1, 0)) %>%
    
    mutate(UEPC = as.numeric(EP %in% c(5) & UEPC %in% c(1)), # UEPC variable construction
           EP = as.numeric(EP %in% c(1, 2, 3, 4, 6)), # EP variable construction
           OLF = as.numeric(OLF %in% c(1:5, 7:10, 13))) %>% # OLF variable construction
    mutate(UE = 1-OLF-EP) %>% # UE variable construction
    mutate(UEO = UE-UEPC) %>% # UEO variable construction
    mutate(UEO = replace(UEO, OLF==1, 0)) # To correct for individuals who are OLF and also lost their job because of plant closure
  return(df)
}

# The rsummstat() sub-function takes a number as its only argument. 
# It returns a summary statistic as LaTeX code. 
# For more information, see 8.2 of this document.
summstat <- function(number) { 
  i <- c("dculture", "dcinema","dsports", "dsocial", "dvolunteer", "dhelp")
  j <- c("culture",  "cinema","sports", "social", "volunteer", "help")
  stargazer(data = as.data.frame(get(i[number])[c(j[number], "EP", 
                                                  "UE", 
                                                  "OLF", 
                                                  "UEPC", 
                                                  "UEO", 
                                                  "age", 
                                                  "yearsedu",
                                                  "disabled_raw",  
                                                  "disabled_harm",  
                                                  "married", 
                                                  "child0", 
                                                  "child1",
                                                  "child2",
                                                  "child3plus",
                                                  "shock_partner",
                                                  "shock_child",
                                                  "shock_sepdiv_raw",
                                                  "shock_sepdiv_harm",
                                                  "west",
                                                  "needcare")]), 
            type="latex", summary = TRUE, 
            title = paste("Summary Statistics for \\textbf{", j[number], "}"),
            digits = 3, median = TRUE,  
            omit.summary.stat = c("N","p25", "p75", "Min", "Max", "Median"), 
            summary.stat = c("Mean", "Sd"), 
            notes.align = "l",
            header = FALSE,
            covariate.labels = c(j[number],
                                 "Employed", 
                                 "Unemployed", 
                                 "Out of labour force (OLF)", 
                                 "Plant closure unemployed", 
                                 "Other unemployed", 
                                 "Age (in years)", 
                                 "Years of education", 
                                 "Work disability (raw)", 
                                 "Work disability (harm)",
                                 "Married", 
                                 "Number of children: 0", 
                                 "Number of children: 1", 
                                 "Number of children: 2", 
                                 "Number of children: 3$+$", 
                                 "Shock: Spouse died", 
                                 "Shock: Child born", 
                                 "Shock: Divorce or separated (raw)", 
                                 "Shock: Divorce or separated (harm)", 
                                 "West Germany", 
                                 "Person needing care in HH"),
            notes = c(paste("N:", nrow(get(i[number]))), paste("Individuals:", length(unique(get(i[number])$pid)))))
}


# The getttests() sub-function takes a number as its only argument. 
# It returns a t-test for all covariates of one particular dataset.
getttests <- function(number){
  EP <- rep(0.724, 6)
  UE <- rep(0.069, 6)
  OLF <- rep(0.207, 6)
  UEPC <- rep(0.004,6)
  UEO <- rep(0.065 , 6)
  age <- c(42.579, 42.563, 42.565, 42.575, 42.562, 42.569)
  yearsedu <- c(12.058, 12.058, 12.060, 12.057, 12.059, 12.057 )
  disabled_harm <- c(0.091, 0.091, 0.090, 0.091, 0.091, 0.091)
  married <- rep(0.660, 6)
  child0 <- rep(0.594, 6)
  child1 <- rep(0.202, 6)
  child2 <- rep(0.152, 6)
  child3plus <- rep(0.052, 6)
  shock_partner <- rep(0.002,6)
  shock_child <- rep(0.031, 6)
  shock_sepdiv_harm <- rep(0.020,6)
  west <- rep(0.744,6)
  needcare <- rep(0.027,6)
  meansref  <- data.frame(rbind(EP, UE, OLF , UEPC, UEO, age, yearsedu, disabled_harm, 
                                married, child0, child1, child2, child3plus, 
                                shock_partner, shock_child, shock_sepdiv_harm, west, 
                                needcare))
  rm(EP, UE, OLF , UEPC, UEO, age, yearsedu, disabled_harm, 
     married, child0, child1, child2, child3plus, 
     shock_partner, shock_child, shock_sepdiv_harm, west, 
     needcare)
  
  i <- c("dculture", "dcinema","dsports", "dsocial", "dvolunteer", "dhelp")
  cov <- c("EP", "UE", "OLF", "UEPC", "UEO", "age", "yearsedu","disabled_harm",  
           "married", "child0", "child1","child2", "child3plus", "shock_partner",
           "shock_child","shock_sepdiv_harm","west","needcare")
  df <- get(i[number])
  for (j in 1:length(cov)){ 
    print(paste("### TEST FOR", cov[j], "OF", i[number], "DATASET ####"))
    print(t.test(df[which(names(df) == cov[j])], mu = meansref[j,number] , 
                 alternative = "two.sided"))
  }
}


getttable <- function(){
  EP <- rep(0.724, 6)
  UE <- rep(0.069, 6)
  OLF <- rep(0.207, 6)
  UEPC <- rep(0.004,6)
  UEO <- rep(0.065 , 6)
  age <- c(42.579, 42.563, 42.565, 42.575, 42.562, 42.569)
  yearsedu <- c(12.058, 12.058, 12.060, 12.057, 12.059, 12.057 )
  disabled_harm <- c(0.091, 0.091, 0.090, 0.091, 0.091, 0.091)
  married <- rep(0.660, 6)
  child0 <- rep(0.594, 6)
  child1 <- rep(0.202, 6)
  child2 <- rep(0.152, 6)
  child3plus <- rep(0.052, 6)
  shock_partner <- rep(0.002,6)
  shock_child <- rep(0.031, 6)
  shock_sepdiv_harm <- rep(0.020,6)
  west <- rep(0.744,6)
  needcare <- rep(0.027,6)
  meansref  <- data.frame(rbind(EP, UE, OLF , UEPC, UEO, age, yearsedu, disabled_harm, 
                                married, child0, child1, child2, child3plus, 
                                shock_partner, shock_child, shock_sepdiv_harm, west, 
                                needcare))
  rm(EP, UE, OLF , UEPC, UEO, age, yearsedu, disabled_harm, 
     married, child0, child1, child2, child3plus, 
     shock_partner, shock_child, shock_sepdiv_harm, west, 
     needcare)
  meandiff <- data.frame(matrix(ncol = 6, nrow = 36)) 
  rownames(meandiff) <- c(rbind(paste0(rownames(meansref),"_diff"), 
                                paste0(rownames(meansref),"_pvalue")))
  colnames(meandiff) <- c("culture", "cinema","sports", "social", "volunteer", "help")
  i <- c("dculture", "dcinema","dsports", "dsocial", "dvolunteer", "dhelp")
  cov <- c("EP" , "UE" , "OLF", "UEPC", "UEO", "age", "yearsedu","disabled_harm",  
           "married", "child0", "child1","child2", "child3plus", "shock_partner",
           "shock_child","shock_sepdiv_harm","west","needcare")

  for(number in 1:length(i)){
    df <- get(i[number])
  for (j in 1:length(cov)){ 
    #print(paste("### TEST FOR", cov[j], "OF", i[number], "DATASET ####"))
    #print(t.test(df[which(names(df) == cov[j])], mu = meansref[j,number] , alternative = "two.sided"))
    test <- t.test(df[which(names(df) == cov[j])], mu = meansref[j,number] , 
                   alternative = "two.sided")
    meandiff[2*j-1,number] <- round(test$estimate - meansref[j,number],3)
    meandiff[2*j,number] <-round(test$p.value, 3)
  }
  }
  return(meandiff)
}


# 5 Merge to universal dataset ------------------------------------------------
universal <- PPATHL%>% 
  left_join(PL) %>%
  left_join(PGEN) %>%
  left_join(EDU) %>%
  left_join(DIS) %>%
  left_join(HL) %>%
  left_join(PEQUIV) %>%
  left_join(PL2) %>%
  left_join(SEPDIV) %>%
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
mean(dculture$disabled_raw,na.rm = TRUE)
mean(dculture$disabled_raw_degree,na.rm = TRUE)
mean(dculture$disabled_harm,na.rm = TRUE)
mean(dculture$disabled_harm_degree,na.rm = TRUE)
mean(dculture$shock_sepdiv_raw, na.rm = TRUE)
mean(dculture$divorced_raw, na.rm = TRUE)
mean(dculture$shock_sepdiv_harm, na.rm = TRUE)

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
       
mean(dculture$shock_child, na.rm = T)
mean(dcinema$shock_child, na.rm = T)
mean(dsports$shock_child, na.rm = T)
mean(dsocial$shock_child, na.rm = T)
mean(dvolunteer$shock_child, na.rm = T)
mean(dhelp$shock_child, na.rm = T)

##  8.2 for Latex -----------------------------------------------
# Please choose a number: 
# 1:= culture
# 2:= cinema
# 3:= sports
# 4:= social
# 5:= volunteer
# 6:= help
summstat(1)



# 9 Regression Outputs ---------------------------------------------------------
## 9.1 Model1: Table 3 in ref paper --------------------------------------------
# CULTURE
model1 <- fixest::feols(culture ~ # Interchange with cinema, sports, help ... 
                          UE + OLF  + age26_30 + age31_35 + 
                          age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                          age61_65 + shock_partner + shock_child + shock_sepdiv_harm + 
                          needcare + yearsedu + disabled_harm + married + child1 +  
                          child2 + child3plus + west  | pid  + syear, 
                        cluster = ~ pid,
                        data = dculture)  # Interchange with dcinema, dsports, dhelp ... 


summary(model1)
## 9.1 Model1: Table 4 in ref paper --------------------------------------------
model2 <- fixest::feols(culture ~ # Interchange with cinema, sports, help ... 
                          UEPC + UEO + OLF  + age26_30 + age31_35 + 
                          age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                          age61_65 + shock_partner + shock_child + shock_sepdiv_harm + 
                          needcare + yearsedu + disabled_harm + married + child1 +  
                          child2 + child3plus + west  | pid  + syear, 
                        cluster = ~ pid, # for individual clustered standard errors
                        data = dculture)  # Interchange with dcinema, dsports, dhelp ...  

summary(model2)


## Extra: Can't include shock_sepdiv_raw: reason ---------------
covariates_harm <- c('pid', 'UE', 'OLF', 'age26_30', 'age31_35',
                'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                'age61_65' , 'age66_more' , 'shock_partner' , 'shock_child' , 'shock_sepdiv_harm' , 
                'needcare' , 'yearsedu' , 'disabled_harm' , 'married' , 'child1' ,  
                'child2' , 'child3plus' , 'west' , 'syear')
data <- dculture[unlist(model1$obs_selection), covariates_harm] # to get the data used by feols after deleting incomplete observations. Change to dhelp, dcinema ... respectively
data[complete.cases(data),] %>% nrow() == nrow(data) # to see that feols uses only complete case observations
covariates_raw <- c('pid', 'UE', 'OLF', 'age26_30', 'age31_35',
                'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                'age61_65' , 'age66_more' , 'shock_partner' , 'shock_child' , 'shock_sepdiv_raw' , 
                'needcare' , 'yearsedu' , 'disabled_harm' , 'married' , 'child1' ,  
                'child2' , 'child3plus' , 'west' , 'syear')

data <- dculture %>% select(all_of(covariates)) # select covariates including shock_sepdiv_raw
colnames(data)
complete.cases(data) %>% mean() # only low share of observations left
data <- data[complete.cases(data),]
unique(data[,1]) %>% nrow() # we now have the same number of unique individuals as observations
  # This leads to more parameters than observations because of the fixed individual effects



# 10 Difference in Means Analysis ---------------------------------------------
## 10.1 Main Variables ----------------------------------------------------------
t.test(dculture$culture, mu = 1.841, alternative = "two.sided") # p-value = 0.01506 <  0.05 -> stat. sign. difference  
t.test(dcinema$cinema, mu = 2.045, alternative = "two.sided") # p-value = 0.195 > 0.05 
t.test(dsports$sports, mu = 2.262, alternative = "two.sided") # p-value = 0.0006551 
t.test(dsocial$social, mu = 3.191, alternative = "two.sided") # p-value = 0.0009791
t.test(dvolunteer$volunteer, mu = 1.553, alternative = "two.sided") # p-value = 0.00634 
t.test(dhelp$help, mu = 2.476, alternative = "two.sided") # p-value = 0.006475

## 10.2 Covariates --------------------------------------------------------------
meansdiff <- getttable() # Check outout dataframe: meandiff in global environment 
# Control: 
# Please choose a number: 
# 1:= culture
# 2:= cinema
# 3:= sports
# 4:= social
# 5:= volunteer
# 6:= help
number <- 1
getttests(1)



# 11 Histogram ----------------------------------------------------------------
data_hist <- data_all %>% 
  select(cinema, culture, sports, social, volunteer, help) %>% 
  rename("Cinema" = cinema, 
         "Culture" = culture, 
         "Sports" = sports, 
         "Socialize" = social, 
         "Volunteer" = volunteer, 
         "Helping" = help)
data_hist <- gather(data_hist)               
data_hist$key <- factor(data_hist$key,levels = c("Cinema","Culture", "Sports", 
                                                 "Socialize", "Volunteer",  "Helping"))
ggplot(gather(data_hist), aes(value, group = key)) + 
  geom_histogram(aes(y = stat(density)),bins = 4, binwidth = 1, col="grey") + 
  #scale_y_continuous(labels = percent) +
  coord_cartesian(ylim=c(0, 0.7))+
  facet_wrap(~ key) + 
  #ggtitle("Relative Response Frequencies for Social Activities") + # Titel würde ich via Latex hinzufügen 
  theme( title = element_text(colour = "gray20", size = 10),
         axis.title.x= element_text(colour = "gray40", size = 10),
         axis.title.y = element_text(colour = "gray40", size = 10), 
         axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=0.2, colour="gray40"),
         panel.grid.major.y = element_line(colour="gray40", size=0.1, linetype = 'solid'),
         axis.ticks = element_line(colour="gray40", size = 0.1),
         panel.background = element_rect(fill = "transparent", color = NA)) + 
    scale_x_continuous(breaks = c(1:4), labels= c("never","seldom",
                                                    "monthly","weekly")) + 
  xlab("Frequency") + 
  ylab("Fraction")  
  



