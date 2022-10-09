# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************  GetDataset.R (master: MASTER.R) **************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v30, teaching version [DOI: 10.5684/soep.v30])
Note:       Please refer to the MASTER.R file to run the present R script.  
'

## 1 Load sub- functions -------------------------------------------------------
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
                  yearsedu ="pgbilzeit",
                  needcare = "hlf0291",
                  EP = 'pgemplst',
                  OLF = 'pglfs',
                  UEPC = 'plb0304_h', 
                  child = "d11107", 
                  disabled = "ple0041",
                  shock_partner = "pld0148", 
                  shock_child = "pld0154", 
                  separated = "pld0145", 
                  divorced = "pld0142")
  
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
  df_main <- df %>% 
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
                             
                             
                             disabled = replace(disabled, disabled %in% c(-1, -3:-8), NA), 
                             disabled_degree = replace(disabled, disabled == -2, 0), 
                             disabled = replace(disabled_degree, disabled_degree > 0, 1), 
                             
                             separated = replace(separated, separated %in% c(1:12), 1),
                             separated = replace(separated, separated %in% c(-1,-3:-8), NA),
                             separated = replace(separated, separated == -2, 0),
                             divorced = replace(divorced, divorced %in% c(1:12), 1),
                             divorced = replace(divorced, divorced %in% c(-1,-3:-8), NA),
                             divorced = replace(divorced, divorced == -2, 0),
                             
                             shock_sepdiv = case_when(divorced == 0 & separated == 0 ~ 0, 
                                                      divorced == 1 & separated == 0 ~ 1,
                                                      divorced == 0 & separated == 1 ~ 1,
                                                      is.na(divorced) & separated == 0 ~ 0,
                                                      divorced == 0 & is.na(separated) ~ 0, 
                                                      divorced == 1 & is.na(separated) ~ 1, 
                                                      is.na(divorced)& separated == 1 ~ 1, 
                                                      divorced == 1 & separated == 1 ~ 1), 
                             
                             # Age variables: 
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
    select(!c(disabled_degree, separated, divorced, gebjahr)) %>% # remove construction variables
    mutate(UEPC = as.numeric(EP %in% c(5) & UEPC %in% c(1)), # UEPC variable construction
           EP = as.numeric(EP %in% c(1, 2, 3, 4, 6)), # EP variable construction
           OLF = as.numeric(OLF %in% c(1:5, 7:10, 13))) %>% # OLF variable construction
    mutate(UE = 1-OLF-EP) %>% # UE variable construction
    mutate(UEO = UE-UEPC) %>% # UEO variable construction
    mutate(UEO = replace(UEO, OLF==1, 0)) # To correct for individuals who are OLF and also lost their job because of plant closure
  return(df)
}

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


# 2 Retrieve data --------------------------------------------------------------
## 2.1 Load in variables from different SEOP-Core data sets  -------------------
PPATHL <- read_dta(file = file.path('data/Stata/ppathl.dta'), 
                   col_select = c("pid", # never changing person ID (key variable)
                                  "hid", # current wave household number (key variable)
                                  "cid", # original houshold number (key variable)
                                  "syear", # survey year (key variable)
                                  "gebjahr")) # used to generate variable 'age', which is derived as 'gebjahr' - 'syear, more info at: https://paneldata.org/soep-core/data/ppath/gebjahr

PL <- read_dta(file = file.path('data/Stata/pl.dta'), 
               col_select = c('pid','cid', 'hid', 'syear', # merging key variables 
                              "pli0090_h", # variable 'culture', more info at: https://paneldata.org/soep-core/data/pl/pli0090_h
                              "pli0091_h", # variable 'cinema', more info at: https://paneldata.org/soep-core/data/pl/pli0091_h
                              "pli0092_h", # variable 'sports', more info at: https://paneldata.org/soep-core/data/pl/pli0092_h
                              "pli0094_h", # variable 'social', more info at: https://paneldata.org/soep-core/data/pl/pli0094_h
                              "pli0095_h", # variable 'help', more info at: https://paneldata.org/soep-core/data/pl/pli0095_h
                              "pli0096_h", # variable 'volunteer', more info at: https://paneldata.org/soep-core/data/pl/pli0096_h
                              'plb0304_h', # variable 'plant closure unemployed (UEPC)', more info at: https://paneldata.org/soep-core/data/pl/plb0304_h
                              "ple0041",   # variable 'disabled', more info at: https://paneldata.org/soep-core/data/pl/ple0041
                              "pld0148",   # variable 'shock: spouse died', more info at:  https://paneldata.org/soep-core/data/pl/pld0148
                              "pld0145",   # used to generate variable 'shock: divorced or separated', more info at: https://paneldata.org/soep-core/data/pl/pld0145
                              "pld0142",   # used to generate variable 'shock: divorced or separated', more info at: https://paneldata.org/soep-core/data/pl/pld0142
                              "pld0154"))  # variable 'shock: child born', more info at: https://paneldata.org/soep-core/data/pl/pld0154


PGEN <- read_dta(file = file.path('data/Stata/pgen.dta'), 
                 col_select = c('pid', 'cid','hid', 'syear', # merging key variables 
                                "pgfamstd",   # variable 'married', more info at:  https://paneldata.org/soep-is/data/pgen/pgfamstd
                                'pgemplst',   # variable 'employed (EP)', more info at: https://paneldata.org/soep-core/data/pgen/pgemplst
                                'pglfs',      # variable 'out of labour force (OLF)' more info at: https://paneldata.org/soep-core/data/pgen/pgemplst 
                                'pgbilzeit')) # variable 'years of education (yearsedu)', more info at: https://paneldata.org/soep-core/datasets/pgen/pgbilzeit

HL <- read_dta(file = file.path('data/Stata/hl.dta'), 
               col_select = c('cid','hid', 'syear', # merging key variables 
                              "hlf0291" ))    # variable 'need care', more info at: https://paneldata.org/soep-core/data/hl/hlf0291

PEQUIV <- read_dta(file = file.path('data/Stata/pequiv.dta'), 
                   col_select = c('pid','cid','hid', 'syear',  # merging key variables 
                                  "l11102",   # variable 'west', more info at: https://paneldata.org/soep-core/datasets/pequiv/l11102
                                  "d11107" )) # used to generate variables 'child0', 'child1', 'child2', 'child3+', more info at: https://paneldata.org/soep-core/data/pequiv/d11107

## 2.1 Merge all variables from different data set to universal data set -------
universal <- PL %>%
  left_join(PGEN) %>%
  left_join(HL) %>%
  left_join(PEQUIV) %>%
  arrange(pid, syear, hid) # order rows according to pid, syear and hid
# Delete all rows with missing values for all merged-onto variables


# 3 Generate main datasets -----------------------------------------------------
## 3.1 Apply filters -----------------------------------------------------------
data_all <- universal %>% 
  drop_na(syear, pid) %>% # exclude all observations with missing values in syear and pid
  mutate( gebjahr = replace(gebjahr, gebjahr<0, NA), # convert negative gebjahr to missing values 
          age = case_when(!is.na(gebjahr) ~ syear - gebjahr), # define age variable as:= sampleyear(year) - year born(gebjahr)
          age = replace (age, age<0, NA)) %>%  # exclude values where syear > gebjahr 
  filter(age>=21, age<=64,   # (1) Filter: Age between 21 and 64
         l11102 %in% c(1,2), # (2) Filter: Place of living is Germany (either West:1 or East:2)  
         # (3) Filter: data from waves 92,94,96,97,01,05,07,09,11
         syear %in% c(1992, 1994, 1996, 1997, 2001, 2005, 2007, 2009, 2011)) 

## 3.2 Rename and recode main variables ----------------------------------------
data_all <- data_all %>% renaming() %>% recoding() 

## 3.3 Subsetting data set ------------------------------------------------------
datasets <- list()
main_vars <- c("culture", "cinema", "sports", "social","volunteer", "help")
for (i in 1:length(main_vars)){
  currentvar <- main_vars[i] # current main variable (e.g main_vars[1] = "culture", main_vars[2] = "cinema" ...)
  allothermainvars <- main_vars[-(which(main_vars == currentvar))] # all other main variables except the current one 
  df_crop <- data_all %>% 
    select(!all_of(allothermainvars)) %>%   # exclude all other main variables (e.g. for data set "culture" exclude: "cinema", "sports", "social", "help" and "volunteer" )
 #   drop_na(all_of(currentvar)) # exclude all missing values of the current main variable 
  datasets[[i]] <- df_crop # add to list "datasets"
  names(datasets)[i]<- paste0("d",currentvar) # rename entry of list 
}
list2env( datasets , .GlobalEnv ) # create 6 dataframes from list "datasets"
# remove irrelevant variables and dataframes from global console
rm(df_crop, i, currentvar, allothermainvars, HL, 
   PEQUIV, PGEN, PL, PPATHL, universal, data_all) 


