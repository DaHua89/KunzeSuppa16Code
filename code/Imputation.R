# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************* Imputation.R (master: MASTER.R) **************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v30, teaching version [DOI: 10.5684/soep.v30])
Note:       Please refer to the MASTER.R file to run the present R script. 
'

# 1 Load in data ---------------------------------------------------------------
## 1.1 Load in data ------------------------------------------------------------
df=list()
toload <- c("data_all", "data_main")
for( i in 1:length(toload)){
  i <- toload[i]
  if (exists(i)) {
    print(paste("File", i, "successfully loaded."))
  }else if (file.exists(file.path(getwd(), "data",paste0(i, ".dta")))){ 
    df[[i]] <- read_dta(file = file.path(getwd(), "data",paste0(i, ".dta")))
    print(paste("File", i, "successfully loaded."))
  }else {
    print(paste("File", i, "missing. Please run GetDataset.R first!"))
  }
  list2env( df , .GlobalEnv )
}
rm(df)


## Long: Till ------------------------------------------------------------------
# 1 Crop data frame ------------------------------------------------------------
# 1.1 Define things and convert variables to factors and numerics -------------
covariates <- c('pid', 'UEPC', 'UEO', 'OLF', 'age26_30', 'age31_35',
                'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                'age61_65', 'shock_partner' , 'shock_child' , 'shock_sepdiv' , 
                'needcare' , 'yearsedu' , 'disabled' , 'married' , 'child1' ,  
                'child2' , 'child3plus' , 'west' , 'syear')
outcomes <- c('culture', 'cinema', 'sports', 'help', 'volunteer', 'social')
years <- c(1992, 1994, 1996, 1997, 2001, 2005, 2007, 2009, 2011)
no_factor <- c("cid", "hid", "pid", "yearsedu", "age", all_of(outcomes), 'syear')

list1 <- lapply(data_main[,-which(colnames(data_main) %in% no_factor)], factor)
list2 <- lapply(data_main[,which(colnames(data_main) %in% no_factor)], as.numeric)
data2 <- data.frame(list1, list2)

# 1.2 Subset set data -------------------
data3 <- data2 %>% select(all_of(covariates), all_of(outcomes))
data3[!data3$syear%in% years, outcomes] <- NA # make sure outcomes are NA in non-selected years
data4 <- data3 %>% filter(syear %in% years) # if we want only the selected years

# 1.3 Predictor matrix and imputation -----------------------
pm <- make.predictorMatrix(data3)
pm[,'pid'] <- -2 # specify cluster variable pid 
pm['pid', 'pid'] <- 0 # (but not for itself)
pm[,'syear'] <- 0 # not sure here, but for now don't include syear as a predictor (gave a warning message if included as a predictor when it was a factor)
pm[outcomes,] <- 0 # if outcomes are not to be imputed
data5 <- drop_na(data4, all_of(outcomes)) # if we want to drop the outcome missings

#install.packages("blme")
library("blme")
# old version: 
# imp_long_old <- mice(data3, predictorMatrix = pm, 
#                     defaultMethod = c('2l.pmm', '2l.pmm', 'polyreg', 'polr'), 
#                     seed=328,
#                     maxit = 5, m=5)
# new version: 
start.time <- Sys.time()
imp_long_new <- mice(data5, predictorMatrix = pm, 
                     method = '2l.pmm', seed=328,
                     maxit = 5, m=5, blme_use=T, blme_args=list('fixef.prior'='normal'))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# new version: 
imp_list <- imp_long_new %>% complete('all') # extract five imputed datasets

# 1.4 Estimation results ---------------------------------
est <- imp_list %>% lapply(feols, fml = cinema ~ # use here the dependent variable you want
                          UEPC + UEO + OLF  + age26_30 + age31_35 + 
                          age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                          age61_65 + shock_partner + shock_child + shock_sepdiv + 
                          needcare + yearsedu + disabled + married + child1 +  
                          child2 + child3plus + west  | pid  + syear, cluster = ~pid) %>%
  pool() 
summary(est)

# Long: Isabell ---------------------------------------------------------------
## 1 Set up data frame: remove irrelevant variables + adjust variable classes
df <- data_main %>% 
  select(!c(hid, cid, child, age, income)) %>% 
  drop_na(culture, cinema, sports, social, volunteer, help)
no_factor <- c("pid", "syear", "yearsedu", "culture", "cinema","sports", "social", "volunteer", "help")
yes_factor <- setdiff(names(df), no_factor)
df <- df %>%
  mutate(across(all_of(yes_factor), factor), 
         across(all_of(no_factor), as.numeric)) %>%   
  as.data.frame()
str(df)
## 2 Make predictor matrix: 
allVars <- names(df)
missVars <- names(df)[colSums(is.na(df)) > 0]
pm <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(pm) <- allVars
colnames(pm) <- allVars
# Imputer Matrix: Specify variables informing imputation 
imputerMatrix <- pm
imputerVars <- c('UEPC', 'UEO', 'OLF', 'EP', 'UE', 'age26_30', 'age31_35',
                 'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                 'age61_65', 'shock_partner' , 'shock_child' , 'shock_sepdiv' , 
                 'needcare' , 'yearsedu' , 'disabled' , 'married' , 'child1' ,  
                 'child2' , 'child3plus' , 'west' )
imputerMatrix[,imputerVars] <- 1
imputerMatrix
# Imputed Matrix: Specify variables with missingness to be imputed. 
imputedMatrix <- pm
imputedVars <- c("shock_sepdiv", "needcare", "yearsedu","married", "disabled", "shock_child", "shock_partner")
imputedMatrix[imputedVars,] <- 1
imputedMatrix
# Full Predictor Matrix: rows are imputed variables; cols are imputer variables
pm <- imputerMatrix * imputedMatrix 
# Specify cluster variable pid 
pm[,'pid'] <- -2 
# Diagonals must be zeros 
diag(pm) <- 0
pm


#install.packages("blme")
library("blme")
# old version: 
imp_long_old <- mice(df, predictorMatrix = pm, 
                 defaultMethod = c('2l.pmm', '2l.pmm', 'polyreg', 'polr'), 
                 seed=328,
                 maxit = 5, m=5)
# new version: 
imp_long_new <- mice(df, predictorMatrix = pm, 
                 method = '2l.pmm', seed=328,
                 maxit = 5, m=5, blme_use=T, blme_args=list('fixef.prior'='normal'))

imp_list <- imp_long %>% complete('all') # change to imp_long_old or imp_long_new






