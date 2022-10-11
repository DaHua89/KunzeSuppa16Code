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
# Later data_crop will be merged onto imputed data set 
data_crop <- data_main %>% select(pid, cid, syear, culture, cinema, sports, social, volunteer, help)
# For the imputation we will work with data_imp of which the following variables are excluded: culture, cinema, sports, social, volunteer, help, hid, cid, child, age, income
data_imp <- data_main %>% select(!c(culture, cinema, sports, social, volunteer, help, hid, cid, child, age, income))
names(data_imp)
covariates <- c('pid', 'UEPC', 'UEO', 'OLF', 'age26_30', 'age31_35',
                'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                'age61_65', 'shock_partner' , 'shock_child' , 'shock_sepdiv' , 
                'needcare' , 'yearsedu' , 'disabled' , 'married' , 'child1' ,  
                'child2' , 'child3plus' , 'west' , 'syear')
no_factor <- c("pid", "yearsedu",  'syear')
list1 <- lapply(data_imp[,-which(colnames(data_imp) %in% no_factor)], factor)
data_imp <- data.frame(list1, data_imp[,no_factor])
data_imp <- data_imp %>% select(all_of(covariates))
str(data_imp)
pm <- make.predictorMatrix(data_imp)
pm[,'pid'] <- -2 # specify cluster variable pid 
pm['pid', 'pid'] <- 0 # (but not for itself)
pm

#install.packages("blme")
library("blme")
# old version: 
imp_long_old <- mice(data_imp, predictorMatrix = pm, 
                     defaultMethod = c('2l.pmm', '2l.pmm', 'polyreg', 'polr'), 
                     seed=328,
                     maxit = 5, m=5)
# new version: 
imp_long_new <- mice(data_imp, predictorMatrix = pm, 
                     method = '2l.pmm', seed=328,
                     maxit = 5, m=5, blme_use=T, blme_args=list('fixef.prior'='normal'))
# new version: 
imp_list <- imp_long %>% complete('all') # extract five imputed datasets

' # this code is to be changed: 
est <- imp_list %>% lapply(feols, fml = cinema ~ 
                             UEPC + UEO + OLF  + age26_30 + age31_35 + 
                             age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                             age61_65 + shock_partner + shock_child + shock_sepdiv + 
                             needcare + yearsedu + disabled + married + child1 +  
                             child2 + child3plus + west  | pid  + syear, 
                           cluster = ~pid) %>% mice::pool() 
' 

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
imputedOnlyVars <- c("shock_sepdiv", "needcare", "yearsedu","married", "disabled", "shock_child", "shock_partner")
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






