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
df=list()
toload <- c("our_dataset")
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




# 2 Imputing missing values - long format  -------------------------------------
## 2.1 Define things and convert variables to factors and numerics -------------
covariates <- c('pid', 'UEPC', 'UEO', 'OLF', 'UE', 'age26_30', 'age31_35',
                'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                'age61_65', 'shock_partner' , 'shock_child' , 'shock_sepdiv' , 
                'needcare' , 'yearsedu' , 'disabled' , 'married' , 'child1' ,  
                'child2' , 'child3plus' , 'west' , 'syear')
outcomes <- c('culture', 'cinema', 'sports', 'help', 'volunteer', 'social')
years <- c(1992, 1994, 1996, 1997, 2001, 2005, 2007, 2009, 2011)
no_factor <- c("cid", "hid", "pid", "yearsedu", "age", all_of(outcomes), 'syear')

list1 <- lapply(our_dataset[,-which(colnames(our_dataset) %in% no_factor)], factor)
list2 <- lapply(our_dataset[,which(colnames(our_dataset) %in% no_factor)], as.numeric)
data2 <- data.frame(list1, list2)

## 2.2 Subset set data ---------------------------------------------------------
data3 <- data2 %>% select(all_of(covariates), all_of(outcomes))
data3[!data3$syear%in% years, outcomes] <- NA # make sure outcomes are NA in non-selected years
data4 <- data3 %>% filter(syear %in% years) # if we want only the selected years

## 2.3 Predictor matrix and imputation ------------------------------------------
pm <- make.predictorMatrix(data3)
pm[,'pid'] <- -2 # specify cluster variable pid 
pm['pid', 'pid'] <- 0 # (but not for itself)
pm[,'syear'] <- 0 # not sure here, but for now don't include syear as a predictor (gave a warning message if included as a predictor when it was a factor)
# pm[outcomes,] <- 0 # if outcomes are not to be imputed
pm
#data5 <- drop_na(data4, all_of(outcomes)) # if we want to drop the outcome missings

if(!exists("imp_long") & file.exists(file.path(getwd(), "data","imp_long.RData"))){ 
  load(file.path(getwd(), "data","imp_long.RData")) 
  print("imp_long.RData is loaded into R environment.")
  } else if (!exists("imp_long") & !file.exists(file.path(getwd(), "data","imp_long.RData"))){ 
  library("blme")
  start.time <- Sys.time()
  imp_long <- mice(data4, predictorMatrix = pm, 
                   method = '2l.pmm', seed = 328,
                   maxit = 5, m = 5, blme_use=T, 
                   blme_args=list('fixef.prior'='normal'))
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  save(imp_long, file = "imp_long.RData")
  } 
imp_list <- imp_long %>% complete('all') # extract five imputed datasets




## 1.4 Estimation results ------------------------------------------------------
### 1.4.1  for Model 1 --------------------------------------------------------- 
depvars <- c("culture", "cinema", "sports", "social", "volunteer", "help")
estimateModel1_imp <- lapply(depvars, function(var) {
est <- imp_list %>% 
  lapply(feols, fml = xpd(..lhs ~ UE + OLF + age26_30 + age31_35 + age36_40 + 
                            age41_45 + age46_50 + age51_55 + age56_60 + age61_65 +
                            shock_partner + shock_child + shock_sepdiv + needcare + 
                            yearsedu + disabled + married + child1 +  child2 + 
                            child3plus + west | 
                            pid + syear, ..cluster = ~ pid, ..lhs = var)) %>% 
  pool() %>% summary() %>% as.data.frame()
})
names(estimateModel1_imp) <- depvars

### 1.4.2  for Model 2 --------------------------------------------------------- 
depvars <- c("culture", "cinema", "sports", "social", "volunteer", "help")
estimateModel2_imp <- lapply(depvars, function(var) {
  est <- imp_list %>% 
    lapply(feols, fml = xpd(..lhs ~ UEPC + UEO + OLF + age26_30 + age31_35 + age36_40 + 
                              age41_45 + age46_50 + age51_55 + age56_60 + age61_65 +
                              shock_partner + shock_child + shock_sepdiv + needcare + 
                              yearsedu + disabled + married + child1 +  child2 + 
                              child3plus + west | 
                              pid + syear, ..cluster = ~ pid, ..lhs = var)) %>% 
    pool() %>% summary() %>% as.data.frame()
})
names(estimateModel2_imp) <- depvars
# Test: 
# estimateModel2_imp[[1]]

