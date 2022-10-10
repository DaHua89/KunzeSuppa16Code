# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************* Inductive.R (master: MASTER.R) ***************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v30, teaching version [DOI: 10.5684/soep.v30])
Note:       Please refer to the MASTER.R file to run the present R script. 
'

# 1 Load in data ---------------------------------------------------------------
df=list()
mains <- c("dculture", "dcinema", "dsports", "dsocial","dhelp", "dvolunteer")
for( i in 1:length(mains)){
  i <- mains[i]
if (exists(i)) {
  print(paste("File", i, "successfully loaded."))
}else if (file.exists(file.path(getwd(), "data",paste0(i, ".dta")))){ 
  df[[i]] <- haven::read_dta(file = file.path(getwd(), "data",paste0(i, ".dta")))
  print(paste("File", i, "successfully loaded."))
}else {
  print(paste("File", i, "missing.Please run GetDataset.R first!"))
}
list2env( df , .GlobalEnv )
}
rm(df)


# 2 Load sub- functions --------------------------------------------------------


# 3 MODEL 1 -------------------------------------------------------------------
## 3.1 Regression --------------------------------------------------------------
model1 <- fixest::feols(culture ~ # Interchange with cinema, sports, help ... 
                          UE + OLF  + age26_30 + age31_35 + 
                          age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                          age61_65 + shock_partner + shock_child + shock_sepdiv + 
                          needcare + yearsedu + disabled + married + child1 +  
                          child2 + child3plus + west | pid + syear,
                        cluster = ~ pid,
                        data = dculture)  # Interchange with dcinema, dsports, dhelp ... 
summary(model1)


## 3.2 Imputation --------------------------------------------------------------
### 3.2.1 pmm method, long format ----------------------------------------------
covariates <- c('pid', 'UEPC', 'UEO', 'OLF', 'age26_30', 'age31_35',
                'age36_40', 'age41_45' , 'age46_50' , 'age51_55' , 'age56_60' , 
                'age61_65', 'shock_partner' , 'shock_child' , 'shock_sepdiv' , 
                'needcare' , 'yearsedu' , 'disabled' , 'married' , 'child1' ,  
                'child2' , 'child3plus' , 'west' , 'syear')
no_factor <- c("cid", "hid", "pid", "yearsedu", "age", "cinema")
list1 <- lapply(dcinema[,-which(colnames(dcinema) %in% no_factor)], factor)
dcinema_crop <- data.frame(list1, dcinema[,no_factor])
dcinema_crop <- dcinema_crop %>% select(all_of(covariates), "cinema")

pm <- make.predictorMatrix(dcinema_crop)
pm[,'pid'] <- -2 # specify cluster variable pid 
pm['pid', 'pid'] <- 0 # (but not for itself)

imp_long <- mice(dcinema_crop, predictorMatrix = pm, 
                 method = '2l.pmm', seed=328,
                 maxit = 5, m=5, blme_use=T, blme_args=list('fixef.prior'='normal'))

imp_list <- imp_long %>% complete('all') # extract five imputed datasets

est <- imp_list %>% lapply(feols, fml = cinema ~ 
                          UEPC + UEO + OLF  + age26_30 + age31_35 + 
                          age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                          age61_65 + shock_partner + shock_child + shock_sepdiv + 
                          needcare + yearsedu + disabled + married + child1 +  
                          child2 + child3plus + west  | pid  + syear, 
                        cluster = ~pid) %>% mice::pool() 


### 3.2.2 Row-by-Columns -------------------------------------------------------


# 4 MODEL 2 -------------------------------------------------------------------
## 4.1 Regression --------------------------------------------------------------
model2 <- fixest::feols(culture ~ # Interchange with cinema, sports, help ... 
                          UEPC + UEO + OLF  + age26_30 + age31_35 + 
                          age36_40 + age41_45 + age46_50 + age51_55 + age56_60 + 
                          age61_65 + shock_partner + shock_child + shock_sepdiv + 
                          needcare + yearsedu + disabled + married + child1 +  
                          child2 + child3plus + west  | pid  + syear, 
                        cluster = ~ pid, # for individual clustered standard errors
                        data = dculture)  # Interchange with dcinema, dsports, dhelp ...  

summary(model2)
## 4.2 Imputation --------------------------------------------------------------
### 4.2.1 Long format ----------------------------------------------------------
### 4.2.2 Row-by-Columns -------------------------------------------------------



# remove irrelevant variables and dataframes from global console
