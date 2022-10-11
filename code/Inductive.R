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

#Code takes approx. 7-8 minutes to run and returns 4 dataframes (`yearsedu imp`, `married impu`,
#`needcare impu`, `disabled impu`) which contain the pid, syears and the final RAC imputation
#value for each person in the dataset. They can be added to an existing imputed dataset 
#called "data" by:

#helpvec = c()

#for (i in 1:nrow(`yearsedu impu`)){
  
#  pid = as.numeric(`yearsedu impu`[i,1])
  
#  year = as.numeric(`yearsedu impu`[i,2])
  
#  helpvec[i] = which(data$pid == pid & data$syear == year)
  
#}

#data[helpvec,"row of variable"] = `yearsedu impu`[,3]


allobs = PPATHL%>% 
  left_join(PL) %>%
  left_join(PGEN) %>%
  left_join(HL) %>%
  left_join(PEQUIV)

allobs$syear = as.numeric(allobs$syear)

#renaming pgbilzeit to yearsedu

colnames(allobs)[colnames(allobs) == "pgbilzeit"] = "yearsedu"

colnames(allobs)[colnames(allobs) == "pgfamstd"] = "married"

colnames(allobs)[colnames(allobs) == "hlf0291"] = "needcare"

colnames(allobs)[colnames(allobs) == "ple0041"] = "disabled"

for (i in 1:4){
  
  var = c("yearsedu", "married", "needcare", "disabled")
  
  var = var[i]
  
  index = match(var, names(allobs))
  
  if (var == "yearsedu"){
    
    universal2 = allobs[which(allobs$yearsedu > -2),]
    
    universal2[which(universal2$yearsedu == -1), index] = NA
    
    nbers = 3000
  }else{}
  
  if (var == "married") {
    
    universal2 = allobs[which(allobs$married > -4),]
    
    universal2[which(universal2$married < 0), 18] = NA
    
    nbers = 15000
  }else{}
  
  if (var == "needcare") {
    
    universal2 = allobs[which(!is.na(allobs$needcare)),]
    
    universal2[which(universal2$needcare < 0), index] = NA
    
    nbers = 3000
  }else{}
  
  if (var == "disabled") {
    
    allobs1 = allobs[which(!is.na(allobs$disabled)),]
    
    universal2 = allobs1[which(allobs1$syear != 1993),]
    
    universal2 = universal2[which(universal2$syear != 1990),]
    
    universal2[which(universal2$disabled > 0), index] = 2
    
    universal2[which(universal2$disabled == -2), index] = 1
    
    universal2[which(universal2$disabled < 0), index] = NA
    
    nbers = 5000
  }else{}
  
  yearmean = aggregate(universal2[,index], by = list(universal2$syear), mean, na.rm = T)
  
  coleffects = (9*yearmean[,2])/sum(yearmean[,2])
  
  #Now that we have the column effects, we need to excude all people with all variables missing
  #for which we will be using the mice fallback method.
  
  form1 = as.formula(paste0(var, "~pid"))
  
  nacount = aggregate(form1, data = universal2, function(x) {sum(is.na(x))}, na.action = NULL)
  
  library(dplyr)
  
  occount = universal2 %>% count(pid)
  
  occount$naprop = occount$n - nacount[,2]
  
  occount2 = subset(occount, occount$naprop > 0)
  
  #Within the pid of occount2, we now have all users who have at least one observation for yearsedu 
  
  #Now we will restrict the dataset to these pids and create dcinema2:
  
  universal3 = universal2 %>%
    filter(pid %in% occount2$pid)
  
  #Computing the row effects:
  
  occount2$m_i = 1/occount2$naprop
  
  #creating a variable that assings column effects to the year
  
  if (var == "disabled") {
    universal3$coleffhelp = case_when(universal3$syear == 1984 ~ coleffects[1],
                                      universal3$syear == 1985 ~ coleffects[2],
                                      universal3$syear == 1986 ~ coleffects[3],
                                      universal3$syear == 1987 ~ coleffects[4],
                                      universal3$syear == 1988 ~ coleffects[5],
                                      universal3$syear == 1989 ~ coleffects[6],
                                      universal3$syear == 1991 ~ coleffects[7],
                                      universal3$syear == 1992 ~ coleffects[8],
                                      universal3$syear == 1994 ~ coleffects[9],
                                      universal3$syear == 1995 ~ coleffects[10],
                                      universal3$syear == 1996 ~ coleffects[11],
                                      universal3$syear == 1997 ~ coleffects[12],
                                      universal3$syear == 1998 ~ coleffects[13],
                                      universal3$syear == 1999 ~ coleffects[14],
                                      universal3$syear == 2000 ~ coleffects[15],
                                      universal3$syear == 2001 ~ coleffects[16],
                                      universal3$syear == 2002 ~ coleffects[17],
                                      universal3$syear == 2003 ~ coleffects[18],
                                      universal3$syear == 2004 ~ coleffects[19],
                                      universal3$syear == 2005 ~ coleffects[20],
                                      universal3$syear == 2006 ~ coleffects[21],
                                      universal3$syear == 2007 ~ coleffects[22],
                                      universal3$syear == 2008 ~ coleffects[23],
                                      universal3$syear == 2009 ~ coleffects[24],
                                      universal3$syear == 2010 ~ coleffects[25],
                                      universal3$syear == 2011 ~ coleffects[26],
                                      universal3$syear == 2012 ~ coleffects[27],
                                      universal3$syear == 2013 ~ coleffects[28],
                                      universal3$syear == 2014 ~ coleffects[29],
                                      universal3$syear == 2015 ~ coleffects[30],
                                      universal3$syear == 2016 ~ coleffects[31],
                                      universal3$syear == 2017 ~ coleffects[32],
                                      universal3$syear == 2018 ~ coleffects[33],
                                      universal3$syear == 2019 ~ coleffects[34],
                                      universal3$syear == 2020 ~ coleffects[35]
    )
    
  }else {
    universal3$coleffhelp = case_when(universal3$syear == 1984 ~ coleffects[1],
                                      universal3$syear == 1985 ~ coleffects[2],
                                      universal3$syear == 1986 ~ coleffects[3],
                                      universal3$syear == 1987 ~ coleffects[4],
                                      universal3$syear == 1988 ~ coleffects[5],
                                      universal3$syear == 1989 ~ coleffects[6],
                                      universal3$syear == 1990 ~ coleffects[7],
                                      universal3$syear == 1991 ~ coleffects[8],
                                      universal3$syear == 1992 ~ coleffects[9],
                                      universal3$syear == 1993 ~ coleffects[10],
                                      universal3$syear == 1994 ~ coleffects[11],
                                      universal3$syear == 1995 ~ coleffects[12],
                                      universal3$syear == 1996 ~ coleffects[13],
                                      universal3$syear == 1997 ~ coleffects[14],
                                      universal3$syear == 1998 ~ coleffects[15],
                                      universal3$syear == 1999 ~ coleffects[16],
                                      universal3$syear == 2000 ~ coleffects[17],
                                      universal3$syear == 2001 ~ coleffects[18],
                                      universal3$syear == 2002 ~ coleffects[19],
                                      universal3$syear == 2003 ~ coleffects[20],
                                      universal3$syear == 2004 ~ coleffects[21],
                                      universal3$syear == 2005 ~ coleffects[22],
                                      universal3$syear == 2006 ~ coleffects[23],
                                      universal3$syear == 2007 ~ coleffects[24],
                                      universal3$syear == 2008 ~ coleffects[25],
                                      universal3$syear == 2009 ~ coleffects[26],
                                      universal3$syear == 2010 ~ coleffects[27],
                                      universal3$syear == 2011 ~ coleffects[28],
                                      universal3$syear == 2012 ~ coleffects[29],
                                      universal3$syear == 2013 ~ coleffects[30],
                                      universal3$syear == 2014 ~ coleffects[31],
                                      universal3$syear == 2015 ~ coleffects[32],
                                      universal3$syear == 2016 ~ coleffects[33],
                                      universal3$syear == 2017 ~ coleffects[34],
                                      universal3$syear == 2018 ~ coleffects[35],
                                      universal3$syear == 2019 ~ coleffects[36],
                                      universal3$syear == 2020 ~ coleffects[37]
    )
    
  }
  
  universal3$rowefffraction = unlist(universal3[,index])/universal3$coleffhelp
  
  roweffs = aggregate(rowefffraction ~ pid, data = universal3, sum)
  
  roweffs$finalroweffects = roweffs$rowefffraction*occount2$m_i
  
  #Now we have computed the row and column effects and can move on to the single imputed value
  
  roweffs2 = roweffs[order(roweffs$finalroweffects),]
  
  experi = universal3[order(match(universal3$pid, roweffs2$pid)),]
  
  experi2 = left_join(experi,roweffs2, by = "pid")
  
  experi3 = experi2[order(match(experi2$pid, roweffs2$pid)),]
  
  impuset = experi3[, c("pid","syear", var, "finalroweffects")]
  
  #Now that we have the impuset and roweffs2, we will make a list of all IDs with missing values in the next step:
  
  missID = case_when(is.na(unlist(impuset[,3])) ~ impuset$pid)
  
  missIDy = case_when(is.na(unlist(impuset[,3])) ~ impuset$syear)
  
  missIDyear = data.frame(na.omit(missID), na.omit(missIDy))
  
  missIDyear2 = missIDyear %>% filter(na.omit.missIDy. %in% c(1992, 1994, 1996, 1997, 1999, 2001, 2005, 2007, 2009, 2011))
  
  #Now we will run a for loop to find the nearest neighbour of each missing ID
  
  uniqueIDs = unique(missIDyear2$na.omit.missID.)
  
  helpmatr = data.frame(matrix(NA, nrow = length(uniqueIDs), ncol = nbers))
  
  nbers2 = nbers + 1
  
  for (i in 1:length(uniqueIDs)){
    
    id = uniqueIDs[i]
    
    row = which(roweffs2$pid == id)
    
    roweff = roweffs2[row,3]
    
    roweffs2$distances = abs(roweffs2$finalroweffects - roweff)
    
    neighbours = roweffs2 %>% slice_min(distances, n = nbers2 )
    
    helpmatr[i,] = as.numeric(neighbours[2:nbers2,1])
    
  }
  
  #With this, we have obtained a list of all neighbours of all unique IDs with missings
  
  #Now we will need to join these with the uniqueIDs and leftjoin to the missIDyears
  
  miss2 = data.frame(uniqueIDs, helpmatr)
  
  colnames(missIDyear) = c("uniqueIDs","years")
  
  missleftj = left_join(missIDyear, miss2, by = "uniqueIDs")
  
  #Now we will try to compute the imputation by iterating through the neighbours to see which offer
  #data that we can use for the imputation
  
  missleftj2 = missleftj %>% filter(years %in% c(1992, 1994, 1996, 1997, 1999, 2001, 2005, 2007, 2009, 2011))
  
  helpmatr2 = data.frame(matrix(NA, nrow = nrow(missleftj2), ncol = 2))
  
  nbers3 = nbers2 + 1
  
  for (i in 1:nrow(missleftj2)){
    
    row = missleftj2[i,]
    
    restrict = impuset %>% filter(pid %in% row[4:nbers3])
    
    restrict2 = restrict %>% filter(syear %in% row[2])
    
    restrict3 = restrict2[order(match(restrict2$pid, row)),]
    
    restrict4 = na.omit(restrict3)
    
    helpmatr2[i,] = restrict4[1,3:4]
    
  }
  
  #Now we only need to find the roweffect of the original variable and can assemble the imputation
  
  helpvec2 = c()
  
  for (i in 1:nrow(helpmatr2)){
    
    pid = as.numeric(missleftj2[i,1])
    
    year = as.numeric(missleftj2[i,2])
    
    helpvec2[i] = which(impuset$pid == pid & impuset$syear == year)
    
  }
  
  #Now we will divide row effect of incomplete case by row effect of donor and multiply with final value
  
  fraction = impuset[helpvec2,4] / helpmatr2[,2]
  
  finalvals = fraction* helpmatr2[,1]
  
  imputation = cbind(missleftj2[,1:2],finalvals)
  
  assign(paste(var, "impu"),imputation)

  #alternatively swap universal for any other data frame that the missing data has to be put into
}



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
