# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************* GetNumbers.R (master: MASTER.R) **************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v37, teaching version [DOI: 10.5684/soep.core.v37t])
Note:       Please refer to the MASTER.R file to run the present R script. 
'

# 1 Load & Subset data ---------------------------------------------------------
df=list()
toload <- c("main_dataset")
for( i in 1:length(toload)){
  i <- toload[i]
  if (exists(i)) {
    print(paste("File", i, "successfully loaded."))
  }else if (file.exists(file.path(getwd(), "data",paste0(i, ".dta")))){ 
    df[[i]] <- read_dta(file = file.path(getwd(), "data",paste0(i, ".dta")))
    print(paste("File", i, "successfully loaded."))
  }else {
    stop(paste("File", i, "missing. Please run GetDataset.R first!"))
    #print(paste("File", i, "missing. Please run GetDataset.R first!"))
  }
  list2env( df , .GlobalEnv )
}
rm(df, toload)


if (!exists("subdatasets")) {
  subdatasets <- list()
  main_vars <- c("culture", "cinema","volunteer", "social","help",  "sports")
  for (i in 1:length(main_vars)){
    currentvar <- main_vars[i] # current main variable (e.g main_vars[1] = "culture", main_vars[2] = "cinema" ...)
    allothermainvars <- main_vars[-(which(main_vars == currentvar))] # all other main variables except the current one 
    df_crop <- main_dataset %>% 
      select(!all_of(allothermainvars)) # exclude all other main variables (e.g. for data set "culture" exclude: "cinema", "sports", "social", "help" and "volunteer" )
    subdatasets[[i]] <- df_crop # add to list "subdatasets"
    names(subdatasets)[i]<- paste0("d",currentvar) # rename entry of list 
  }
  # Complete case subdatasets:  
  subdatasets_cc <- lapply(subdatasets, function(x){ 
    x <- x[complete.cases(x),]})
  names(subdatasets_cc) <- paste0(names(subdatasets_cc), "_cc")
}
# Unlist "subdatasets" and "subdatasets_cc" and create 6x2 dataframes 
list2env( subdatasets , .GlobalEnv ) 
list2env( subdatasets_cc , .GlobalEnv ) 




# 2 Get Numbers ----------------------------------------------------------------
# How many obs. does our main data set contain?
nrow(main_dataset) # 62812
# How many individuals does our main data set contain?
length(unique(main_dataset$pid)) # 21241

# How many obs. do we lose when applying the complete.case() fct. on the following datasets: 
# CULTURE:
# absolute
nrow(dculture_cc)-nrow(dculture) # - 5036
# percent
nrow(dcinema_cc)/nrow(dcinema) # 0.9190123

# CINEMA:
# absolute: 
nrow(dcinema_cc)-nrow(dcinema) #-5087
# percent: 
nrow(dvolunteer_cc)/nrow(dvolunteer) # 0.9179138

# VOLUNTEER
# absolute: 
nrow(dvolunteer_cc)-nrow(dvolunteer) # -5156
# percent: 
nrow(dvolunteer_cc)/nrow(dvolunteer) # 0.9179138


# SOCIALIZE 
# absolute: 
nrow(dsocial_cc)-nrow(dsocial) # -5043
# percent: 
nrow(dsocial_cc)/nrow(dsocial) # 0.9197128

# HELPING
# absolute: 
nrow(dhelp_cc)-nrow(dhelp) # -5077
# percent: 
nrow(dhelp_cc)/nrow(dhelp) # 0.9191715

# SPORTS:
# absolute: 
nrow(dsports_cc)-nrow(dsports) # -5215
# percent: 
nrow(dsports_cc)/nrow(dsports) # 0.9169745


# remove irrelevant variables and dataframes from global console
rm(dcinema, dculture, dsocial, dsports, dhelp, dvolunteer, 
   dcinema_cc, dculture_cc, dsocial_cc, dsports_cc, dhelp_cc, dvolunteer_cc)

