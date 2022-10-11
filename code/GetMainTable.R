# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************* Inductive.R (master: MASTER.R) ***************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v30, teaching version [DOI: 10.5684/soep.v30])
Note:       Please refer to the MASTER.R file to run the present R script. 
'

# 1 Load and subset data -------------------------------------------------------
## 1.1 Load in data ------------------------------------------------------------
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

## 1.2 Subsetting data ---------------------------------------------------------
datasets <- list()
main_vars <- c("culture", "cinema", "sports", "social","volunteer", "help")
for (i in 1:length(main_vars)){
  currentvar <- main_vars[i] # current main variable (e.g main_vars[1] = "culture", main_vars[2] = "cinema" ...)
  allothermainvars <- main_vars[-(which(main_vars == currentvar))] # all other main variables except the current one 
  df_crop <- our_dataset %>% 
    select(!all_of(allothermainvars))# %>% # exclude all other main variables (e.g. for data set "culture" exclude: "cinema", "sports", "social", "help" and "volunteer" )
  #drop_na(all_of(currentvar)) # exclude all missing values of the current main variable 
  df_crop <- df_crop[complete.cases(df_crop),] # only include complete cases! (remove all rows with NAs)
  datasets[[i]] <- df_crop # add to list "datasets"
  names(datasets)[i]<- paste0("d",currentvar) # rename entry of list 
}
list2env( datasets , .GlobalEnv ) # create 6 dataframes from list "datasets"
rm(df_crop, i, currentvar, allothermainvars)

## 1.3 Complete case dataset ---------------------------------------------------
data <- our_dataset[complete.cases(our_dataset),]
# Merge all 6 main datasets
# data <- dculture %>% full_join(dcinema) %>% full_join(dsports) %>% full_join(dsocial) %>%
#  full_join(dvolunteer) %>% full_join(dhelp)




# 2 Load sub- functions --------------------------------------------------------
MoveTStatistic <- function(df){
  x <- df %>% select(term, estimate, p.value) %>% mutate(estimate = as.character(round(estimate, 4)))
  xx <- df %>% select( term, statistic) %>% mutate( p.value = rep(NA, nrow(x)), 
                                                    term = paste0(term, "_t")) %>% 
    rename( estimate = statistic) %>% mutate(estimate = paste0("(",as.character(round(estimate, 2)), ")"))
  
  r <- rbind(x, xx)
  r <- r[kronecker(1:nrow(x), c(0, nrow(x)), "+"), ]
  return(r)
}
SetSigLevels <- function(df){ 
  # Define significance levels: * p<0.1. ** p < 0.05. *** p < 0.01
  df <- df %>% mutate(p.value = round(p.value,5), 
                      stars = case_when(p.value<0.01 ~ "three", 
                                        p.value<0.05 ~ "two", 
                                        p.value<0.1  ~ "one"))
  # Add stars behinde estimates: 
  df <- df %>% mutate(estimate = case_when(!is.na(stars) ~ paste0(estimate, stars), 
                                           is.na(stars) ~ estimate))
  return(df)
}
GetTexfile <- function(df){
  # Remove all names with _t in column "term"
  df$term <- str_replace(df$term, ".*_t", "")
  # Extract part of colnames after "_": 
  new_colnames <- str_extract(colnames(df), "(?<=_).*")
  new_colnames <- new_colnames[!duplicated(new_colnames)]
  new_colnames <- new_colnames[!is.na(new_colnames)]
  # Set number of columns per data set 
  ncols <- 3
  # Extract number of datasets, which are going to be plotted
  rep_no <- (ncol(df)-1)/ncols
  # Set up new header
  header_names <- c("")
  for(i in 1:rep_no){
    a <- new_colnames[i]
    header_names <- c(header_names,a)
  }
  new_header <- data_frame(c(" ", new_colnames[1],new_colnames[2],new_colnames[3]), c(1,ncols, ncols, ncols))
  
  # Create .tex file
  options(knitr.table.format = "latex")
  main <- kable(df, booktabs = TRUE, caption = "Summary of Regression Estimates", 
                col.names = c("",rep(c("\\textit{orig.}", "\\textit{repl.}", "\\textit{ext.I}"),rep_no)),
                align = "lcccccc", row.names = FALSE, linesep = "", escape = F) %>% 
    add_header_above(data_frame(header_names, c(1,rep(ncols, rep_no))), bold = TRUE) %>% 
    kable_styling(latex_options = "hold_position")
  main <- gsub("one" ,"^{*}",main ,fixed=TRUE)
  main <- gsub("two" ,"^{**}",main ,fixed=TRUE)
  main <- gsub("three" ,"^{***}",main,fixed=TRUE) 
  main <- gsub("table" ,"sidewaystable",main,fixed=TRUE) 
  main <- gsub("\\begin{sidewaystable}[!h]" ,"\\usepackage{booktabs, rotating} \n\\usepackage[table]{xcolor}" ,
               main,fixed=TRUE)
  main <- gsub("\\caption" ,"\\begin{sidewaystable}[!h] \n\\caption",main ,fixed=TRUE)
  return(main)
}



# 3 Preparation ----------------------------------------------------------------
# Extract no of individuals and observations of each complete case dataset
data_list <- Hmisc::llist(dculture, dcinema, dsports, dsocial, dvolunteer, dhelp)
no_indiv <- sapply(data_list, function(x){
  a <- length(unique(x[complete.cases(x), ]$pid))
  return(a)
})
no_obs <- sapply(data_list, function(x){
  a <- nrow(x[complete.cases(x), ])
  return(a)
})



# 4 MODEL 1 --------------------------------------------------------------------
## 4.1 Original regression results ---------------------------------------------
# Create dataframe with original regression estimates (only UE and OLF) 
# from Kunze and Suppa (2007)
model1_orig <- list()
model1_orig[["culture"]] <- data.frame(term  = c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                       original_culture = c("-0.0334three", "(−3.98)",  "−0.0254three", "(−3.68)", "115562", "34640"))         
model1_orig[["cinema"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                      original_cinema = c("-0.0679three", "(−6.96)",  "−0.0733three", "(−8.98)", "115463", "34634"))
model1_orig[["sports"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                      original_sports = c("-0.00804", "(−0.58)",  "−0.0292two", "(−2.30)", "115197", "34618"))
model1_orig[["social"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                      original_social = c("0.0362three", "(3.08)",  "0.0368three", "(4.21)", "115558", "34663"))
model1_orig[["volunteer"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                         original_volunteer = c("-0.00871", "(-0.86)",  "-0.00938", "(-0.98)",  "115301", "34605"))
model1_orig[["help"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                    original_help = c("0.0697three", "(5.86)",  "0.0352three", "(3.66)",  "115465", "34648"))

## 4.2 Reproduce regression results --------------------------------------------
# Run regression for model 1 
depvars <- c("culture", "cinema", "sports", "social", "volunteer", "help")
estimateModel1 <- lapply(depvars, function(var) {
  res <- feols(xpd(..lhs ~ UE+ OLF + age26_30 + age31_35 + age36_40 + age41_45 + age46_50 + 
                     age51_55 + age56_60 + age61_65 + shock_partner + shock_child + shock_sepdiv + 
                     needcare + yearsedu + disabled + married + child1 +  child2 + child3plus + 
                     west | pid + syear, ..cluster = ~ pid, ..lhs = var), data = data)
})
names(estimateModel1) <- depvars
# Get regression table (estimates with sig. levels and t-statistic)
model1_rep <- list()
vars_printed <- 2
for (i in 1:length(estimateModel1)) {
  # Test: 
  # x <- estimateModel1[[2]]
  x <- estimateModel1[[i]]
  name <- names(estimateModel1[i])
  r <- x %>% tidy () %>% MoveTStatistic() %>% SetSigLevels() %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("our_", name)
  r <- as.data.frame(r[1:(vars_printed*2),])
  r[(vars_printed*2+1),1] <- "NoObs"
  r[(vars_printed*2+1),2] <-  x$nobs
  r[(vars_printed*2+2),1] <- "NoIndiv"
  r[(vars_printed*2+2),2] <- no_indiv[i]
  model1_rep[[i]] <- as.data.frame(r)
  names(model1_rep)[i] <- name
  rm(x,r)
}

## 4.3 Extension: Fit Model 1 with imputed missing values ----------------------
# Check if estimatedModel1_imp list is in environment
if (exists("estimateModel1_imp")) {
  print("List estimateModel1_imp is loaded.")
  } else {
    print(paste("List estimateModel1_imp missing. Please run Imputation.R first!"))
  }
# Get regression table (estimates with sig. levels and t-statistic)
hf <- estimateModel1_imp
model1_imp <- list()
for(i in 1:length(hf)) {
  df <- hf[[i]]
  name <- names(hf[i])
  df <- df %>% select(term, estimate, statistic, p.value) 
  df$term <- str_replace(df$term, "1$", "")
  r <- df %>% MoveTStatistic() %>% SetSigLevels()
  r <- r %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("extensionI_", name)
  r <- as.data.frame(r[1:4,])
  model1_imp[[i]] <- as.data.frame(r)
  names(model1_imp)[i] <- name
}
# Check: 
# estimateModel1_imp[[1]][1:2,]
# model1_imp[[1]]

## 4.4 Built table -------------------------------------------------------------
# Panel A: Merge original,reproduction and extension for culture, cinema and sports: 
model1_tableA <-  data.frame(term  = c("UE", "UE_t", "OLF", "OLF_t"))
for (i in 1:3){
  joined <- model1_orig[[i]] %>% full_join( model1_rep[[i]], by = "term" ) %>% 
    full_join( model1_imp[[i]], by = "term" )
  model1_tableA <- model1_tableA %>% full_join(joined, by = "term")
}
# Check: 
# model1_tableA

# Panel B: Merge original,reproduction and extension for socialize, volunteer, help
model1_tableB <-  data.frame(term  = c("UE", "UE_t", "OLF", "OLF_t"))
for (i in 4:6){
  joined <- model1_orig[[i]] %>% full_join( model1_rep[[i]], by = "term" ) %>% 
    full_join( model1_imp[[i]], by = "term" )
  model1_tableB <- model1_tableB %>% full_join(joined, by = "term")
}
# Check: 
# model1_tableB

# Create .tex files 
model1_tableA <- GetTexfile(model1_tableA)
model1_tableB <- GetTexfile(model1_tableB)





# 5 MODEL 2 --------------------------------------------------------------------
## 5.1 Original regression results ---------------------------------------------
# Create dataframe with original regression estimates (only UEPC, UEO, OLF) 
# from Kunze and Suppa (2007)
model2_orig <- list()
model2_orig[["culture"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                       original_culture = c("−0.0495one", "(−1.77)",  "−0.0334three", "(−3.84)", "−0.0250three","(−3.61)", "114276", "34560"))         
model2_orig[["cinema"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                      original_cinema = c("−0.0746two", "(−2.31)",  "−0.0696three",  "(−6.90)",  "−0.0738three",  "(−8.98)", "114176", "34554"))
model2_orig[["sports"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                      original_sports = c("0.0595", "(1.30)", "−0.0136",   "(−0.94)",   "−0.0284two",  "(−2.22)", "113916", "34538"))
model2_orig[["social"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                      original_social = c("0.0866two", "(2.21)",  "0.0305two",  "(2.50)",  "0.0370three", "(4.20)", "114270", "34583"))
model2_orig[["volunteer"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                         original_volunteer = c("−0.0219",  "(−0.57)",  "−0.0107",  "(−1.03)",  "−0.0117",  "(−1.22)", "114015", "34525"))
model2_orig[["help"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv"),
                                    original_help = c("0.0874two", "(2.02)", "0.0687three", "(5.60)", "0.0362three", "(3.74)", "114177", "34568"))

## 5.2 Reproduce regression results ----------------------------------------------
# Run regression for model 2
depvars <- c("culture", "cinema", "sports", "social", "volunteer", "help")
estimateModel2 <- lapply(depvars, function(var) {
  res <- feols(xpd(..lhs ~ UEPC + UEO + OLF + age26_30 + age31_35 + age36_40 + age41_45 + age46_50 + 
                     age51_55 + age56_60 + age61_65 + shock_partner + shock_child + shock_sepdiv + 
                     needcare + yearsedu + disabled + married + child1 +  child2 + child3plus + 
                     west | pid + syear, ..cluster = ~ pid, ..lhs = var), data = data)
})
names(estimateModel2) <- depvars
# Get regression table (estimates with sig. levels and t-statistic)
model2_rep <- list()
vars_printed <- 3
for (i in 1:length(estimateModel2)) {
  # Test: 
  # x <- estimateModel2[[2]]
  x <- estimateModel2[[i]]
  name <- names(estimateModel2[i])
  r <- x %>% tidy () %>% MoveTStatistic() %>% SetSigLevels() %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("our_", name)
  r <- as.data.frame(r[1:(vars_printed*2),])
  r[(vars_printed*2+1),1] <- "NoObs"
  r[(vars_printed*2+1),2] <-  x$nobs
  r[(vars_printed*2+2),1] <- "NoIndiv"
  r[(vars_printed*2+2),2] <- no_indiv[i]
  model2_rep[[i]] <- as.data.frame(r)
  names(model2_rep)[i] <- name
  rm(x,r)
}

## 5.3 Extension: Fit Model 2 with imputed missing values ----------------------
# Check if estimatedModel2_imp list is in environment
if (exists("estimateModel2_imp")) {
  print("List estimateModel2_imp is loaded.")
} else {
  print(paste("List estimateModel2_imp missing. Please run Imputation.R first!"))
}
# Get regression table (estimates with sig. levels and t-statistic)
hf <- estimateModel2_imp
model2_imp <- list()
for(i in 1:length(hf)) {
  df <- hf[[i]]
  name <- names(hf[i])
  df <- df %>% select(term, estimate, statistic, p.value) 
  df$term <- str_replace(df$term, "1$", "")
  r <- df %>% MoveTStatistic() %>% SetSigLevels()
  r <- r %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("extensionI_", name)
  r <- as.data.frame(r[1:6,])
  model2_imp[[i]] <- as.data.frame(r)
  names(model2_imp)[i] <- name
}
# Check: 
# estimateModel2_imp[[1]][1:3,]
# model2_imp[[1]]

## 5.4 Built table -------------------------------------------------------------
# Panel A: Merge original,reproduction and extension for culture, cinema and sports: 
model2_tableA <-  data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t"))
for (i in 1:3){
  joined <- model2_orig[[i]] %>% full_join( model2_rep[[i]], by = "term" ) %>% 
    full_join( model2_imp[[i]], by = "term" )
  model2_tableA <- model2_tableA %>% full_join(joined, by = "term")
}
# Check: 
# model2_tableA

# Panel B: Merge original,reproduction and extension for socialize, volunteer, help
model2_tableB <-  data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t"))
for (i in 4:6){
  joined <- model2_orig[[i]] %>% full_join( model2_rep[[i]], by = "term" ) %>% 
    full_join( model2_imp[[i]], by = "term" )
  model2_tableB <- model2_tableB %>% full_join(joined, by = "term")
}
# Check: 
# model1_tableB

# Create .tex files 
model2_tableA <- GetTexfile(model2_tableA)
model2_tableB <- GetTexfile(model2_tableB)













