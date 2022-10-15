# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************* Inductive.R (master: MASTER.R) ***************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v37, teaching version [DOI: 10.5684/soep.core.v37t])
Note:       Please refer to the MASTER.R file to run the present R script. 
'

# 1 Load in data ---------------------------------------------------------------
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
  # list2env( subdatasets_cc , .GlobalEnv ) 






# 2 Load sub- functions --------------------------------------------------------
SetComma <- function(df){
  has_fourplusdigits <- nchar(df)>=4
  df[has_fourplusdigits] <- paste0(
    str_sub(df, start= 1, end = (unique(nchar(df))-3)), ",",
    str_sub(df, start= -3))
}
MoveTStatistic <- function(df){
  x <- df %>% select(term, estimate, p.value) %>% mutate(estimate = as.character(round(estimate, 4)))
  xx <- df %>% select( term, statistic) %>% mutate( p.value = rep(NA, nrow(x)), 
                                                    term = paste0(term, "_t")) 
  names(xx)[2] <- "estimate"
  xx <- xx %>% mutate(estimate = paste0("(",as.character(round(estimate, 2)), ")"))
  
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
GetTexfile <- function(df, model){
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
 
  # Create .tex file
  options(knitr.table.format = "latex")
  main <- kable(df, booktabs = TRUE, caption = paste("Summary of Regression Estimates for Model", model), 
                col.names = c("",rep(c("\\textit{orig.}", "\\textit{repl.}", "\\textit{imp.}"),rep_no)),
                align = "lcccccc", row.names = FALSE, linesep = "", escape = F) %>% 
    kableExtra::add_header_above(data_frame(x = header_names, y = c(1,rep(ncols, rep_no))), bold = TRUE) %>% 
    kable_styling(latex_options = "hold_position")
  main <- gsub("one" ,"^{*}",main ,fixed=TRUE)
  main <- gsub("two" ,"^{**}",main ,fixed=TRUE)
  main <- gsub("three" ,"^{***}",main,fixed=TRUE) 
  main <- gsub("table" ,"sidewaystable",main,fixed=TRUE) 
  main <- gsub("\\begin{sidewaystable}[!h]" ,"\\usepackage{booktabs, rotating, tabularx} \n\\usepackage[table]{xcolor} \n \\newcolumntype{Y}{>{\\raggedright\\arraybackslash}X}" ,
               main,fixed=TRUE)
  main <- gsub("\\caption" ,"\\begin{sidewaystable}[!h] \n\\caption",main ,fixed=TRUE)
  main <- gsub("\\end{tabular}" ,"\\end{tabularx}",main ,fixed=TRUE)
  main <- gsub("\\begin{tabular}[t]{lcccccclcc}" ,"\\begin{tabularx}{\\linewidth}{l l*{9}{Y}}",main ,fixed=TRUE)
  main <- gsub("NoObs" ,"\\midrule No. of Obs.",main ,fixed=TRUE)
  main <- gsub("NA" ,"-",main ,fixed=TRUE)
  main <- gsub("NoIndiv" ,"Individuals",main ,fixed=TRUE) 
  main <- gsub("AdjR2" ,"Adj. R^2",main ,fixed=TRUE) 
  main <- gsub("OLF" ,"Out of labour force",main ,fixed=TRUE) 
  main <- gsub("UEPC" ,"Unemployed due to plant closure",main ,fixed=TRUE) 
  main <- gsub("UEO" ,"Unemployed due to other reasons",main ,fixed=TRUE) 
  main <- gsub("UE" ,"Unemployed",main ,fixed=TRUE) 
  main
  return(main)
}



# 3 Preparation ----------------------------------------------------------------
# Make complete case data set 
data <- main_dataset_cc <- main_dataset[complete.cases(main_dataset),]

# Extract no of individuals and observations of each complete case dataset
data_list <- Hmisc::llist(dculture, dcinema, dsports, dsocial, dvolunteer, dhelp)
no_indiv_completeCase <- sapply(data_list, function(x){
  a <- length(unique(x[complete.cases(x), ]$pid)) 
  a <- a %>% SetComma
  return(a)
})
no_obs_completeCase <- sapply(data_list, function(x){
  a <- nrow(x[complete.cases(x), ])
  a <- a %>% SetComma
  return(a)
})
no_obs <- sapply(data_list, function(x){
  a <- nrow(x)
  a <- a %>% SetComma
  return(a)
})
no_indiv <- sapply(data_list, function(x){
  a <- length(unique(x$pid))
  a <- a %>% SetComma
  return(a)
})


# 4 MODEL 1 --------------------------------------------------------------------
## 4.1 Original regression results ---------------------------------------------
# Create dataframe with original regression estimates (only UE and OLF) 
# from Kunze and Suppa (2007)
model1_orig <- list()
model1_orig[["culture"]] <- data.frame(term  = c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                       original_culture = c("-0.0334three", "(-3.98)",  "-0.0254three", "(-3.68)", "115,562", "34,640", NA))        
model1_orig[["cinema"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                      original_cinema = c("-0.0679three", "(-6.96)",  "-0.0733three", "(-8.98)", "115,463", "34,634", NA))
model1_orig[["sports"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                      original_sports = c("-0.00804", "(-0.58)",  "-0.0292two", "(-2.30)", "115,197", "34,618", NA))
model1_orig[["social"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                      original_social = c("0.0362three", "(3.08)",  "0.0368three", "(4.21)", "115,558", "34,663", NA))
model1_orig[["volunteer"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                         original_volunteer = c("-0.00871", "(-0.86)",  "-0.00938", "(-0.98)",  "115,301", "34,605", NA))
model1_orig[["help"]] <- data.frame(term  =  c("UE", "UE_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                    original_help = c("0.0697three", "(5.86)",  "0.0352three", "(3.66)",  "115,465", "34,648", NA))

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
  adjr <- round(glance(x)$adj.r.squared, 4)
  r <- x %>% tidy () %>% MoveTStatistic() %>% SetSigLevels() %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("our_", name)
  r <- as.data.frame(r[1:(vars_printed*2),])
  r[(vars_printed*2+1),1] <- "NoObs"
  r[(vars_printed*2+1),2] <-  no_obs_completeCase[i]
  r[(vars_printed*2+2),1] <- "NoIndiv"
  r[(vars_printed*2+2),2] <- no_indiv_completeCase[i]
  r[(vars_printed*2+3),1] <- "AdjR2"
  r[(vars_printed*2+3),2] <-  adjr
  model1_rep[[i]] <- as.data.frame(r)
  names(model1_rep)[i] <- name
  rm(x,r)
}

## 4.3 Extension: Fit Model 1 with imputed missing values ----------------------
# Check if estimatedModel1_imp list is in environment
if (exists("estimateModel1_imp")) {
  print("List estimateModel1_imp is loaded.")
  } else { 
    stop(paste("List estimateModel1_imp missing. Please run Imputation.R first!"))
}
# Get regression table (estimates with sig. levels and t-statistic)
hf <- estimateModel1_imp
model1_imp <- list()
for(i in 1:length(hf)) {
  df <- hf[[i]]
  name <- names(hf[i])
  adjr <- df %>% select(adjr) %>% distinct %>% round(digits = 4) %>% as.character()
  df <- df %>% select(term, estimate, statistic, p.value)
  df$term <- str_replace(df$term, "1$", "")
  r <- df %>% MoveTStatistic() %>% SetSigLevels()
  r <- r %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("extensionI_", name)
  r <- as.data.frame(r[1:4,])
  rownames(r) <- NULL
  r[nrow(r)+1,] <- c("NoObs", no_obs[i])
  r[nrow(r)+1,] <- c("NoIndiv", no_indiv[i])
  r[nrow(r)+1,] <- c("AdjR2", adjr)
  
  model1_imp[[i]] <- as.data.frame(r)
  names(model1_imp)[i] <- name
}
# Check: 
# estimateModel1_imp[[1]][1:2,]
# model1_imp[[1]]


## 4.4 Built table -------------------------------------------------------------
# Panel A: Merge original,reproduction and extension for public activities (i.e., culture, cinema, volunteer)
tab1A <-  data.frame(term  = c("UE", "UE_t", "OLF", "OLF_t"))
for (i in c(1:2,5)){
  joined <- model1_orig[[i]] %>% full_join( model1_rep[[i]], by = "term" ) %>% 
    full_join( model1_imp[[i]], by = "term" )
  tab1A <- tab1A %>% full_join(joined, by = "term")
}
# Check: 
tab1A

# Panel B: Merge original,reproduction and extension for social/public activities (i.e. sports, socialize, help)
tab1B <-  data.frame(term  = c("UE", "UE_t", "OLF", "OLF_t"))
for (i in c(4,6,3)){
  joined <- model1_orig[[i]] %>% full_join( model1_rep[[i]], by = "term" ) %>% 
    full_join( model1_imp[[i]], by = "term" )
  tab1B <- tab1B %>% full_join(joined, by = "term")
}
# Check: 
tab1B
# Bind tables: 
# Create .tex files 
model1_tableA <- GetTexfile(tab1A, model = 1)
model1_tableA
model1_tableB <- GetTexfile(tab1B, model = 1)
model1_tableB



# 5 MODEL 2 --------------------------------------------------------------------
## 5.1 Original regression results ---------------------------------------------
# Create dataframe with original regression estimates (only UEPC, UEO, OLF) 
# from Kunze and Suppa (2007)
model2_orig <- list()
model2_orig[["culture"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                       original_culture = c("-0.0495one", "(-1.77)",  "-0.0334three", "(-3.84)", "-0.0250three","(-3.61)", "114,276", "34,560", NA))        
model2_orig[["cinema"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                      original_cinema = c("-0.0746two", "(-2.31)",  "-0.0696three",  "(-6.90)",  "-0.0738three",  "(-8.98)", "114,176", "34,554", NA))
model2_orig[["sports"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                      original_sports = c("0.0595", "(1.30)", "-0.0136",   "(-0.94)",   "-0.0284two",  "(-2.22)", "113,916", "34,538", NA))
model2_orig[["social"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                      original_social = c("0.0866two", "(2.21)",  "0.0305two",  "(2.50)",  "0.0370three", "(4.20)", "114,270", "34,583", NA))
model2_orig[["volunteer"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                         original_volunteer = c("-0.0219",  "(-0.57)",  "-0.0107",  "(-1.03)",  "-0.0117",  "(-1.22)", "114,015", "34,525", NA))
model2_orig[["help"]] <- data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t", "NoObs", "NoIndiv", "AdjR2"),
                                    original_help = c("0.0874two", "(2.02)", "0.0687three", "(5.60)", "0.0362three", "(3.74)", "114,177", "34,568", NA))

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
  adjr <- round(glance(x)$adj.r.squared, 4)
  r <- x %>% tidy () %>% MoveTStatistic() %>% SetSigLevels() %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("our_", name)
  r <- as.data.frame(r[1:(vars_printed*2),])
  r[(vars_printed*2+1),1] <- "NoObs"
  r[(vars_printed*2+1),2] <-  no_obs_completeCase[i]
  r[(vars_printed*2+2),1] <- "NoIndiv"
  r[(vars_printed*2+2),2] <- no_indiv_completeCase[i]
  r[(vars_printed*2+3),1] <- "AdjR2"
  r[(vars_printed*2+3),2] <-  adjr
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
  adjr <- df %>% select(adjr) %>% distinct %>% round(digits = 4) %>% as.character()
  df <- df %>% select(term, estimate, statistic, p.value) 
  df$term <- str_replace(df$term, "1$", "")
  r <- df %>% MoveTStatistic() %>% SetSigLevels()
  r <- r %>% select(term, estimate) 
  names(r)[names(r)== "estimate"] <- paste0("extensionI_", name)
  r <- as.data.frame(r[1:6,])
  rownames(r) <- NULL
  r[nrow(r)+1,] <- c("NoObs", no_obs[i])
  r[nrow(r)+1,] <- c("NoIndiv", no_indiv[i])
  r[nrow(r)+1,] <- c("AdjR2", adjr)
  model2_imp[[i]] <- as.data.frame(r)
  names(model2_imp)[i] <- name
}
# Check: 
# estimateModel2_imp[[1]][1:3,]
# model2_imp[[1]]

## 5.4 Built table -------------------------------------------------------------
# Panel A: Merge original,reproduction and extension for culture, cinema and sports: 
tab2A <-  data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t"))
for (i in c(1:2,5)){
  joined <- model2_orig[[i]] %>% full_join( model2_rep[[i]], by = "term" ) %>% 
    full_join( model2_imp[[i]], by = "term" )
  tab2A <- tab2A %>% full_join(joined, by = "term")
}
# Check: 
tab2A

# Panel B: Merge original,reproduction and extension for socialize, volunteer, help
tab2B <-  data.frame(term  = c("UEPC", "UEPC_t", "UEO", "UEO_t", "OLF", "OLF_t"))
for (i in c(4,6,3)){
  joined <- model2_orig[[i]] %>% full_join( model2_rep[[i]], by = "term" ) %>% 
    full_join( model2_imp[[i]], by = "term" )
  tab2B <- tab2B %>% full_join(joined, by = "term")
}
# Check: 
# model1_tableB

# Create .tex files 
model2_tableA <- GetTexfile(tab2A, model = 2)
model2_tableB <- GetTexfile(tab2B, model = 2)


# remove irrelevant variables and dataframes from global console
rm(vars_printed, no_obs, no_obs_completeCase, no_indiv_completeCase, no_indiv, 
   name, i, depvars, adjr, r, tab1A, tab1B, joined, hf, dvolunteer, 
   dcinema, dculture, dsocial, dsports, dhelp, df, tab2A, tab2B, data_list, data, 
   main_dataset_cc)














