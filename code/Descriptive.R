# Credits ----------------------------------------------------------------------
'
********************************************************************************
******************** Descriptive.R (master: MASTER.R) **************************
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
# Unlist "subdatasets" and "subdatasets_cc" and create 6x2 dataframes 
# list2env( subdatasets , .GlobalEnv ) 
# list2env( subdatasets_cc , .GlobalEnv ) 
rm(df_crop, i, currentvar, allothermainvars)
}


# 2 Load sub- functions -------------------------------------------------------
specify_decimal <- function(x) trimws(format(round(x, 3), nsmall=3))
getMeans <- function(df) { 
  mainvars <- c("culture", "cinema", "volunteer", "social", "help", "sports") 
  incl <- mainvars %in% names(df)
  i <- which(incl ==TRUE)
  mx <- as.data.frame(matrix(rep(NA, nrow(df)*5), ncol=5))
  names(mx) <- mainvars[which(incl ==FALSE)]
  df <- cbind(df, mx)
  cin <- df  %>% mutate(obs = nrow(df), 
                        ids = length(unique(pid))) %>% 
    select(pid, culture, cinema,volunteer, social, help, sports, 
           EP, UE, OLF, UEPC, UEO, age, yearsedu, 
           disabled, married, child0, child1, child2, 
           child3plus, shock_partner, shock_child, shock_sepdiv, 
           west, needcare, obs, ids)
  names <- c("Culture", "Cinema","Volunteer", "Socialize", "Helping", "Sports",
             "Employed", "Unemployed", "Out of labour force (OLF)",  "Plant closure unemployed", 
             "Other unemployed", "Age (in years)", "Years of education", "Work disability",
             "Married", "Number of children: 0", "Number of children: 1", "Number of children: 2", 
             "Number of children: 3+", "Shock: Spouse died", "Shock: Child born", 
             "Shock: Divorce or separated", "West Germany", "Person needing care in HH", 
             "No. of observations", "No. of individuals")
  names(cin)[-1] <- names
  s <- suppressWarnings(cin %>% 
                          gather(variable, value) %>% 
                          group_by(variable) %>% summarise(`Mean` = mean(value, na.rm = TRUE))  %>%
                          mutate_if(is.numeric , specify_decimal) %>%
                          dplyr::slice(match(names, variable))) %>%
    as.data.frame()
  names(s)[1] <- "Dataset No."
  names(s)[2] <- paste0("(", i, ")")
  s[25,2] <- round(as.numeric(s[25,2]),1)
  s[26,2] <- round(as.numeric(s[26,2]),1)
  
  return(s)
}
getOrigSumStat <- function(){
  culture <- c(1.841, rep(NaN, 5))
  cinema <- c(NaN, 2.045, rep(NaN,4))
  volunteer <- c(NaN, NaN, 1.553, rep(NaN,3))
  social <- c(rep(NaN,3), 3.191, rep(NaN,2))
  help <- c(rep(NaN,4), 2.476, NaN)
  sports <- c(rep(NaN,5), 2.262) 
  EP <- rep(0.724, 6)
  UE <- rep(0.069, 6)
  OLF <- rep(0.207, 6)
  UEPC <- rep(0.004,6)
  UEO <- rep(0.065 , 6)
  age <- c(42.579, 42.563, 42.565, 42.575, 42.562, 42.569)
  yearsedu <- c(12.058, 12.058, 12.060, 12.057, 12.059, 12.057 )
  disabled<- c(0.091, 0.091, 0.090, 0.091, 0.091, 0.091)
  married <- rep(0.660, 6)
  child0 <- rep(0.594, 6)
  child1 <- rep(0.202, 6)
  child2 <- rep(0.152, 6)
  child3plus <- rep(0.052, 6)
  shock_partner <- rep(0.002,6)
  shock_child <- rep(0.031, 6)
  shock_sepdiv <- rep(0.020,6)
  west <- rep(0.744,6)
  needcare <- rep(0.027,6)
  obs <- c(115562, 115463, 115197, 115558 , 115301, 115465)
  ids <- c(34640, 34634, 34618, 34663, 34605, 34648)
  SumStat_orig  <- data.frame(rbind(culture, cinema, volunteer, social,help, sports, 
                                    EP, UE, OLF , UEPC, UEO, age, yearsedu, disabled, 
                                    married, child0, child1, child2, child3plus, 
                                    shock_partner, shock_child, shock_sepdiv, west, 
                                    needcare, obs, ids))  %>%
    mutate_if(is.numeric , specify_decimal) 
  names <- c("Culture", "Cinema", "Volunteer","Socialize", "Helping", "Sports", 
             "Employed", "Unemployed", "Out of labour force (OLF)",  "Plant closure unemployed", 
             "Other unemployed", "Age (in years)", "Years of education", "Work disability",
             "Married", "Number of children: 0", "Number of children: 1", "Number of children: 2", 
             "Number of children: 3+", "Shock: Spouse died", "Shock: Child born", 
             "Shock: Divorce or separated", "West Germany", "Person needing care in HH", 
             "No. of observations", "No. of individuals")
  SumStat_orig2 <-  SumStat_orig
  SumStat_orig2$X6[7:26] <-  SumStat_orig$X3[7:26]   # Column 3 -> Column 6
  SumStat_orig2$X3[7:26] <-  SumStat_orig$X5[7:26]   # Column 5 -> Column 3
  SumStat_orig2$X5[7:26] <-  SumStat_orig$X6[7:26]   # Column 6 -> Column 5
  rm(SumStat_orig)
  SumStat_orig <- SumStat_orig2
  SumStat_orig[25,] <- round(as.integer(SumStat_orig[25,]),1)
  SumStat_orig[26,] <- round(as.integer(SumStat_orig[26,]),1)
  SumStat_orig$X0 <- rownames(SumStat_orig)
  rownames(SumStat_orig) <- NULL
  names(SumStat_orig) <- c( "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "Dataset No.")
  SumStat_orig[] <- lapply(SumStat_orig, as.character)
  
  return(SumStat_orig)
}
MakeCellsBlue <- function(df){
  is_numeric<- grepl("NaN",df)
  df[!is_numeric] <-   sub("^", "BlueCell", df[!is_numeric])
  return(df)
}
SetComma <- function(df){
  has_fourplusdigits <- nchar(df)>=4
  df[has_fourplusdigits] <- paste0(
    str_sub(df, start= 1, end = (unique(nchar(df))-3)), ",",
    str_sub(df, start= -3))
}



# 3 CREATE TABLES --------------------------------------------------------------
## 3.1 Comparison Summary Statistics -------------------------------------------
sumstat_solo <- lapply(subdatasets_cc, getMeans)
names(sumstat_solo) <- c(rep("",6))
sumstat_our <- sumstat_solo %>% reduce(left_join, by = "Dataset No.")
sumstat_our[25:26,-1] <- as.data.frame(apply(sumstat_our[25:26,-1],2, SetComma))
sumstat_our[,-1] <- as.data.frame(apply(sumstat_our[,-1],2, MakeCellsBlue))
sumstat_orig <- getOrigSumStat()
sumstat_orig[25:26,-7] <- as.data.frame(apply(sumstat_orig[25:26,-7],2, SetComma))
sumstat_all <- rbind(sumstat_our, sumstat_orig)
sumstat_all <- sumstat_all[kronecker(1:nrow(sumstat_orig), c(0, nrow(sumstat_orig)), "+"), ]
for (i in 2:nrow(sumstat_all)){
  if(i %% 2 == 0){
    sumstat_all$`Dataset No.`[i] <- ""
  }
}
rownames(sumstat_all) <- NULL
# Create Latex output
options(knitr.table.format = "latex")
tex_sumstat <- kable(sumstat_all, booktabs = TRUE, caption = "Comparison of summary Statistics in Complete Case", 
              align = "lcccccc", row.names = FALSE, linesep = "") %>% 
  add_header_above(c(" ", "Mean Values of Reproduction vs. Original"=6)) %>% 
  kable_styling(latex_options = "hold_position")
tex_sumstat <- gsub("NaN" ,"",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("0." ,".",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("parencite" ,"\\parencite{main}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Culture" ,"\\textbf{Culture}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Cinema" ,"\\textbf{Cinema}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Sports" ,"\\textbf{Sports}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Socialize" ,"\\textbf{Socialize}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Volunteer" ,"\\textbf{Volunteer}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Helping" ,"\\textbf{Helping}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("+" ,"$+$",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("BlueCell" ,"\\BlueCell{20}",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("\\begin{table}[!h]" ,"\\usepackage{booktabs} \n\\usepackage[table]{xcolor} \n \\newcommand\\BlueCell[1] {\\cellcolor{blue!#1!white}}" ,
                    tex_sumstat,fixed=TRUE)
tex_sumstat <- gsub("\\caption" ,"\\begin{table}[!h] \n\\caption",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub(".027\\\\" ,".027\\\\ \n \\\\[-1.8ex] \\hline \\\\[-1.8ex] ",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("\\bottomrule" ,"\\hline \\hline \\\\[-2ex]  ",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("\\toprule" ,"\\hline \\hline \\\\[-2ex]  ",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Dataset No." ,"\\textit{Dataset No.}  ",tex_sumstat ,fixed=TRUE)
tex_sumstat <- gsub("Employed" ,"\\\\[-1.8ex] \n Employed ",tex_sumstat ,fixed=TRUE)
tex_sumstat
# remove irrelevant variables and dataframes from global console
rm(i,sumstat_solo, sumstat_all, sumstat_orig, sumstat_our)
# Test: 
mean(subdatasets_cc[[1]]$UEPC) + mean(subdatasets_cc[[1]]$UEO) == mean(subdatasets_cc[[1]]$UE)





## 3.2 Appendix B: Summary statistics ------------------------------------------
sumtables <- list()
main_vars <- c("culture", "cinema", "volunteer", "social","help", "sports")
library(xtable)
for(i in 1:length(subdatasets)){
  currvar <- main_vars[i]
  data_stats <- subdatasets[[i]] %>% 
    select(pid, all_of(currvar),  EP, UE, OLF, UEPC, UEO, age, yearsedu, disabled, married, child0, 
           child1, child2, child3plus, shock_partner, shock_child, shock_sepdiv, 
           west, needcare)
# Add number of obs per variable 
vars <- names(data_stats)[-1]
n_obs <- c()
for(j in 1:length(vars)){
  currentvar <- vars[j]
  no_nas <- sum(is.na(data_stats[, currentvar]))
  no_obsv <-  nrow(data_stats) - no_nas
  n_obs[j] <- no_obsv %>% as.character() %>% SetComma()
}
# Add number of individuals per variable 
n_indiv <- c()
for(j in 1:length(vars)){
  currentvar <- vars[j]
  n_indiv[j] <- data_stats %>% drop_na(all_of(currentvar)) %>% select(pid) %>% distinct() %>% 
    count() %>% as.character() %>% SetComma()
}
cov_names <- c(str_to_title(currvar), "Employed", "Unemployed", "Out of labour force (OLF)",  "Plant closure unemployed", 
           "Other unemployed", "Age (in years)", "Years of education", "Work disability",
           "Married", "Number of children: 0", "Number of children: 1", "Number of children: 2", 
           "Number of children: 3+", "Shock: Spouse died", "Shock: Child born", 
           "Shock: Divorce or separated", "West Germany", "Person needing care in HH")
names(data_stats)[-1] <- cov_names
# An output is created that shows a complete descriptive statistic with: 
#  min, p20, mean, sd, median, p75, max, n_obs, n_indiv
table_summarystats <- data_stats %>% select(!pid) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>% summarise(`Min` = as.character(min(value, na.rm = TRUE)), 
                                   `p25` = as.character(quantile(value, probs= c(0.25),na.rm = TRUE)), 
                                   `Mean` = as.character(round(mean(value, na.rm = TRUE),4)),
                                   `SD` = as.character(round(sd(value, na.rm = TRUE),4)),
                                   `Median` = as.character(median(value, na.rm = TRUE)),
                                   `p75` = as.character(quantile(value, probs= c(0.75),na.rm = TRUE)),
                                   `Max` = as.character(max(value, na.rm = TRUE))) %>%
                                   #`No. of obs.` = sum(!is.na(value))) %>% 
  suppressWarnings() %>% 
  slice(match(cov_names, variable))
table_summarystats$`No. of obs.` <- n_obs
table_summarystats$`Individuals` <- n_indiv
sumtables[[i]] <- xtable(table_summarystats,type = "latex", method = "compact",
                               caption = '\\textbf{...}')
}
names(sumtables) <- main_vars



# remove irrelevant variables and dataframes from global console
rm( table_summarystats, cov_names, currentvar, currvar, i, j, main_vars, 
    n_indiv, n_obs, no_nas, no_obsv, vars, data_stats) 













