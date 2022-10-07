# Credits ----------------------------------------------------------------------
'
********************************************************************************
******************** Descriptive.R (master: MASTER.R) **************************
********************************************************************************
Authors:    T. Bethge, I. Fetzer, M. Paul  
Data:       SOEP (v30, teaching version [DOI: 10.5684/soep.v30])
Note:       Please refer to the MASTER.R file to run the present R script. 
'

## 1 Load sub- functions -------------------------------------------------------
specify_decimal <- function(x) trimws(format(round(x, 3), nsmall=3))
getMeans <- function(df) { 
  mainvars <- c("culture", "cinema", "sports", "social", "volunteer", "help") 
  incl <- mainvars %in% names(df)
  i <- which(incl ==TRUE)
  mx <- as.data.frame(matrix(rep(NA, nrow(df)*5), ncol=5))
  names(mx) <- mainvars[which(incl ==FALSE)]
  df <- cbind(df, mx)
  cin <- df  %>% mutate(obs = nrow(df), 
                        ids = length(unique(pid))) %>% 
    select(pid, culture, cinema, sports, social, volunteer, help, 
           EP, UE, OLF, UEPC, UEO, age, yearsedu, 
           disabled, married, child0, child1, child2, 
           child3plus, shock_partner, shock_child, shock_sepdiv, 
           west, needcare, obs, ids)
  names <- c("Culture", "Cinema","Sports", "Socialize", "Volunteer", "Helping", 
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
                          slice(match(names, variable))) %>%
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
  sports <- c(NaN, NaN, 2.262, rep(NaN,3))
  social <- c(rep(NaN,3), 3.191, rep(NaN,2))
  volunteer <- c(rep(NaN,4), 1.553, NaN)
  help <- c(rep(NaN,5), 2.476)
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
  SumStat_orig  <- data.frame(rbind(culture, cinema, sports, social, volunteer, help, 
                                    EP, UE, OLF , UEPC, UEO, age, yearsedu, disabled, 
                                    married, child0, child1, child2, child3plus, 
                                    shock_partner, shock_child, shock_sepdiv, west, 
                                    needcare, obs, ids))  %>%
    mutate_if(is.numeric , specify_decimal) 
  names <- c("Culture", "Cinema","Sports", "Socialize", "Volunteer", "Helping", 
             "Employed", "Unemployed", "Out of labour force (OLF)",  "Plant closure unemployed", 
             "Other unemployed", "Age (in years)", "Years of education", "Work disability",
             "Married", "Number of children: 0", "Number of children: 1", "Number of children: 2", 
             "Number of children: 3+", "Shock: Spouse died", "Shock: Child born", 
             "Shock: Divorce or separated", "West Germany", "Person needing care in HH", 
             "No. of observations", "No. of individuals")
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



# 2. CREATE TABLES -------------------------------------------------------------
## 2.1 Summary Statistics as .tex file -----------------------------------------
sumstat_solo <- lapply(datasets, getMeans)
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
tex_sumstat <- kable(sumstat_all, booktabs = TRUE, caption = "Comparison of Summary Statistics", 
              align = "lcccccc", row.names = FALSE, linesep = "") %>% 
  add_header_above(c(" ", "Mean Values of our analysis vs.parencite "=6)) %>% 
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

# remove irrelevant variables and dataframes from global console
rm(i,sumstat_solo, main_vars, n)






