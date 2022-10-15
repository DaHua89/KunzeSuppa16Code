# Credits ----------------------------------------------------------------------
'
********************************************************************************
********************* GetFigure.R (master: MASTER.R) **************************
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
    print(paste("File", i, "missing. Please run GetDataset.R first!"))
  }
  list2env( df , .GlobalEnv )
}
rm(df)

# 2 Subset data frame ----------------------------------------------------------
# Include only variables with missing values 
dataset_na <- main_dataset %>% select(cultureNA, cinemaNA, volunteerNA, 
                                     socialNA, helpNA, sportsNA, yearseduNA, 
                                     marriedNA, disabledNA, shock_partnerNA, 
                                     shock_childNA, shock_sepdivNA, needcareNa) 
dataset_na[dataset_na >= 0] <- NA
dataset_na$x <- c(rep(c(-5,-3,-1,NA), 15703))
# Re-arange dataframe 
hf <- data.frame(matrix(vector(), 3, 0))
for(i in 1:length(colnames(dataset_na))) {
  currcol <- dataset_na[i]
  s <- c(length(which(currcol == -5)), length(which(currcol == -3)), 
         length(which(currcol == -1)))
  hf <- suppressWarnings(cbind(hf,s))
  colnames(hf)[i] <- colnames(dataset_na)[i]
}
hf$x <- c(-5,-3,-1)
p <- melt(hf, id.vars = "x")

# 3 Make plot ------------------------------------------------------------------
new_names <- c(
  `cultureNA` = paste0("Culture (N:",hf$cultureNA[3], ")"),
  `cinemaNA` = paste0("Cinema (N:",hf$cinemaNA[3], ")"),
  `volunteerNA` = paste0("Volunteer (N:",hf$volunteerNA[3], ")"),
  `socialNA` = paste0("Socialize (N:",hf$socialNA[3], ")"),
  `helpNA` = paste0("Helping (N:",hf$helpNA[3], ")"),
  `sportsNA` = paste0("Sports (N:",hf$sportsNA[3], ")"),
  `yearseduNA` = paste0("Years of Education (N:",hf$yearseduNA[3], ")"),
  `marriedNA` = paste0("Married (N(-3):",hf$marriedNA[2], "; N(-1):",hf$marriedNA[3],")"),
  `disabledNA` = paste0("Disabled (N(-3):",hf$disabledNA[2], "; N(-1):",hf$disabledNA[3],")"),
  `shock_partnerNA` = paste0("Shock: Partner died (N:",hf$shock_partnerNA[1], ")"),
  `shock_childNA` = paste0("Shock: Child born (N(-5):",hf$shock_childNA[1], "; N(-1):",hf$shock_childNA[3],")"),
  `shock_sepdivNA` = paste0("Shock: Divorced/ Separated (N:",hf$shock_sepdivNA[1], ")"),
  `needcareNa` = paste0("Person needing care in HH (N:",hf$needcareNa[3], ")")
)

barplot <- ggplot(p, aes(x = as.factor(x), y = value)) + 
  geom_bar(stat = "identity") + theme_bw() + 
  theme(axis.title.x= element_text(colour = "gray40", size = 9),
        axis.title.y = element_text(colour = "gray40", size = 9),
        axis.text = element_text(size = 7),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs( x = "Categories of missing values ", y = "No. of missing values (absolut)")
barplot <- barplot + facet_wrap(~variable, scales = "free",  ncol = 3,  labeller = as_labeller(new_names))



# remove irrelevant variables and dataframes from global console
rm(currcol, hf, p, dataset_na, i, new_names, s, x, toload) 






