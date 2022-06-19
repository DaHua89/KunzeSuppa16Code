# Figure3 - Versuch ------------------------------------------------------------
d <- data_all
# For efficiency reasons, apply the following for-loop only on individuals(pid), which ...
# (1) ... have at least one time the entry "UE==1".  Consequently excludes all indiviuals and 
# their observations, if they are employed over the whole time period (UE==0 for all entries).
pid_f1 <- d %>% arrange(pid, syear) %>% group_by(pid) %>%  filter(UE ==1 ) %>%
  select(pid) %>% distinct() 
# Save pid_f1 as vector
pid_f1 <- as.vector(pid_f1$pid)
# (2) ... have at least 8 entries that the times t=-4,t=-3, t=-2, t=-1, t=1, t=2, t=3, t=4 
# can be denoted to. Consequently all individuals with less than 8 entries are excluded. 
pid_f2 <- d %>%  group_by(pid) %>% dplyr::count() %>% filter(n >8) %>% select(pid) %>% distinct() 
# Save pid_f2 as vector
pid_f2 <- as.vector(pid_f2$pid)

# Select all entries of individulas(pid) that are element of pid_f1 and pid_f2
pid_select <- intersect(pid_f2, pid_f1)

# Create empty dataframe "dfigure3" which is to be filled in the next loop 
dfigure3 <- data.frame(matrix(ncol = 4, nrow = 0)) 
for(i in 1:length(pid_select)){
  #which(pid_select == "13802")
  pid_nr <- pid_select[i]
  crop <- d  %>% select(pid, syear, EP, UE, OLF, sports, culture, social, volunteer, cinema, help) %>% 
    dplyr::mutate(UE = 2*UE, UE = UE + EP) %>% # recode UE variable (this line could be commented out)
    #mutate(UE = (EP-1)*(-1)) %>% # recode EP variable (ask me, if you would like to know more)
    filter(pid == pid_nr)
  
  # recode NA in "UE" variable to value 9 
  crop$UE[is.na(crop$UE)] <- 9 
  # create empty variable that is to be filled with -4,-3,-2,-1,1,2,3,4
  crop$time <- NA  
  # grasp all UE entries  for each pid, e.g. for firm with pid == "5201" : 
  # "2111112000000" (with recoded UE) / "1000001000000" (without recoded UE)
  entries <- paste0(crop$UE, collapse="")
  entries
  
  # look for specific patterns: (comment out the one you would like to run)
  # (A dot in a regular expression stands for ALL possible entries)
  # a) for recoded UE: 
  #pattern <- "(?<=[1,2]{3})1(?=2{4})" # sss12222, whereas s:= 1 or 2
  #pattern <- "(?<=[1,2]{3})1(?=2{1}[1,2]{3})" # sss12sss, whereas s:= 1 or 2
  #pattern <- "(?<=1{3})1(?=2{1}[1,2]{3})" # 11112sss, whereas x:= 1 or 2 
  pattern <- "(?<=.{3})1(?=2{4})" # xxx12222, whereas x:= 1 or 2 or 0
  #pattern <- "(?<=.{3})1(?=2{1}.{3})" # xxx12xxx, whereas x:= 1 or 2 or 0
  #pattern <- "(?<=1{3})1(?=2{1}.{3})" # 11112xxx, whereas x:= 1 or 2 or 0
  #pattern <- "(?<=1{3})1(?=2{4})" # 11112222
  # b) for original UE: 
  #pattern <- "(?<=.{3})0(?=1{4})" # xxx01111, whereas x:= 1 or 0
  #pattern <- "(?<=.{3})0(?=1{1}.{3})" # xxx01xxx, whereas x:= 1 or 0
  #pattern <- "(?<=0{3})0(?=1{1}.{3})" # 00001xxx, whereas x:= 1 or 0
  #pattern <- "(?<=0{3})0(?=1{4})" # 00001111
  
  p <- str_locate_all(entries, pattern) 
  p_rownr <- do.call(rbind, p)[,1] # save row number of forth position
  p_rownr
  
  # Add values to variable "time": 
  if(length(p_rownr)>0){ 
    for (i in 1:length(p_rownr)) {
      row <- p_rownr[i] 
      crop$time[row] <- -1 # add -1 to forth position
      crop$time[row-3] <- -4 # add -4 to first position 
      crop$time[row-2] <- -3 # add -3 to second position
      crop$time[row-1] <- -2 # add -2 to third position
      crop$time[row+1] <- 1 # add 1 to fifth position
      crop$time[row+2] <- 2 # add 2 to sixth position
      crop$time[row+3] <- 3 # add 3 to seventh position
      crop$time[row+4] <- 4 # add 4 to eighth position
      dfigure3 <- rbind(dfigure3, crop[(row-3):(row+4),]) 
      
    }
  }
}

' 
1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
-4,   -3,   -2,   -1    1,    2,    3,    4
      -4,   -3,   -2,   -1    1,    2,    3,    4
            -4,   -3,   -2,   -1    1,    2,    3,    4
                  -4,   -3,   -2,   -1    1,    2,    3,    4
                         -4,  -3,   -2,   -1    1,    2,    3,    4
                              -4,   -3,   -2,   -1    1,    2,    3,    4
                                    -4,   -3,   -2,   -1    1,    2,    3,    4
                                          -4,   -3,   -2,   -1    1,    2,    3,    4
                                                -4,   -3,   -2,   -1    1,    2,    3,    4
                                                       -4,   -3,   -2,   -1    1,    2,    3,    4
                                                             -4,   -3,   -2,   -1    1,    2,    3,    4
                                                                   -4,   -3,   -2,   -1    1,    2,    3,    4
                                                                         -4,   -3,   -2,   -1    1,    2,    3,    4
                                                                               -4,   -3,   -2,   -1    1,    2,    3,    4
'
# In case the waves are expanded to include the waves c(1992, 1994, 1995, 1996, 1997, 1998,2001, 2003, 2005, 2007, 
# 2008, 2009, 2011), then the next filter could also be added. It makes sure that the transition from 
# employment to unemployment happens in the years 1996-1997 or 1997-1998. In both cases we have enough 
# years to plot t=-4 to t=4. 
# pid_select2 <- figure3 %>% filter(UE==1 & syear %in% c(1997, 1998)) %>% select(pid) %>% distinct()
# pid_select2 <- as.vector(# pid_select2$pid)
# figure3 <- figure3 %>% filter(pid %in% # pid_select2) 


# Plot the mean over time in time-to-event-plot:
# (1) Plot them all together: 
library(gridExtra) # Please DO NOT put into Initialization, as some functions of dplyr will be overwritten (e.g. mutate()) and lead to errors in the code.
n <- c("social", "help", "cinema", "culture", "volunteer", "sports")
g <- lapply(1:6, function(j) {
  if(n[j] %in% c("social", "help")){
    ggplot(dfigure3, aes(x= time), y= get(n[j])) +
      stat_summary(aes(y = get(n[j])  , group=1), fun=mean, colour="red", geom="line", group=1) + 
      scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
      #coord_cartesian(ylim = c(1, 2.4)) + 
      coord_cartesian(ylim = c(2, 3.4)) +
      ylab(n[j]) + labs(title=paste(str_remove_all(pattern, "[\\(\\),<>=?]"),n[j]))
  }else {
    ggplot(dfigure3, aes(x= time), y= get(n[j])) +
      stat_summary(aes(y = get(n[j])  , group=1), fun=mean, colour="red", geom="line", group=1) + 
      scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
      coord_cartesian(ylim = c(1, 2.4)) + 
      #coord_cartesian(ylim = c(2, 3.4)) +
      ylab(n[j]) + labs(title=paste(str_remove_all(pattern, "[\\(\\),<>=?]"),n[j]))
  }
  
})
grid.arrange(grobs = g, ncol = 2)

# (2) Plot them separately: 
# Please comment out the pair of n and d which an output should be created for:
n <- "culture"
# n <- "cinema"
# n <- "sports"
# n <- "social"
# n <- "volunteer"
# n <- "help"
i <- as.integer(unlist(dfigure3[which(names(dfigure3) == n)]))
dfigure3 %>% ggplot(aes(x= time), y=  i) +
  stat_summary(aes(y = i, group=1), fun=mean, colour="red", geom="line", group=1) + 
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
  coord_cartesian(ylim = c(1, 2.4)) + # if social or help is plotted, please change this line with the next line
  # coord_cartesian(ylim = c(2, 3.4)) +
  ylab(n) + labs(title=str_remove_all(pattern, "[\\(\\),<>=?]"))






