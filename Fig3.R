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

## ------------------ 2nd approach

#Optionally, one can exchange the years for data_all to c(1992, 1994, 1995, 1996,   
#                                        1997, 1998, 1999, 2001, 2003, 2005, 
#                                       2007, 2008, 2009, 2011) and proceed as follows:
data = data_all

subsel = aggregate((1-data$UE), by = list(as.factor(data$pid)), prod)

#Now, subsel consists of the product of all employment indicators, leaving us with 0 for everyone who at any one point was unemployed. 

index1 = subsel[subsel$x == 0,]

sub2 = data[data$pid %in% index1$Group.1,]

#sub2 is now our dataset with all people who at at least one point have been unemployed

#further filtering:

subsel2 = aggregate((sub2$UE), by = list(as.factor(sub2$pid)), prod)

#Now, subsel2 consists of the product of all employment indicators that were left, 
#leaving us with 0 for everyone who at any one point was unemployed.

index2 = subsel2[subsel2$x == 0,]

sub3 = sub2[sub2$pid %in% index2$Group.1,]

#Now we have filtered out all people who have been without work for the whole period

#Next, we will filter out all people whose first value is not a 1.

index3 =  aggregate((sub3$UE), by = list(as.factor(sub3$pid)), mean)

list = index3$Group.1

list = as.numeric(as.character(list))

length(list)

help = rep(NA,length(list))

#install.packages("foreach")

library(foreach)

foreach (i = list, j = 1:length(list)) %do% {
  
    dat = sub3[sub3$pid == i,]
    
    pattern = c(1,1,1,1,0,0,0,0,0)
  
    help[j] = grepl(toString(pattern),toString(dat$EP))

}

#Now we have created a variable that contains TRUE values for all ped's that contain the pattern

pedpattern = list[help]

sub4 = sub3[sub3$pid %in% pedpattern,]

#338 people with 6124 measurements show this pattern

#Now we can move on to our last step, the a 

library(stringr)

i = 1

helpframe = data.frame(matrix(0, ncol = 6, nrow = 9))

for (i in pedpattern) {
  
  dat2 = sub4[sub4$pid == i,]
  
  sequence = paste(dat2$EP, collapse = "")
  
  match = str_locate_all(sequence, "111100000")
  
  matches = do.call(rbind, match)
  
  finaldata = dat2[matches[1]:matches[2],]
  
  finaldata = finaldata[,25:30]
  
  helpframe = cbind(helpframe, finaldata)
  
}

finalframe = helpframe[,7:2334]

T1 = as.numeric(finalframe[1,])
T2 = as.numeric(finalframe[2,])
T3 = as.numeric(finalframe[3,])
T4 = as.numeric(finalframe[4,])
T5 = as.numeric(finalframe[5,])
T6 = as.numeric(finalframe[6,])
T7 = as.numeric(finalframe[7,])
T8 = as.numeric(finalframe[8,])
T9 = as.numeric(finalframe[9,])

cult1 = mean(T1[seq(1, length(T1), 6)], na.rm = T)
cult2 = mean(T2[seq(1, length(T1), 6)], na.rm = T)
cult3 = mean(T3[seq(1, length(T1), 6)], na.rm = T)
cult4 = mean(T4[seq(1, length(T1), 6)], na.rm = T)
cult5 = mean(T5[seq(1, length(T1), 6)], na.rm = T)
cult6 = mean(T6[seq(1, length(T1), 6)], na.rm = T)
cult7 = mean(T7[seq(1, length(T1), 6)], na.rm = T)
cult8 = mean(T8[seq(1, length(T1), 6)], na.rm = T)
cult9 = mean(T9[seq(1, length(T1), 6)], na.rm = T)

plot(c(cult1, cult2, cult3, cult4, cult5, cult6, cult7, cult8, cult9), type = "b", ylim=c(1,2.5))

cint1 = mean(T1[seq(2, length(T1), 6)], na.rm = T)
cint2 = mean(T2[seq(2, length(T1), 6)], na.rm = T)
cint3 = mean(T3[seq(2, length(T1), 6)], na.rm = T)
cint4 = mean(T4[seq(2, length(T1), 6)], na.rm = T)
cint5 = mean(T5[seq(2, length(T1), 6)], na.rm = T)
cint6 = mean(T6[seq(2, length(T1), 6)], na.rm = T)
cint7 = mean(T7[seq(2, length(T1), 6)], na.rm = T)
cint8 = mean(T8[seq(2, length(T1), 6)], na.rm = T)
cint9 = mean(T9[seq(2, length(T1), 6)], na.rm = T)

plot(c(cint1, cint2, cint3, cint4, cint5, cint6, cint7, cint8, cint9), type = "b", ylim=c(1,2.5))

sport1 = mean(T1[seq(3, length(T1), 6)], na.rm = T)
sport2 = mean(T2[seq(3, length(T1), 6)], na.rm = T)
sport3 = mean(T3[seq(3, length(T1), 6)], na.rm = T)
sport4 = mean(T4[seq(3, length(T1), 6)], na.rm = T)
sport5 = mean(T5[seq(3, length(T1), 6)], na.rm = T)
sport6 = mean(T6[seq(3, length(T1), 6)], na.rm = T)
sport7 = mean(T7[seq(3, length(T1), 6)], na.rm = T)
sport8 = mean(T8[seq(3, length(T1), 6)], na.rm = T)
sport9 = mean(T9[seq(3, length(T1), 6)], na.rm = T)

plot(c(sport1, sport2, sport3, sport4, sport5, sport6, sport7, sport8, sport9), type = "b", ylim=c(1,2.5))

soct1 = mean(T1[seq(4, length(T1), 6)], na.rm = T)
soct2 = mean(T2[seq(4, length(T1), 6)], na.rm = T)
soct3 = mean(T3[seq(4, length(T1), 6)], na.rm = T)
soct4 = mean(T4[seq(4, length(T1), 6)], na.rm = T)
soct5 = mean(T5[seq(4, length(T1), 6)], na.rm = T)
soct6 = mean(T6[seq(4, length(T1), 6)], na.rm = T)
soct7 = mean(T7[seq(4, length(T1), 6)], na.rm = T)
soct8 = mean(T8[seq(4, length(T1), 6)], na.rm = T)
soct9 = mean(T9[seq(4, length(T1), 6)], na.rm = T)

plot(c(soct1, soct2, soct3, soct4, soct5, soct6, soct7, soct8, soct9), type = "b", ylim=c(1,2.5))

#helping and volunteer ommited for space reasons




