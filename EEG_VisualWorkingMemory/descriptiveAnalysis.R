###########################
# R settings and packages #
###########################
library(pacman)
pacman::p_load(readr,rgl,eegkit,eegkitdata,edfReader,signal,car,oce,caret,psych,
               GGally,pracma,tidyverse,kernlab,plyr,openxlsx,ez,apaTables,tibble,plyr,
               tree,reprtree,plotrix,openxlsx,dplyr,MatchIt,tableone,knitr,showtext)
font_add_google("Lexend Deca")

######################################
# Adding information to the subjects #
######################################
my_home <- c('/Volumes/EXTRA_SPACE/EEG_CORE_DATASETS/EEG_VisualWorkingMemory/ds003523') # CHANGE TO /ds003523 location
setwd(my_home)
files_all <- as.data.frame(matrix(data = list.files(), nrow = length(list.files()),ncol = 1));colnames(files_all)[1] <- c('files')
files_all <- subset(files_all,substr(files_all$files,1,5) == c('sub-0'))
subject_2 <- as.data.frame(matrix(data = NA, nrow = nrow(files_all),ncol = 17)); colnames(subject_2)[1:17] <- c('participant_id','correct','incorrect','total','no_response','3R_count_correct','3R_time_correct','3R2Y_count_correct','3R2Y_time_correct','5R_count_correct','5R_time_correct','3R_count_incorrect','3R_time_incorrect','3R2Y_count_incorrect','3R2Y_time_incorrect','5R_count_incorrect','5R_time_incorrect')
for (i in 1:nrow(files_all)) {
  
  home_dummy <- paste0(my_home,'/',files_all$files[i],'/eeg')
  setwd(home_dummy)
  name_dummy <- paste0(files_all$files[i],'_ses-01_task-VisualWorkingMemory_events.tsv')
  unique_info <- read.csv(name_dummy,sep = '\t',header = T) # for i == 1 is stim_info, for i = 2 is value
  unique_info$plus2 <- unique_info$plus1 <- c(NA)
  subject_2$participant_id[i] <- as.character(files_all$files[i])
  if(!('stim_info' %in% colnames(unique_info)) && sum(is.na(unique_info$value)) == nrow(unique_info)){
    for(j in 1:nrow(unique_info)){
      if(unique_info$stim_file[j] == 'S200' || unique_info$stim_file[j] == 'S201'){
        unique_info$plus1[j] <- unique_info[j,1]-unique_info[j-2,1]
        unique_info$plus2[j] <- as.character(unique_info[j-2,4])
      }
    }
    dd <- dplyr::count(unique_info,stim_file)
    dd1 <- unique_info %>%
      dplyr::filter(stim_file %in% c("S200","S201")) %>%
      group_by(stim_file,plus2) %>%
      dplyr::summarise(mean_r = mean(plus1),count_sti = length(plus2))
    
  }else if(!('stim_info' %in% colnames(unique_info)) && sum(is.na(unique_info$value)) == 0){
    for(j in 1:nrow(unique_info)){
      if(unique_info$value[j] == 'S200' || unique_info$value[j] == 'S201'){
        unique_info$plus1[j] <- unique_info[j,1]-unique_info[j-2,1]
        unique_info$plus2[j] <- as.character(unique_info[j-2,4])
      }
    }
    dd <- dplyr::count(unique_info,value)
    dd1 <- unique_info %>%
      dplyr::filter(value %in% c("S200","S201")) %>%
      group_by(value,plus2) %>%
      dplyr::summarise(mean_r = mean(plus1),count_sti = length(plus2))
    
  }else if('stim_info' %in% colnames(unique_info)){
    for(j in 1:nrow(unique_info)){
      if(unique_info$stim_info[j] == 'S200' || unique_info$stim_info[j] == 'S201'){
        unique_info$plus1[j] <- unique_info[j,1]-unique_info[j-2,1]
        unique_info$plus2[j] <- as.character(unique_info[j-2,4])
      }
    }
    dd <- dplyr::count(unique_info,stim_info)
    dd1 <- unique_info %>%
      dplyr::filter(stim_info %in% c("S200","S201")) %>%
      group_by(value,plus2) %>%
      dplyr::summarise(mean_r = mean(plus1),count_sti = length(plus2))
  }else{
    print(glue('There was an unknown error processing the file of subject {files_all$files[i]}'))
  }
  
  if(exists('dd') == T){
    subject_2$correct[i] <- as.numeric(dd[dd[,1] == 'S201',2])  ## modified for making sense of figures, original 'S200'
    subject_2$incorrect[i] <- as.numeric(dd[dd[,1] == 'S200',2]) ## modified for making sense of figures, original 'S201'
    subject_2$total[i] <- subject_2$correct[i] + subject_2$incorrect[i]
    subject_2$no_response[i] <- 150 - subject_2$total[i]
  }
  # if(exists('dd1') == T && nrow(dd1) == 6){
  #   subject_2[i,6] <- as.numeric(dd1[1,4])
  #   subject_2[i,7] <- dd1[1,3]
  #   subject_2[i,8] <- dd1[2,4]
  #   subject_2[i,9] <- dd1[2,3]
  #   subject_2[i,10] <- dd1[3,4]
  #   subject_2[i,11] <- dd1[3,3]
  #   subject_2[i,12] <- dd1[4,4]
  #   subject_2[i,13] <- dd1[4,3]
  #   subject_2[i,14] <- dd1[5,4]
  #   subject_2[i,15] <- dd1[5,3]
  #   subject_2[i,16] <- dd1[6,4]
  #   subject_2[i,17] <- dd1[6,3]
  # }
  if(exists('dd1') == T && nrow(dd1) == 6){
    subject_2[i,6] <- as.numeric(dd1[4,4])
    subject_2[i,7] <- as.numeric(dd1[4,3])
    subject_2[i,8] <- as.numeric(dd1[5,4])
    subject_2[i,9] <- as.numeric(dd1[5,3])
    subject_2[i,10] <- as.numeric(dd1[6,4])
    subject_2[i,11] <- as.numeric(dd1[6,3])
    subject_2[i,12] <- as.numeric(dd1[1,4])
    subject_2[i,13] <- as.numeric(dd1[1,3])
    subject_2[i,14] <- as.numeric(dd1[2,4])
    subject_2[i,15] <- as.numeric(dd1[2,3])
    subject_2[i,16] <- as.numeric(dd1[3,4])
    subject_2[i,17] <- as.numeric(dd1[3,3])
  }
  rm('dd','dd1')
}

subject_2$participant_id <- as.factor(subject_2$participant_id)
name_dummy <- paste0(my_home,'/participants.tsv')
participants <- read.csv(name_dummy,sep = '\t',header = T) # for i == 1 is stim_info, for i = 2 is value
subjects_info <- inner_join(participants,subject_2, by = c("participant_id"))
subjects_info$Group <- factor(subjects_info$Group,labels = c('mTBI','Control'))
subjects_info$sex <- factor(subjects_info$sex,labels = c('Male','Female'))

subjects_info <- subset(subjects_info,!(is.na(subjects_info$`3R_count_correct`)))

subjects_info$ratio_3R <- subjects_info$`3R_count_correct`/(subjects_info$`3R_count_correct`+subjects_info$`3R_count_incorrect`)
subjects_info$ratio_5R <- subjects_info$`5R_count_correct`/(subjects_info$`5R_count_correct`+subjects_info$`5R_count_incorrect`)
subjects_info$ratio_3R2Y <- subjects_info$`3R2Y_count_correct`/(subjects_info$`3R2Y_count_correct`+subjects_info$`3R2Y_count_incorrect`) 

set.seed(1234)
match.it <- matchit(Group ~ age + sex + correct, data = subjects_info, method="nearest", ratio=1)
a <- summary(match.it)
subjects_info <- match.data(match.it)[1:ncol(subjects_info)]
setwd(my_home)

### correct ratio
dt_01 <- subset(subjects_info,select = c(1,4:8))
dt_01$ratio <- dt_01$correct/(dt_01$correct+dt_01$incorrect)
### false alarm
dt_02 <- subset(subjects_info,select = c(1,4:8))
dt_02$ratio <- dt_02$incorrect/(dt_02$correct+dt_02$incorrect)

table1 <- CreateTableOne(vars = c('age', 'sex', 'ratio'), 
                         data = dt_01, 
                         factorVars = 'sex', 
                         strata = 'Group')
table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table1[,1:3],  
      align = 'c', 
      caption = 'Table 1: Comparison of total score in VWM of matched samples')

View(subjects_info) # This displays the subjects that conformed the final sample for the analysis

# descriptivo01 <- ddply(subjects_info,.(Group),summarise, mean_ = mean(correct), sd_ = sd(correct),ci_ = 1.96*sd_/sqrt(length(correct)), se_ = sd_/sqrt(length(correct)))
# descriptivo01$type <- c('correct')
# descriptivo02 <- ddply(subjects_info,.(Group),summarise, mean_ = mean(incorrect), sd_ = sd(incorrect), ci_ = 1.96*sd_/sqrt(length(incorrect)), se_ = sd_/sqrt(length(incorrect)))
# descriptivo02$type <- c('incorrect')
# descriptivo <- rbind(descriptivo01,descriptivo02)
# descriptivo$type <- as.factor(descriptivo$type)

descriptivo_00_hit <- ddply(dt_01,.(Group),summarise, mean_ = mean(ratio), sd_ = sd(ratio),
                            ci_ = 1.96*sd_/sqrt(length(ratio)),
                            se_ = sd_/sqrt(length(ratio)),
                            min_ = 0.5, max_ = 1)
descriptivo_00_hit$class <- 'Hit Rate'


descriptivo_00_false <- ddply(dt_02,.(Group),summarise, mean_ = mean(ratio), sd_ = sd(ratio),
                              ci_ = 1.96*sd_/sqrt(length(ratio)),
                              se_ = sd_/sqrt(length(ratio)),
                              min_ = 0,max_ = 0.4)
descriptivo_00_false$class <- 'False Alarm'
descriptivo_00 <- rbind(descriptivo_00_hit,descriptivo_00_false)
descriptivo_00$class <- as.factor(descriptivo_00$class)

(fact_plot01 <- ggplot(data=descriptivo_00_hit, aes(x = Group,
                                                    y = mean_, fill = Group)) + 
    #facet_wrap(~ class,ncol = 2,scales = "free_y") + # ,scales = "free_y"
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin= mean_ - se_, ymax= mean_ + se_), width = 0.4, position = position_dodge(.9), linetype = 1) +
    annotate("segment", x=c(1,1,2),xend=c(2,1,2), y= c(0.85,0.848,0.848), yend=c(0.85,0.85,0.85))+
    annotate("text",x=1.5,y=0.86,label="p = 0.5 n.s.")+
    #coord_cartesian(ylim = c(0.1,1))+
    #geom_line(aes(linetype = type, position = pd)) +
    #geom_point(aes(shape = type, position = mean_),size = 4) +
    #guides(linetype = guide_legend('type')) +
    labs(title = paste0("Hit Ratio in VWM task"),
         x='Group',y=paste0('Ratio'))+coord_cartesian(ylim = c(0.75,0.875))
)
fact_plot01 + theme_light()

(fact_plot02 <- ggplot(data=descriptivo_00_false, aes(x = Group,
                                                      y = mean_, fill = Group)) + 
    #facet_wrap(~ class,ncol = 2,scales = "free_y") + # ,scales = "free_y"
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin= mean_ - se_, ymax= mean_ + se_), width = 0.4, position = position_dodge(.9), linetype = 1) +
    #coord_cartesian(ylim = c(0.1,1))+
    #geom_line(aes(linetype = type, position = pd)) +
    #geom_point(aes(shape = type, position = mean_),size = 4) +
    #guides(linetype = guide_legend('type')) +
    labs(title = paste0("False Alarm Ratio in VWM task by Group"),
         x='Group',y=paste0('Ratio'))+coord_cartesian(ylim = c(0.1,0.3))
)
fact_plot02 + theme_light()

dt_01 <- subset(subjects_info,select = c(1,6,23));colnames(dt_01)[3] <- c('ratio');dt_01$condition <- '3 Red'
dt_02 <- subset(subjects_info,select = c(1,6,24));colnames(dt_02)[3] <- c('ratio');dt_02$condition <- '5 Red'
dt_03 <- subset(subjects_info,select = c(1,6,25));colnames(dt_03)[3] <- c('ratio');dt_03$condition <- '3 Red + 2 yellow'

dt <- rbind.data.frame(dt_01,dt_02,dt_03)
dt$condition <- as.factor(dt$condition)
descriptivo_01 <- ddply(dt,.(Group,condition),summarise, mean_ = mean(ratio), sd_ = sd(ratio), ci_ = 1.96*sd_/sqrt(length(ratio)), se_ = sd_/sqrt(length(ratio)))

anova_results <- ezANOVA(data= dt,
                         dv = ratio, wid = .(participant_id), between = .(Group),within = .(condition))
setwd(my_home)
anova_table <- apa.ezANOVA.table(anova_results,filename = "AOV_table2.doc")  # there should be a new file called that with the ANOVA results
dt$crossed_group <- as.factor(paste(dt$Group,'-',dt$condition))
anova_results <- aov(ratio ~ crossed_group * condition,data = dt)
posthoc      <-  TukeyHSD(anova_results, 'crossed_group', conf.level=0.95)

(fact_plot2 <- ggplot(data=descriptivo_01, aes(x = Group,
                                               y = mean_, group = condition, fill = condition)) + 
    #facet_grid(condition ~ .) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin= mean_ - se_, ymax= mean_ + se_), width = 0.4, position = position_dodge(.9), linetype = 1) +
    #geom_line(aes(linetype = type, position = pd)) +
    #geom_point(aes(shape = type, position = mean_),size = 4) +
    guides(linetype = guide_legend('type')) +
    labs(title = paste0(""), #Hit Rate by stimulus type in VWM task 
         x='Group',y=paste0('Mean'))+coord_cartesian(ylim = c(0.70,1))+
    annotate("segment", x=c(1.7,1.7,2),xend=c(2,1.7,2), y= c(0.94,0.93,0.93), yend=c(0.94,0.94,0.94))+
    annotate("segment", x=c(0.7,0.7,1.3),xend=c(1.3,0.7,1.3), y= c(0.94,0.93,0.93), yend=c(0.94,0.94,0.94))+
    annotate("text",x=1.85,y=0.955,label="p < 0.05")+
    annotate("text",x=0.95,y=0.955,label="p < 0.01")
)
fact_plot2 + theme_light()

dt_01 <- subset(subjects_info,select = c(1,6,12));colnames(dt_01)[3] <- c('time');dt_01$condition <- '3 Red';dt_01$outcome <- 'correct' #correct
dt_02 <- subset(subjects_info,select = c(1,6,14));colnames(dt_02)[3] <- c('time');dt_02$condition <- '3 Red + 2 yellow';dt_02$outcome <- 'correct' #correct
dt_03 <- subset(subjects_info,select = c(1,6,16));colnames(dt_03)[3] <- c('time');dt_03$condition <- '5 Red';dt_03$outcome <- 'correct' #correct
dt_04 <- subset(subjects_info,select = c(1,6,18));colnames(dt_04)[3] <- c('time');dt_04$condition <- '3 Red';dt_04$outcome <- 'incorrect' #correct
dt_05 <- subset(subjects_info,select = c(1,6,20));colnames(dt_05)[3] <- c('time');dt_05$condition <- '3 Red + 2 yellow';dt_05$outcome <- 'incorrect' #correct
dt_06 <- subset(subjects_info,select = c(1,6,22));colnames(dt_06)[3] <- c('time');dt_06$condition <- '5 Red';dt_06$outcome <- 'incorrect' #correct
dt <- rbind.data.frame(dt_01,dt_02,dt_03,dt_04,dt_05,dt_06)
dt$condition <- as.factor(dt$condition)
dt$outcome <- as.factor(dt$outcome)
descriptivo_02 <- ddply(dt,.(Group,condition,outcome),summarise, mean_ = mean(time), sd_ = sd(time), ci_ = 1.96*sd_/sqrt(length(time)), se_ = sd_/sqrt(length(time)))

(fact_plot3 <- ggplot(data=descriptivo_02, aes(x = Group,
                                               y = mean_, group = condition,fill = condition)) + 
    facet_wrap(outcome ~ .,scales = "free_y") +
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin= mean_ - se_, ymax= mean_ + se_), width = 0.4, position = position_dodge(.9), linetype = 1) +
    coord_cartesian(ylim = c(3.1,3.7))+
    #geom_line(aes(linetype = type, position = pd)) +
    #geom_point(aes(shape = type, position = mean_),size = 4) +
    guides(linetype = guide_legend('type')) +
    labs(title = paste0(""), # Mean RT in VWM
         x='Group',y=paste0('Mean'))
)
fact_plot3 + theme_light()

# Color selection
colors <- c("#FDAE61", # Orange
            "#66BD63") # Darker green

# Reorder the factor levels
reordered_groups <- factor(subjects_info$Group, levels = c("mTBI",
                                                           "Control"))
# Scatter plot
plot(subjects_info$age,subjects_info$correct,
     main = c('Correct Score in VWM by Age'),
     xlab = c('Age'), ylab = c('Count Correct'),
     pch = 19,
     col = colors[reordered_groups])

abline(v=c(35), col=c("blue"), lty=c(2), lwd=c(3))
# Legend
legend("bottomleft",
       legend = c("mTBI", "Control"),
       pch = 19,
       col = colors[factor(levels(reordered_groups))]) 

subjects_info <- subjects_info %>%
  mutate(age_group = case_when(age >= 35 ~ "Younger than 35",
                               age < 35 ~ "35 or older") %>%
           as.factor() %>%
           structure(levels = c('Younger than 35','35 or older'))
  )

dt_01 <- subset(subjects_info,select = c(1,6,23,26));colnames(dt_01)[3] <- c('ratio');dt_01$condition <- '3 Red'
dt_02 <- subset(subjects_info,select = c(1,6,24,26));colnames(dt_02)[3] <- c('ratio');dt_02$condition <- '5 Red'
dt_03 <- subset(subjects_info,select = c(1,6,25,26));colnames(dt_03)[3] <- c('ratio');dt_03$condition <- '3 Red + 2 yellow'

dt <- rbind.data.frame(dt_01,dt_02,dt_03)
dt$condition <- as.factor(dt$condition)
descriptivo_03 <- ddply(dt,.(Group,condition,age_group),summarise, mean_ = mean(ratio), sd_ = sd(ratio), ci_ = 1.96*sd_/sqrt(length(ratio)), se_ = sd_/sqrt(length(ratio)))

(fact_plot4 <- ggplot(data=descriptivo_03, aes(x = Group,
                                               y = mean_, group = condition, fill = condition)) + 
    facet_grid(age_group ~ .) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin= mean_ - se_, ymax= mean_ + se_), width = 0.3, position = position_dodge(.9), linetype = 1) +
    #geom_line(aes(linetype = type, position = pd)) +
    #geom_point(aes(shape = type, position = mean_),size = 4) +
    coord_cartesian(ylim = c(0.60,1))+
    guides(linetype = guide_legend('type')) +
    labs(title = paste0(""), # Hit Ratio in VWM task by age
         x='Group',y=paste0('Mean'))
)

fact_plot4 + theme_light()

