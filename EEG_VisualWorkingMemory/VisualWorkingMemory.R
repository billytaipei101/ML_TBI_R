############################
# Importing subjects' info #
############################
devtools::install_github("MI2DataLab/randomForestExplainer",force = TRUE)
library(randomForestExplainer)
setwd('/Volumes/EXTRA_SPACE/EEG_CORE_DATASETS/EEG_VisualWorkingMemory')  # change working directory
subjects_info <- read.xlsx('subjects_info.xlsx', sheet = 1, colNames = TRUE, rowNames = FALSE)
#describeBy(subjects_info$age,group = subjects_info$Group) # not sure whether do a behavior analysis
# selected subjects matching age and sex for a balanced sample
#subjects_info <- read.csv('participants.tsv',sep = '\t',header = T)
#subjects_info <- subjects_info[c(2,3,6,10,12,15,16,24,25,26,31,32,39,50,57,60,62),]
# deleted 1 / 5 / 7 / 8 / 9 / 11 / 13 / 17 / 19 / 23 /27 /28  /29/ 30  51/ 58 / 17 / 27 / 91
subjects_info$participant_id <- factor(subjects_info$participant_id)
subjects_info$sex <- factor(subjects_info$sex, labels = c('Male','Female'))
subjects_info$Group <- factor(subjects_info$Group, labels = c('mTBI','Control'))

# Descriptive statistic of up to two groups classified under a factor variable
descriptive_ver01 <- function(dataset,factore,variables,output){
  d_count <- count(eval(parse(text = dataset)),.(eval(parse(text = factore))));d_count$extra <- c(NA)
  assign(as.character('sample1'),eval(parse(text = paste0("subset(",dataset,", subjects_info$",factore,"== c('",as.character(d_count[1,1]),"'))"))))
  assign(as.character('sample2'),eval(parse(text = paste0("subset(",dataset,", subjects_info$",factore,"== c('",as.character(d_count[2,1]),"'))"))))
  if(nrow(d_count)==2){
    for (j in 1:nrow(d_count)) {
      d_count$extra[j] <- paste0(as.character(d_count[j,1]),' (n = ',as.character(d_count[j,2]),')')
    }
    desc_01 <- data.frame(matrix(data = NA,nrow = length(variables), ncol = 11)); 
    for (j in 1:length(variables)) {
      if(eval(parse(text = paste0('is.factor(',dataset,'$',variables[j],')'))) == FALSE){
        desc_01[j,1] <- as.character(variables[j]) 
        # group 01
        eval(parse(text = paste0('desc_01[j,2] <- mean(sample1$',variables[j],')')))
        eval(parse(text = paste0('desc_01[j,3] <- sd(sample1$',variables[j],')')))
        eval(parse(text = paste0('desc_01[j,4] <- 1.96*sd(sample1$',variables[j],')/sqrt(nrow(sample1))')))
        eval(parse(text = paste0('desc_01[j,5] <- plotrix::std.error(sample1$',variables[j],')')))
        # group 02
        eval(parse(text = paste0('desc_01[j,6] <- mean(sample2$',variables[j],')')))
        eval(parse(text = paste0('desc_01[j,7] <- sd(sample2$',variables[j],')')))
        eval(parse(text = paste0('desc_01[j,8] <- 1.96*sd(sample2$',variables[j],')/sqrt(nrow(sample2))')))
        eval(parse(text = paste0('desc_01[j,9] <- plotrix::std.error(sample2$',variables[j],')')))
        
        eval(parse(text = paste0('inner_t <- t.test(',variables[j],' ~ ', factore,',data =',dataset,')')))
        eval(parse(text = paste0('desc_01[j,10] <- inner_t$statistic[1]')))
        eval(parse(text = paste0('desc_01[j,11] <- inner_t$p.value[1]')))
        label_df <- round(as.numeric(inner_t[2]),0)
      }else if(eval(parse(text = paste0('is.factor(',dataset,'$',variables[j],')'))) == TRUE){
        factor_count1 <- count(sample1,.(eval(parse(text = variables[j]))))
        eval(parse(text = paste0('factor_count1 %>% arrange(desc(freq))')))
        desc_01[j,2] <- round((100/sum(factor_count1[,c(2)]))*factor_count1[1,2],2)
        factor_count2 <- count(sample2,.(eval(parse(text = variables[j]))))
        eval(parse(text = paste0('factor_count2 %>% arrange(desc(freq))')))
        desc_01[j,1] <- paste0(as.character(variables[j]),' (',as.character(factor_count1[1,1]),')') #')(',as.character(factor_count2[1,1]),#  alternatively with two base groups
        desc_01[j,6] <- round((100/sum(factor_count2[,c(2)]))*factor_count2[1,2],2)
      }  
    }
    
    colnames(desc_01)[1:11] <- c('Variable','Mean','(SD)','ci','sem',
                                 'Mean','(SD)','ci','sem',
                                 paste0('t-score (df = ',label_df,')'),'p-value')
    if(output == c('excel')){
      wb <- createWorkbook()
      addWorksheet(wb,"sheet1")
      writeData(wb,"sheet1", d_count[1,3], startCol = 2, startRow = 1, rowNames = FALSE, colNames = FALSE)
      writeData(wb,"sheet1", d_count[2,3], startCol = 6, startRow = 1, rowNames = FALSE, colNames = FALSE)
      writeData(wb,"sheet1", desc_01, startCol = 1, startRow = 2, rowNames = FALSE, colNames = TRUE)
      saveWorkbook(wb,"desc_01.xlsx", overwrite = TRUE)
    }else if(output == c('csv')){
      write.csv(desc_01,"desc_01.csv",row.names = T)
    }

  }else{
    print('There is one or more than two levels in the factor variable introduced for the descriptive analysis')
  }
  return(desc_01)
}
dataset <- 'subjects_info'
factore <- 'Group' # the group variable
variables <-  c('age','sex','correct','incorrect','no_response') # it might not work when name of variables is mistaken, check twice
descriptive_ver01(dataset,factore,variables, output = 'csv')
descriptive_ver01(dataset,factore,variables, output = 'excel')

############################
# pre-processing of files per condition #
############################
setwd('/Volumes/EXTRA_SPACE/EEG_CORE_DATASETS/EEG_VisualWorkingMemory/incorrect') # change for both conditions
condition_label <- 0 # either 0 or , 0 for incorrect and 1 for correct

files <- as.data.frame(matrix(data = list.files(),nrow = length(list.files()),ncol = 1));colnames(files)[1] <- c('files')
files$trial_n <- c(NA)
Fsample <- 500
bands <- c('Delta','Theta','Alpha','Beta','Gamma') 
limits <- matrix(data = c(c(1,3),c(4,7),c(8,12),c(13,30),c(31,100)),
                 nrow = 5, ncol = 2, byrow = TRUE) 
phases <- c('baseline','encoding','retention')
for (i in 1:nrow(files)) {
  dummy <- read.csv(as.character(files$files[i]),sep = ',',header = TRUE)
  sequen <- seq(1,nrow(dummy),2000)
  files$trial_n[i] <- length(sequen) 
  for (j in 1:length(sequen)) {
    if(j == length(sequen)){
      dt <- dummy[c(sequen[j]:nrow(dummy)),]
    }else{
      dt <- dummy[c(sequen[j]:sequen[j+1]-1),]  
    }
    # specify the sample frequency
    electrodes <- as.character(colnames(dt)[2:64])    # electrode vectors
    for (k in 1:length(electrodes)) {
      spec <- specgram(x=eval(parse(text = paste0('dt$',electrodes[k]))),n = 400,Fs=Fsample)
      P=abs(spec$S);t=spec$t
      # imagep(x = t,
      #        y = spec$f,
      #        z = t(P),
      #        #ylim = c(0,100),
      #        col = oce.colorsViridis,
      #        ylab = 'Frequency [Hz]',
      #        xlab = 'Time [s]',
      #        drawPalette = T,
      #        decimate = F) # in case you are interested in visualizing the process
      for(l in 1:length(bands)){
        for(m in 1:length(phases)){
          if(m == 1){
            assign(as.character(paste0(electrodes[k],bands[l],phases[m])),
                   P[limits[l,1]:limits[l,2],c(1:2)])
          }else if(m == 2){
            assign(as.character(paste0(electrodes[k],bands[l],phases[m])),
                   P[limits[l,1]:limits[l,2],c(3:4)])
          }else if(m == 3){
            assign(as.character(paste0(electrodes[k],bands[l],phases[m])),
                   P[limits[l,1]:limits[l,2],c(5:8)])
          }
        }
        
        assign(as.character(paste0(electrodes[k],bands[l])),
               P[limits[l,1]:limits[l,2],])
      }
    }
    # create matrix with names
    for(k in 1:length(electrodes)){
      for(l in 1:length(bands)){
        c_names <- as.data.frame(matrix(data = c(NA),nrow = length(phases)));colnames(c_names)[1] <- c('names')
        for(m in 1:length(phases)){
          c_names$names[m] <- as.character(paste0(electrodes[k],bands[l],phases[m]))          
        }
        if(l == 1){
          my_db_nam0 <- c_names
        }else if(l >= 2){
          my_db_nam0 <- rbind(my_db_nam0,c_names)
        }
      }
      if(k == 1){
        my_db_nam1 <- my_db_nam0
      }else if(k >= 2){
        my_db_nam1 <- rbind(my_db_nam1,my_db_nam0)
      }
      rm('my_db_nam0')
    }
    # compute means by band and phase
    my_db_nam1$names2 <- paste0(my_db_nam1$names,'Mean')
    df <- data.frame(time=seq(1:2)) 
    for(k in 1:nrow(my_db_nam1)){
      if(substr(my_db_nam1$names[k],nchar(my_db_nam1$names[k])-8,nchar(my_db_nam1$names[k])) == c('retention')){
        eval(parse(text = paste0('a <- colMeans(eval(parse(text = my_db_nam1$names[k])))')))
        eval(parse(text = paste0('df$',my_db_nam1$names2[k],' <- a[2:3]')))         
      }else if(substr(my_db_nam1$names[k],nchar(my_db_nam1$names[k])-8,nchar(my_db_nam1$names[k])) != c('retention')){
        eval(parse(text = paste0('df$',my_db_nam1$names2[k],' <- colMeans(eval(parse(text = my_db_nam1$names[k])))')))  
      }
    }
    
    for(k in 1:length(electrodes)){
      rm(list =ls(pat = electrodes[k]))
    }
    
    df$trial <- as.numeric(j)
    if(j == 1){
      df0 <- df
    }else if(j >= 2){
      df0 <- rbind(df0,df)
    }
    rm('df')
  }
  df0$subject <- substr(files$files[i],1,3)
  if(i == 1){
    df1 <- df0
  }else if(i >= 2){
    df1 <- rbind(df1,df0)
  }
  rm('df0')
}
df1$condition <- as.numeric(condition_label)
setwd('/Volumes/EXTRA_SPACE/EEG_CORE_DATASETS/EEG_VisualWorkingMemory')
write.csv(df1, file = "processed_incorrect.csv") # change accordingly

####################
# process all data #
####################
INFO1 <- read.csv('processed_correct.csv',sep = ',',header = T,row.names = NULL)
INFO2 <- read.csv('processed_incorrect.csv',sep = ',',header = T, row.names = NULL)
# extra variables include the following
#' X
#' trial
#' subject
#' time
#' condition
INFO <- rbind.data.frame(INFO1,INFO2)
write.csv(INFO, file = "processed_ALL.csv")
dataAll <- read.csv('processed_ALL.csv',sep = ',',header = T,row.names = NULL) # it only works after you have the two files!!!
dataAll <- subset(dataAll,select = c(-1,-2))
dataAll$condition <- factor(dataAll$condition, labels = c('Incorrect','Correct'))

subjects_info$subject <- as.numeric(substr(subjects_info$participant_id,nchar(as.character(subjects_info$participant_id))-2,nchar(as.character(subjects_info$participant_id))))
dataAll$group <- c(NA)
for (i in 1:nrow(dataAll)) {
  dummy <- subset(subjects_info,subjects_info$subject == dataAll$subject[i])
  dataAll$group[i] <- dummy$Group[1]
}
dataAll$group <- factor(dataAll$group, labels = c('mTBI','Control'))

####
# Models
####
controlOnly <- subset(dataAll,dataAll$group == c('Control'))
mtbiOnly <- subset(dataAll, dataAll$group == c('mTBI'))
correctOnly <- subset(dataAll,dataAll$condition == c('Correct'))
incorrectOnly <- subset(dataAll,dataAll$condition == c('Incorrect'))

# model 1 VWM accuracy for the healthy sample
intrain <- createDataPartition(y = correctOnly$group, p= 0.6, list = FALSE) # before df$SC
training <- correctOnly[intrain,];training <- subset(training,select = -c(condition,subject,trial,time)) # -c(group,subject,trial,time)
testing <- correctOnly[-intrain,];testing <- subset(testing,select = -c(condition,subject,trial,time))

training[,-946] <- round(training[,-946],2)
# alter random forest
Model_rf <- randomForest::randomForest(group ~., data=training,localImp = TRUE) #! modify either group or condition
FeatImpt <- randomForest::importance(Model_rf)
randomForestExplainer::explain_forest(Model_rf, interactions = TRUE, data = training)
#colnames(FeatImpt)=NULL
ImptMatrix <- data.frame(FeatImpt) # "correct"=FeatImpt INI corresponds to the subject label, likely to be saved for all the subjects 
ImptMatrix$channel <- as.character(rownames(FeatImpt))
num_features <- 10 # number of features to select
FeatImptOrdered <- arrange(ImptMatrix,desc(ImptMatrix$MeanDecreaseGini))
FeatImptOrdered <- FeatImptOrdered[c(1:num_features),]

min_depth_frame <- min_depth_distribution(Model_rf)
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

plot_min_depth_distribution(min_depth_frame)
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 10)
plot_min_depth_distribution(min_depth_frame, mean_sample = "top_trees")
importance_frame <- measure_importance(Model_rf,measures = c('mean_min_depth','no_of_nodes','mse_increase','node_purity_increase','no_of_trees','times_a_root','p_value'))
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
plot_multi_way_importance(importance_frame, x_measure = "mean_min_depth", y_measure = "times_a_root", size_measure = "p_value", no_of_labels = 5)

(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
interactions_frame <- min_depth_interactions(Model_rf, vars)
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])
plot_min_depth_interactions(interactions_frame)

rfFrml <- as.formula(paste("group", paste(FeatImptOrdered$channel,sepA = "",  # modify condition/ group
                                                collapse = " + "), sep = " ~ "))
# Random forest model
tc <- trainControl(method = "cv", 3)
modFitP <- train(rfFrml, method = "rf", data = training, trControl = tc,
                 allowParallel=TRUE, importance=TRUE, ntree = 500)
prdval <- predict(modFitP, testing)

cfsMatrix <- confusionMatrix(testing$group, prdval) # modify condition / group
table <- data.frame(confusionMatrix(testing$group,prdval)$table)
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good","bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

plot00 <- ggplot(data = plotTable, mapping = aes(x = Reference,
                                                 y = Prediction,
                                                 fill = goodbad,
                                                 alpha = prop)) +
  geom_tile() + geom_text(aes(label = Freq), vjust = .5, fontface = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  labs(title = 'Confusion Matrix - Diagnosis based on Incorrect trials') +
  theme_classic()+
  xlim(rev(levels(table$Reference)))
plot00

accuracy_rate <- cfsMatrix$overall[[1]]
accuracy_rate
