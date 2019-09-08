library(randomForest)
library(vcd)
library(plotly)
library(pROC)

# import data
setwd("C:/Users/geitb/Documents/SynVar/Proefschrift/JC_AJN")
# biblical data
dat <- read.csv("jc_nojc_bib.csv")
# extrabiblical data
datx <- read.csv("jc_nojc_xbib.csv")
dath <- rbind(dat, datx)
# select hebrew data
dath <- dath[dath$language %in% c('Hebrew','hbo'),]

#remove PP-subject
dath <- dath[dath$subj_type != 'PP',]
head(dath)
dim(dath)
table(dath$cl_type)

# select relevant columns
colnames(dath)
selected_cols <- c("cl_type", "cl_id", "book", "chapter", "verse", "ebh_lbh", "genre",
                   "txt_type", "s_p_order", "CP", "cl_len", "subj_len", "pc_len",
                   "main_sub", "Loca", "QUes", "Rela", "Time", "mother")
dat.jc.nojc <- dath[, selected_cols]

#label ABH as 'abh' and 'poetry'
dat.jc.nojc$ebh_lbh <- factor(dat.jc.nojc$ebh_lbh)
dat.jc.nojc$book.ch <- paste(dat.jc.nojc$book, dat.jc.nojc$chapter, sep = '_')
dat.jc.nojc$ebh_lbh <- as.character(dat.jc.nojc$ebh_lbh)
#dat6$ebh_lbh[dat6$book.ch == 'Genesis_49' & as.numeric(dat6$verse) > 1 & as.numeric(dat6$verse) < 28] <- 'abh'
#dat6$ebh_lbh[dat6$book.ch == 'Exodus_15' & as.numeric(dat6$verse) > 1 & as.numeric(dat6$verse) < 19] <- 'abh'
#dat6$ebh_lbh[dat6$book.ch == 'Numbers_23' & as.numeric(dat6$verse) > 6] <- 'abh'
##dat6$ebh_lbh[dat6$book.ch == 'Numbers_24' & as.numeric(dat6$verse) < 25] <- 'abh'
#dat6$ebh_lbh[dat6$book.ch == 'Deuteronomy_32' & as.numeric(dat6$verse) < 44] <- 'abh'
#dat6$ebh_lbh[dat6$book.ch == 'Deuteronomy_33' & as.numeric(dat6$verse) > 2] <- 'abh'
#dat6$ebh_lbh[dat6$book.ch == 'Judges_5' & as.numeric(dat6$verse) > 1 & as.numeric(dat6$verse) < 31] <- 'abh'

#dat6$ebh_lbh[dat6$book.ch == '2_Samuel_22' & as.numeric(dat6$verse) > 2] <- 'abh'
#dat6$ebh_lbh[dat6$book.ch == '2_Samuel_1' & as.numeric(dat6$verse) > 18 & as.numeric(dat6$verse) < 28] <- 'abh'
dat.jc.nojc$ebh_lbh <- as.factor(dat.jc.nojc$ebh_lbh)

dat.jc.nojc$genre <- as.character(dat.jc.nojc$genre)
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Genesis_49' & as.numeric(dat.jc.nojc$verse) > 1 & as.numeric(dat.jc.nojc$verse) < 28] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Exodus_15' & as.numeric(dat.jc.nojc$verse) > 1 & as.numeric(dat.jc.nojc$verse) < 19] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Numbers_6' & as.numeric(dat.jc.nojc$verse) > 23 & as.numeric(dat6$verse) < 27] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Numbers_21' & as.numeric(dat.jc.nojc$verse) > 27 & as.numeric(dat6$verse) < 31] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Numbers_23' & as.numeric(dat.jc.nojc$verse) > 7 & as.numeric(dat6$verse) < 11] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Numbers_23' & as.numeric(dat.jc.nojc$verse) > 18 & as.numeric(dat6$verse) < 25] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Numbers_24' & as.numeric(dat.jc.nojc$verse) > 3 & as.numeric(dat6$verse) < 10] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Numbers_24' & as.numeric(dat.jc.nojc$verse) > 15 & as.numeric(dat6$verse) < 25] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Deuteronomy_32' & as.numeric(dat.jc.nojc$verse) < 44] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Deuteronomy_33' & as.numeric(dat.jc.nojc$verse) > 2] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Judges_5' & as.numeric(dat.jc.nojc$verse) > 1 & as.numeric(dat.jc.nojc$verse) < 31] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == 'Judges_9' & as.numeric(dat.jc.nojc$verse) > 7 & as.numeric(dat.jc.nojc$verse) < 16] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == '1_Samuel_2' & as.numeric(dat.jc.nojc$verse) < 11] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == '2_Samuel_22' & as.numeric(dat.jc.nojc$verse) > 2] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == '2_Samuel_23' & as.numeric(dat.jc.nojc$verse) > 2 & as.numeric(dat.jc.nojc$verse) < 8] <- 'poetry'
dat.jc.nojc$genre[dat6$book.ch == '2_Samuel_1' & as.numeric(dat.jc.nojc$verse) > 18 & as.numeric(dat.jc.nojc$verse) < 28] <- 'poetry'
dat.jc.nojc$genre <- as.factor(dat.jc.nojc$genre)

dat.jc.nojc$genre[dat6$book.ch == 'Jonah_2' & as.numeric(dat.jc.nojc$verse) > 3 & as.numeric(dat.jc.nojc$verse) < 11] <- 'poetry'

dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Daniel_2' & as.numeric(dat.jc.nojc$verse) > 19 & as.numeric(dat.jc.nojc$verse) < 24] <- 'poetry'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Daniel_8' & as.numeric(dat.jc.nojc$verse) > 22 & as.numeric(dat.jc.nojc$verse) < 27] <- 'poetry'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Daniel_12' & as.numeric(dat.jc.nojc$verse) < 4] <- 'poetry'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Nehemiah_9' & as.numeric(dat.jc.nojc$verse) > 5 & as.numeric(dat.jc.nojc$verse) < 38] <- 'poetry'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == '1_Chronicles_16' & as.numeric(dat.jc.nojc$verse) > 7 & as.numeric(dat.jc.nojc$verse) < 37] <- 'poetry'

dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Job_1' & as.numeric(dat.jc.nojc$verse) < 21] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Job_2' & as.numeric(dat.jc.nojc$verse) < 14] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Job_42' & as.numeric(dat.jc.nojc$verse) > 6 & as.numeric(dat.jc.nojc$verse) < 18] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Isaiah_36' & as.numeric(dat.jc.nojc$verse) < 23] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Isaiah_37' & as.numeric(dat.jc.nojc$verse) < 22] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Isaiah_38' & as.numeric(dat.jc.nojc$verse) < 10] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Isaiah_38' & as.numeric(dat.jc.nojc$verse) > 20] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Isaiah_39'] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Jeremiah_52'] <- 'prose'
dat.jc.nojc$genre[dat.jc.nojc$book.ch == 'Jeremiah_39' | dat.jc.nojc$book.ch == 'Jeremiah_40'] <- 'prose'

dat.jc.nojc <- dat.jc.nojc[dat.jc.nojc$ebh_lbh != 'epigraphic',]
dat.jc.nojc <- droplevels(dat.jc.nojc)


#################################################################################

# language phase
phase <- table(dat.jc.nojc$ebh_lbh, dat.jc.nojc$cl_type)
colnames(phase) <- c('????', 'no-????')
mosaicplot(phase, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# main and subordinate clauses
m.s <- table(dat.jc.nojc$main_sub, dat.jc.nojc$cl_type)
colnames(m.s) <- c('????', 'no-????')
mosaicplot(m.s, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# discourse
genre <- table(dat.jc.nojc$txt_type, dat.jc.nojc$cl_type)
colnames(genre) <- c('????', 'no-????')
mosaicplot(genre, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# genre
genre <- table(dat.jc.nojc$genre, dat.jc.nojc$cl_type)
colnames(genre) <- c('????', 'no-????')
mosaicplot(genre, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# make spineplot of clause length versus clause type
len_table <- table(dat.jc.nojc$cl_len, dat.jc.nojc$cl_type)
colnames(len_table) <- c('????', 'no-????')
spineplot(len_table, main = '')

# genre and discourse together
mosaic(structable(dat.jc.nojc$genre, dat.jc.nojc$txt_type, dat.jc.nojc$cl_type), rot_labels = c(right = -45))

mother_table <- table(dat.jc.nojc$mother, dat.jc.nojc$cl_type)
colnames(mother_table) <- c('????', 'no-????')
mother_table <- mother_table[1:10,]
mosaicplot(mother_table, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

################################################################################################################

# do RF with 5-fold CV

head(dat.jc.nojc)
table(dat.jc.nojc$ebh_lbh, dat.jc.nojc$cl_type)

nfolds = 5

set.seed(4)
folds <- sample(rep(1:nfolds, length.out = nrow(dat.jc.nojc)), size = nrow(dat.jc.nojc), replace = F)

CV.rf <- lapply(1:nfolds, function(x){ 
  
  # split in train/test
  dat.train <- dat.jc.nojc[folds != x,]
  dat.test <- dat.jc.nojc[folds == x,]
  print(length(dat.test$cl_type))
  
  # do oversampling on train data
  dat.jc.train <- dat.train[dat.train$cl_type=='jc',]
  dat.nojc.train <- dat.train[dat.train$cl_type=='no_jc',]
  
  set.seed(5)
  dat.jc.oversamp <- dat.jc.train[sample(1:nrow(dat.jc.train),nrow(dat.nojc.train),replace = T),]
  
  dat.oversamp <- rbind(dat.jc.oversamp, dat.nojc.train)
  
  # train the  on selection of variables that contribute positively to prediction accuracy
  # hyperparameters: ntree=5000, mtry=6
  rf.model <- randomForest(cl_type ~ mother+main_sub+CP+ebh_lbh+genre+s_p_order+cl_len+txt_type+Ques,ntree=5000,mtry=6,data = dat.oversamp)
  
  ########################################
  
  # make predictions
  preds <- predict(rf.model,  dat.test, type="prob")
  # cut-off point is 0.05, if prob > 0.05, prediction is "jc", else "no-jc"
  preds.resp <- ifelse(preds[,1] > 0.05, "jc", "no_jc")
  
  #print(preds)
  print(table(preds.resp, dat.test$cl_type))
  result_tab <- table(preds.resp, dat.test$cl_type)
  
  precision <- result_tab[1,1]/sum(result_tab[1,1:2])
  print(paste("precision", precision))
  recall <- result_tab[1,1]/sum(result_tab[1:2,1])
  print(paste("recall", recall))
  f_score <- 2 * precision * recall /(precision + recall)
  print(paste("f", f_score))
  return(list(preds.resp, true.vals = dat.test$cl_type, importance(rf.model), preds[,2], rf.model, dat.oversamp, precision=precision, recall=recall, f_score=f_score))
  
})

########################################################################

# calculate ROC 

tot.acc <- numeric(nfolds)

corr.jc <- numeric(nfolds)
tot.jc <- numeric(nfolds)

corr.nojc <- numeric(nfolds)
tot.nojc <- numeric(nfolds)

rocs <- list()

for (i in 1:nfolds) {
  rf.tab <- table(CV.rf[[i]][[2]], CV.rf[[i]][[1]])
  tot.acc[i] <- sum(diag(rf.tab))/sum(rf.tab)
  corr.jc[i] <- rf.tab[1,1]
  tot.jc[i] <- sum(rf.tab[1,])
  corr.nojc[i] <- rf.tab[2,2]
  tot.nojc[i] <- sum(rf.tab[2,])
  print(rf.tab)
  print(rf.tab[1,])
  print(sum(rf.tab[1,]))

  true.v <- CV.rf[[i]][[2]]
  true.v.num <- ifelse(true.v == 'jc', 1,0)
  pred.v <- CV.rf[[i]][[1]]
  pred.v.num <- ifelse(pred.v == 'jc', 1,0)  
  
  rocs[[i]] <- roc(true.v.num, CV.rf[[i]][[4]])
}
rocs

###############################################################

# make ROC plot

plot(rocs[[1]], lty=3,add=FALSE,asp=NA,main='')
for (i in 2:nfolds) {
  lines(rocs[[i]], lty=3)
}

###############################################################

# precision

all_precisions <- numeric(5)

for (i in 1:nfolds) {
  print(CV.rf[[i]]["precision"][[1]])
  all_precisions[i] <- CV.rf[[i]]["precision"][[1]]
}

average_precision <- mean(all_precisions)
average_precision

################################################################

# recall

all_recalls <- numeric(5)

for (i in 1:nfolds) {
  print(CV.rf[[i]]["recall"])
  all_recalls[i] <- CV.rf[[i]]["recall"][[1]]
}

average_recall <- mean(all_recalls)
average_recall

################################################################

# F-score

for (i in 1:nfolds) {
  print(CV.rf[[i]]["f_score"])
}

################################################################

# make plot of average variable importance with confidence interval

all_imports <- list()

for (i in 1:nfolds) {
  import <- as.data.frame(CV.rf[[i]][[3]])
  sort_import <- import[ order(row.names(import)), ]
  print(sort_import)
  all_imports[[i]] <- sort_import
  
}

all_imports_df <- as.data.frame(do.call(cbind, all_imports))
row.names(all_imports_df) <- row.names(import)[order(row.names(import))]

means <- apply(all_imports_df, 1, mean)
sds <- apply(all_imports_df, 1, sd)
mins <- means - 1.96*(sds/sqrt(5))
maxs <- means + 1.96*(sds/sqrt(5)) 

means.sds <- cbind(means, sds, mins, maxs)
means.sds <- as.data.frame(means.sds)
means.sds$feature <- row.names(means.sds)

means.sds <- means.sds[order(-means.sds$means),]

means.sds$feature <- factor(means.sds$feature, levels = means.sds$feature)
p <- ggplot(means.sds, aes(x = feature, y = means, ymin = mins, ymax = maxs)) +
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1,vjust=-0.1)) +
  ggtitle("")
p

####################################################################

# make plot with accuracy for both classes

mean.acc <- mean(tot.acc)
sd.acc <- sd(tot.acc)
min.acc <- mean.acc - 1.96*(sd.acc/sqrt(nfolds))
max.acc <- mean.acc + 1.96*(sd.acc/sqrt(nfolds))
tot.data <- c('All data',mean.acc,sd.acc,min.acc, max.acc)

frac.corr.nojc <- corr.nojc/tot.nojc
frac.corr.jc <- corr.jc/tot.jc


mean.nojc <- mean(frac.corr.nojc)
sd.nojc <- sd(frac.corr.nojc)
min.nojc <- mean.nojc - 1.96*(sd.nojc/sqrt(nfolds))
max.nojc <- mean.nojc + 1.96*(sd.nojc/sqrt(nfolds))
tot.nojc <- c('no_jc',mean.nojc,sd.nojc,min.nojc, max.nojc)

mean.jc <- mean(frac.corr.jc)
sd.jc <- sd(frac.corr.jc)
min.jc <- mean.jc - 1.96*(sd.jc/sqrt(nfolds))
max.jc <- mean.jc + 1.96*(sd.jc/sqrt(nfolds))
tot.jc <- c('jc',mean.jc,sd.jc,min.jc, max.jc)

overall <- rbind(tot.nojc,tot.jc)
overall <- data.frame(overall)
colnames(overall) <- c('data','means','sds','mins','maxs')

overall$means <- as.numeric(as.character(overall$means))
overall$sds <- as.numeric(as.character(overall$sds))
overall$mins <- as.numeric(as.character(overall$mins))
overall$maxs <- as.numeric(as.character(overall$maxs))
overall

axis_labels <- c("????", "no-????")

p <- ggplot(overall, aes(x = data, y = means, ymin = mins, ymax = maxs)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_x_discrete(breaks=c("jc", "no_jc"),
                  labels=axis_labels) +
  labs(x = "Class", y = "Accuracy") +
  ggtitle("")

p
