library(tidyverse)
library(factoextra)
library(cluster)

# for this analysis, four files are needed, there is a choice between phrase level and word level analysis
# jo_jo_ru_preds_phrase_level_Q.csv
# jo_jo_ru_preds_phrase_level_N.csv
# Validation_preds_phrase_level_Q.csv
# Validation_preds_phrase_level_N.csv

# OR

# jo_jo_ru_preds_word_level_Q.csv
# jo_jo_ru_preds_word_level_N.csv
# Validation_preds_word_level_Q.csv
# Validation_preds_word_level_N.csv

# There is also a file needed with clause counts, these are equal for both word and phrase levels
# clause_counts_phrase_level_Q.csv
# clause_counts_phrase_level_N.csv

setwd("C:/Users/geitb/Documents/SynVar/proefschrift_scripts_data/chapter6")

# import predictions for Jonah, Ruth, and Job
jo_jo_ru_q <- read.csv("jo_jo_ru_preds_word_level_Q.csv", header = TRUE)
head(jo_jo_ru_q)

jo_jo_ru_n <- read.csv("jo_jo_ru_preds_word_level_N.csv", header = TRUE)
head(jo_jo_ru_n)

# import predictions ebh and lbh books
valid_q <- read.csv("Validation_preds_word_level_Q.csv", header = TRUE)

valid_n <- read.csv("Validation_preds_word_level_N.csv", header = TRUE)
head(valid_n)

# Make histograms for Q clauses

par(mfrow = c(3,2))
hist(valid_q$Genesis, xlab = '', main = "Distribution of Q-clauses classified as LBH in Genesis")
hist(valid_q$Samuel, xlab = '', main = "Distribution of Q-clauses classified as LBH in Samuel")
hist(valid_q$Esther, xlab = '', main = "Distribution of Q-clauses classified as LBH in Esther")
hist(jo_jo_ru_q$Jonah, xlab = '', main = "Distribution of Q-clauses classified as LBH in Jonah")
hist(jo_jo_ru_q$Ruth, xlab = '', main = "Distribution of Q-clauses classified as LBH in Ruth")
hist(jo_jo_ru_q$Job, xlab = '', main = "Distribution of Q-clauses classified as LBH in Job")

par(mfrow = c(1,1))

# make histograms for N clauses

par(mfrow = c(3,2))
hist(valid_n$Genesis, xlab = '', main = "Distribution of N-clauses classified as LBH in Genesis")
hist(valid_n$Samuel, xlab = '', main = "Distribution of N-clauses classified as LBH in Samuel")
hist(valid_n$Esther, xlab = '', main = "Distribution of N-clauses classified as LBH in Esther")
hist(jo_jo_ru_n$Jonah, xlab = '', main = "Distribution of N-clauses classified as LBH in Jonah")
hist(jo_jo_ru_n$Ruth, xlab = '', main = "Distribution of N-clauses classified as LBH in Ruth")
hist(jo_jo_ru_n$Job, xlab = '', main = "Distribution of N-clauses classified as LBH in Job")

par(mfrow = c(1,1))

##########################################################################################################

# Cumulative means of Q

gen_q <- cumsum(valid_q$Genesis) / seq_along(valid_q$Genesis)
sam_q <- cumsum(valid_q$Samuel) / seq_along(valid_q$Samuel)
est_q <- cumsum(valid_q$Esther) / seq_along(valid_q$Esther)
jon_q <- cumsum(jo_jo_ru_q$Jonah) / seq_along(jo_jo_ru_q$Jonah)
rut_q <- cumsum(jo_jo_ru_q$Ruth) / seq_along(jo_jo_ru_q$Ruth)
job_q <- cumsum(jo_jo_ru_q$Job) / seq_along(jo_jo_ru_q$Job)

par(mfrow=c(3,2))
plot(gen_q, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 200 Predictions of Q-clauses in Genesis")
plot(sam_q, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 200 Predictions of Q-clauses in Samuel")
plot(est_q, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 200 Predictions of Q-clauses in Esther")
plot(jon_q, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 2800 Predictions of Q-clauses in Jonah")
plot(rut_q, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 2800 Predictions of Q-clauses in Ruth")
plot(job_q, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 2800 Predictions of Q-clauses in Job")
par(mfrow=c(1,1))

###########################################################################################################
# Cumulative means of N

gen_n <- cumsum(valid_n$Genesis) / seq_along(valid_n$Genesis)
sam_n <- cumsum(valid_n$Samuel) / seq_along(valid_n$Samuel)
est_n <- cumsum(valid_n$Esther) / seq_along(valid_n$Esther)
jon_n <- cumsum(jo_jo_ru_n$Jonah) / seq_along(jo_jo_ru_n$Jonah)
rut_n <- cumsum(jo_jo_ru_n$Ruth) / seq_along(jo_jo_ru_n$Ruth)
job_n <- cumsum(jo_jo_ru_n$Job) / seq_along(jo_jo_ru_n$Job)

par(mfrow=c(3,2))
plot(gen_n, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 200 Predictions of N-clauses in Genesis")
plot(sam_n, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 200 Predictions of N-clauses in Samuel")
plot(est_n, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 200 Predictions of N-clauses in Esther")
plot(jon_n, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 2800 Predictions of N-clauses in Jonah")
plot(rut_n, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 2800 Predictions of N-clauses in Ruth")
plot(job_n, type = "b", pch = 1, lty = 1,main = "Cumulative mean of 2800 Predictions of N-clauses in Job")
par(mfrow=c(1,1))

# import numbers of clauses for each book
counts_q <- read.csv("clause_counts_phrase_level_Q.csv", header = TRUE)
counts_q <- t(counts_q)
colnames(counts_q) <- "count"
book <- rownames(counts_q)

clause_counts_q <- as.data.frame(cbind(book, counts_q))
clause_counts_q$count <- as.numeric(as.character(clause_counts_q$count))


counts_n <- read.csv("clause_counts_phrase_level_N.csv", header = TRUE)
counts_n <- t(counts_n)
colnames(counts_n) <- "count"
book <- rownames(counts_n)
clause_counts_n <- as.data.frame(cbind(book, counts_n))
clause_counts_n$count <- as.numeric(as.character(clause_counts_n$count))

# calculate fraction of late predictions for each book for Q and N

ebh_lbh <- colnames(valid_n)

ebh_lbh_q_list <- list()

for (bo in ebh_lbh) {
  counts_q <- clause_counts_q[clause_counts_q$book == bo, "count"]
  
  ebh_lbh_q_list[[bo]] <- valid_q[, bo] / counts_q
  
}

ebh_lbh_q <- do.call("cbind", ebh_lbh_q_list)

ebh_lbh_q_st <- as.data.frame(apply(ebh_lbh_q, 2, mean))
colnames(ebh_lbh_q_st) <- c("Q")

ebh_lbh_n_list <- list()
for (bo in ebh_lbh) {
  counts_n <- clause_counts_n[clause_counts_n$book == bo, "count"]
  
  ebh_lbh_n_list[[bo]] <- valid_n[, bo] / counts_n
  
}

ebh_lbh_n <- do.call("cbind", ebh_lbh_n_list)
head(ebh_lbh_n)
ebh_lbh_n_st <- as.data.frame(apply(ebh_lbh_n, 2, mean))
colnames(ebh_lbh_n_st) <- c("N")
ebh_lbh_n_st

ebh_lbh_qn <- cbind(ebh_lbh_n_st, ebh_lbh_q_st)
ebh_lbh_qn
plot(ebh_lbh_qn$N , ebh_lbh_qn$Q)
text(ebh_lbh_qn$N , ebh_lbh_qn$Q, labels = rownames(ebh_lbh_qn), cex= 0.8)

################################################################################################

# process jo_jo_ru_n and jo_jo_ru_q
clause_counts_n <- clause_counts %>% filter (disc == "N")
clause_counts_n

pred_books <- c("Jonah", "Ruth", "Job")
jo_jo_ru_n_list <- list()
for (bo in pred_books) {
  print(bo)
  counts_n <- clause_counts_n[clause_counts_n$book == bo, "count"]
  
  jo_jo_ru_n_list[[bo]] <- jo_jo_ru_n[, bo] / counts_n
  
}

pred_books_n <- do.call("cbind", jo_jo_ru_n_list)
head(pred_books_n)
pred_books_n_st <- as.data.frame(apply(pred_books_n, 2, mean))
colnames(pred_books_n_st) <- c("N")
pred_books_n_st

############################

jo_jo_ru_q_list <- list()
for (bo in pred_books) {
  counts_q <- clause_counts_q[clause_counts_q$book == bo, "count"]
  
  jo_jo_ru_q_list[[bo]] <- jo_jo_ru_q[, bo] / counts_q
  
}

pred_books_q <- do.call("cbind", jo_jo_ru_q_list)
head(pred_books_q)
pred_books_q_st <- as.data.frame(apply(pred_books_q, 2, mean))
colnames(pred_books_q_st) <- c("Q")
pred_books_q_st

##########################

pred_books_qn <- cbind(pred_books_n_st, pred_books_q_st)
pred_books_qn

all_books_qn <- rbind(ebh_lbh_qn, pred_books_qn)
plot(all_books_qn$N, all_books_qn$Q, 
     ylab = 'Fraction of Q-clauses classified as LBH',
     xlab = 'Fraction of N-clauses classified as LBH',
     main = '')
text(all_books_qn$N , all_books_qn$Q, labels = rownames(all_books_qn), cex= 0.8)


###############################################################################


# K-means

fviz_nbclust(all_books_qn,kmeans, method='wss')

km.res <- kmeans(all_books_qn, 2, nstart = 25)

fviz_cluster(km.res, data = all_books_qn,
             palette = c("#2E9FDF", "#00AFBB"), #, "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
)

################################################################

# PAM clustering

fviz_nbclust(all_books_qn, pam, method = "silhouette")+
  theme_classic()

# optimal: 3 clusters
pam.res <- pam(all_books_qn, 2)

fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07", "#00AFBB", "#E7B800"), 
             #ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

# also nice: 2 clusters
pam.res <- pam(all_books_qn, 2)

fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), 
             ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

