library(tidyverse)
library(reshape2)

setwd("C:/Users/geitb/Documents/SynVar/VerbalValence")

# needed for function adapt_genre()
source("adapt_genre.R")

# first import data about all cases of NTN and FJM
all_ntn_fjm <- read.csv("fjm_ntn_qn_mainsub.csv")

# adapt genre for specific verses
all_ntn_fjm <- adapt_genre(all_ntn_fjm)

dim(all_ntn_fjm)
table(all_ntn_fjm$verb)
head(all_ntn_fjm)

# select Hebrew cases
all_ntn_fjm <- all_ntn_fjm[all_ntn_fjm$language == "Hebrew",]
head(all_ntn_fjm)

table(all_ntn_fjm$verb, all_ntn_fjm$book)

all_ntn <- all_ntn_fjm[all_ntn_fjm$verb == "NTN",]
dim(all_ntn)

#########################################################################

# First make mosaic plots for the verb FJM
# for the main variables of the SynVar project

all_fjm <- all_ntn_fjm[all_ntn_fjm$verb == "FJM",]

head(all_fjm)
dim(all_fjm)

# import double object data of FJM
do_fjm <- read.csv("C:/Users/geitb/Documents/SynVar/VerbalValence/dat_fjm_cleaned.csv")
do_fjm <- adapt_genre(do_fjm)
dim(do_fjm)
head(do_fjm)

#select cases with L-object or two direct objects
l_do_fjm <- do_fjm[do_fjm$obj_valence %in% c(1100, 2000),]
l_do_fjm <- droplevels(l_do_fjm)

# process discourse FJM
disc_table_do_fjm <- table(l_do_fjm$discourse)

# get all cases
disc_table_all_fjm <- table(all_fjm$txt_type)
df_all_fjm <- as.data.frame(disc_table_all_fjm)

# get cases of double object (do)
df_do_fjm <- as.data.frame(disc_table_do_fjm)
all_data_fjm <- data.frame(df_all_fjm, df_do_fjm)
all_data_fjm <- all_data_fjm[,c(1,2,4)]

colnames(all_data_fjm) <- c("txt_type", "all", "double")
row.names(all_data_fjm) <- all_data_fjm[,1]
all_data_fjm[,1] <- NULL
all_data_fjm$no_double <- all_data_fjm$all - all_data_fjm$double
all_data_fjm$all <- NULL

colnames(all_data_fjm) <- c('Double object constructions', 'Other constructions')

mosaicplot(all_data_fjm, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 1)

##########################################################################

# process genre FJM
disc_table_do_fjm <- table(l_do_fjm$genre)
disc_table_all_fjm <- table(all_fjm$genre)
df_all_fjm <- as.data.frame(disc_table_all_fjm)

df_do_fjm <- as.data.frame(disc_table_do_fjm)
all_data_fjm <- data.frame(df_all_fjm, df_do_fjm)
all_data_fjm <- all_data_fjm[,c(1,2,4)]

colnames(all_data_fjm) <- c("genre", "all", "double")
row.names(all_data_fjm) <- all_data_fjm[,1]
all_data_fjm[,1] <- NULL
all_data_fjm$no_double <- all_data_fjm$all - all_data_fjm$double
all_data_fjm$all <- NULL

colnames(all_data_fjm) <- c('Double object constructions', 'Other constructions')

mosaicplot(all_data_fjm, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 1)

##########################################################################

# main and subordinate clauses FJM
ms_table_do_fjm <- table(l_do_fjm$main_sub)
ms_table_all_fjm <- table(all_fjm$main_sub)
df_ms_all_fjm <- as.data.frame(ms_table_all_fjm)

df_ms_do_fjm <- as.data.frame(ms_table_do_fjm)
df_ms_do_fjm$Var1 <- as.character(df_ms_do_fjm$Var1)

do_subarg <- c("SubArg", 0)
df_ms_do_fjm <- rbind(df_ms_do_fjm, do_subarg)
df_ms_do_fjm$Var1 <- c("Main", "SubAdv", "SubMod", "SubArg")

all_ms_data_fjm <- merge(df_ms_all_fjm, df_ms_do_fjm, by = "Var1")

all_ms_data_fjm$Freq.y <- as.numeric(all_ms_data_fjm$Freq.y)
all_ms_data_fjm$Freq.x <- all_ms_data_fjm$Freq.x - all_ms_data_fjm$Freq.y

colnames(all_ms_data_fjm) <- c("Var1", "Other constructions", "Double object constructions")
rownames(all_ms_data_fjm) <- all_ms_data_fjm$Var1
all_ms_data_fjm$Var1 <- NULL

all_ms_data_fjm <- all_ms_data_fjm[,c(2,1)]

mosaicplot(all_ms_data_fjm, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

##################################################################################################

# make mosaic plots for main variables of SynVar project for NTN

all_ntn <- all_ntn_fjm[all_ntn_fjm[,1] == "NTN",]
head(all_ntn)
dim(all_ntn)

# import double object data of NTN
do_ntn <- read.csv("C:/Users/geitb/Documents/SynVar/VerbalValence/dat_ntn_cleaned.csv")
do_ntn <- adapt_genre(do_ntn)
dim(do_ntn)
head(do_ntn)

#select cases with L-object or two direct objects
l_do_ntn <- do_ntn[do_ntn$obj_valence %in% c(1100, 2000),]
l_do_ntn <- droplevels(l_do_ntn)

# discourse NTN
disc_table_do_ntn <- table(l_do_ntn$txt_type)
disc_table_all_ntn <- table(all_ntn$txt_type)
df_all_ntn <- as.data.frame(disc_table_all_ntn)

df_do_ntn <- as.data.frame(disc_table_do_ntn)
all_data_ntn <- merge(df_do_ntn, df_all_ntn, by = "Var1", all=TRUE)
all_data_ntn[4,2] <- 0
all_data_ntn

colnames(all_data_ntn) <- c("txt_type", "double", "all")

row.names(all_data_ntn) <- all_data_ntn[,1]
all_data_ntn[,1] <- NULL
all_data_ntn$no_double <- all_data_ntn$all - all_data_ntn$double
all_data_ntn$all <- NULL
all_data_ntn <- all_data_ntn[c(4,1,2,3),]

colnames(all_data_ntn) <- c('Double object constructions', 'Other constructions')
all_data_ntn

all_data_ntn
mosaicplot(all_data_ntn, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 1)

##########################################################################

# genre NTN
disc_table_do_ntn <- table(l_do_ntn$genre)
disc_table_do_ntn
disc_table_all_ntn <- table(all_ntn$genre)
disc_table_all_ntn
df_all_ntn <- as.data.frame(disc_table_all_ntn)

df_do_ntn <- as.data.frame(disc_table_do_ntn)
all_data_ntn <- data.frame(df_all_ntn, df_do_ntn)
all_data_ntn <- all_data_ntn[,c(1,2,4)]

colnames(all_data_ntn) <- c("genre", "all", "double")
row.names(all_data_ntn) <- all_data_ntn[,1]
all_data_ntn[,1] <- NULL
all_data_ntn$no_double <- all_data_ntn$all - all_data_ntn$double
all_data_ntn$all <- NULL

colnames(all_data_ntn) <- c('Double object constructions', 'Other constructions')

# Mosaic plot
all_data_ntn
mosaicplot(all_data_ntn, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 1)

##########################################################################

# main and subordinate clauses NTN
head(l_do_ntn)
ms_table_do_ntn <- table(l_do_ntn$main_sub)

ms_table_all_ntn <- table(all_ntn$main_sub)
df_ms_all_ntn <- as.data.frame(ms_table_all_ntn)

df_ms_do_ntn <- as.data.frame(ms_table_do_ntn)
df_ms_do_ntn$Var1 <- as.character(df_ms_do_ntn$Var1)

do_subarg <- c("SubArg", 0)
df_ms_do_ntn <- rbind(df_ms_do_ntn, do_subarg)
df_ms_do_ntn$Var1 <- c("Main", "SubAdv", "SubMod", "SubArg")

all_ms_data_ntn <- merge(df_ms_all_ntn, df_ms_do_ntn, by = "Var1")

all_ms_data_ntn$Freq.y <- as.numeric(all_ms_data_ntn$Freq.y)
all_ms_data_ntn$other <- all_ms_data_ntn$Freq.x - all_ms_data_ntn$Freq.y
all_ms_data_ntn$Freq.x <- NULL
colnames(all_ms_data_ntn) <- c("Var1", "Double object constructions", "Other constructions")

rownames(all_ms_data_ntn) <- all_ms_data_ntn$Var1
all_ms_data_ntn$Var1 <- NULL

# Mosaic plot
mosaicplot(all_ms_data_ntn, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)


#############################################################################################

# make barplots of fraction of double object constructions for NTN for the MT books
ntn_counts <- as.data.frame(tapply(ntn_fjm$ntn_count, ntn_fjm$book2, sum))

ntn_counts$book <- rownames(ntn_counts)
rownames(ntn_counts) <- NULL
colnames(ntn_counts) <- c('sum', 'book')

do3 <- merge(do, ntn_counts, by = 'book', sort=FALSE)

do3$double_ntn <- do3$ntn_ld + do3$ntn_dd

do3$fraction_ntn <- do3$double_ntn / do3$sum
do3 <- do3[!is.nan(do3$fraction_ntn),]

lower_ntn <- NULL
upper_ntn <- NULL

for (row in 1:nrow(do3)) {
  binom <- binom.test(do3[row, "double_ntn"], do3[row, "sum"])
  lower_ntn[row] <- binom$conf.int[1]
  upper_ntn[row] <- binom$conf.int[2]
  
}

do3$lower <- lower_ntn
do3$upper <- upper_ntn

do3$book <- factor(do3$book, levels = do3$book)

# Fraction DO of NTN
p <- ggplot(do3, aes(x = book, y = fraction_ntn, ymin = lower, ymax = upper)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_y_continuous("Fraction of double object constructions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("")

p

###############################################################################################

# make barplot with fraction of double objects with FJM

fjm_counts <- as.data.frame(tapply(ntn_fjm$fjm_count, ntn_fjm$book2, sum))

fjm_counts$book <- rownames(fjm_counts)
rownames(fjm_counts) <- NULL
colnames(fjm_counts) <- c('sum', 'book')

do4 <- merge(do, fjm_counts, by = 'book', sort=FALSE)

do4$double_fjm <- do4$fjm_ld + do4$fjm_dd
do4$fraction_fjm <- do4$double_fjm / do4$sum
do4 <- do4[!is.nan(do4$fraction_fjm),]

lower_fjm <- NULL
upper_fjm <- NULL

for (row in 1:nrow(do4)) {
  binom <- binom.test(do4[row, "double_fjm"], do4[row, "sum"])
  lower_fjm[row] <- binom$conf.int[1]
  upper_fjm[row] <- binom$conf.int[2]
  
}

do4$lower <- lower_fjm
do4$upper <- upper_fjm

do4$book <- factor(do4$book, levels = do4$book)

# Fraction Double Objects FJM
p <- ggplot(do4, aes(x = book, y = fraction_fjm, ymin = lower, ymax = upper)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_y_continuous("Fraction of double object constructions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("")

p

##################################################################################

# make figure of double object FJM as fraction of total double object cases

do$fjm_vs_ntn <- (do$fjm_ld + do$fjm_dd) / (do$fjm_ld + do$fjm_dd + do$ntn_ld + do$ntn_dd)
do$double_fjm <- do$fjm_ld + do$fjm_dd
do$all_double <- do$fjm_ld + do$fjm_dd + do$ntn_ld + do$ntn_dd
do5 <- do[!is.nan(do$fjm_vs_ntn),]
do5

lower_fjm <- NULL
upper_fjm <- NULL

for (row in 1:nrow(do5)) {
  binom <- binom.test(do5[row, "double_fjm"], do5[row, "all_double"])
  lower_fjm[row] <- binom$conf.int[1]
  upper_fjm[row] <- binom$conf.int[2]
  
}

lower_fjm
upper_fjm
do5$lower <- lower_fjm
do5$upper <- upper_fjm
head(do5)

do5$book <- factor(do5$book, levels = do5$book)

# Double objects with FJM as fraction of FJM plus NTN 
p <- ggplot(do5, aes(x = book, y = fjm_vs_ntn, ymin = lower, ymax = upper)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_y_continuous("Fraction of double object construction with FJM") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("")

p

##################################################

# K-object visualization

setwd("C:/Users/geitb/Documents/SynVar/VerbalValence")

do_ntn <- read.csv("C:/Users/geitb/Documents/SynVar/VerbalValence/dath.ntn.csv")
do_fjm <- read.csv("C:/Users/geitb/Documents/SynVar/VerbalValence/dath.fjm.csv")

k_ntn <- do_ntn %>% filter(obj_valence == "1010")

count_k_ntn <- table(k_ntn$book)
count_k_ntn <- as.data.frame(count_k_ntn)

count_k_fjm <- table(k_fjm$book)
count_k_fjm <- as.data.frame(count_k_fjm)

k_fjm <- do_fjm %>% filter(obj_valence == "1010")
dim(k_fjm)

# K_objects

all_ntn_fjm <- read.csv("fjm_ntn_qn_mainsub.csv")
head(all_ntn_fjm)
unique(all_ntn_fjm$book)
books <- unique((all_ntn_fjm[1:2009,])$book)
books <- data.frame(books)

k_ntn_counts <- merge(books, count_k_ntn, by.x = "books", by.y = "Var1", all.x = TRUE)

merge(k_ntn_counts, count_k_fjm, by.x = "books", by.y = "Var1", all.x = TRUE)

ntn_counts <- left_join(books, count_k_ntn, by = c("books" = "Var1"))

ntn_fjm_counts <- left_join(ntn_counts, count_k_fjm, by = c("books" = "Var1"))
colnames(ntn_fjm_counts) <- c("book", "NTN", "FJM")

ntn_fjm_counts$NTN[is.na(ntn_fjm_counts$NTN)] <- 0
ntn_fjm_counts$FJM[is.na(ntn_fjm_counts$FJM)] <- 0

ntn_fjm_counts$book <- factor(ntn_fjm_counts$book, levels = ntn_fjm_counts$book)
ntn_fjm_counts$book <- as.character(ntn_fjm_counts$book)

Samuel <- c("Samuel", "0", "0")
Kings <- c("Kings", "5", "2")
Chronicles <- c("Chronicles", "4", "0")
ntn_fjm_counts <- rbind(ntn_fjm_counts, Samuel, Kings, Chronicles)
ntn_fjm_counts <- ntn_fjm_counts[!(ntn_fjm_counts$book %in% c("1_Samuel", "2_Samuel", "1_Kings", "2_Kings", "1_Chronicles", "2_Chronicles")),]

ntn_fjm_counts <- ntn_fjm_counts[c(1:7, 33, 34,8:32, 35),]
ntn_fjm_counts$book <- as.factor(ntn_fjm_counts$book)

ntn_fjm_counts$NTN <- as.numeric(as.character(ntn_fjm_counts$NTN))
ntn_fjm_counts$FJM <- as.numeric(as.character(ntn_fjm_counts$FJM))
ntn_fjm_counts$book <- factor(ntn_fjm_counts$book, levels = ntn_fjm_counts$book)

                                               
ntn_fjm_counts_melt <- melt(ntn_fjm_counts)

# NTN and FJM with K-object
ggplot(ntn_fjm_counts_melt, aes(fill=variable, y=value, x=book)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Count") +
  scale_fill_grey(name="Verb",
                  breaks=c("NTN", "FJM"),
                  labels=c("NTN", "FJM"))

# Mosaic plot of distribution in Pentateuch
fjm <- c(14, 2)
ntn <- c(7, 24)
pent <- rbind(fjm, ntn)
colnames(pent) <- c("J/E", "P")

rownames(pent) <- c( "FJM", "NTN" )
pent

mosaicplot(pent)
mosaicplot(pent, main = '', 
           cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

