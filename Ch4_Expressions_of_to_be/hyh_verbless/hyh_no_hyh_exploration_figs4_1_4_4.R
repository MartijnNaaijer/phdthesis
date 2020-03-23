# visualize hyh versus all other clauses (without hyh)
# used for figures 4.1-4.4 in the thesis

biblical <- read.csv("hyh_clauses_bib.csv", header = TRUE)
head(biblical)

xbib <- read.csv("hyh_clauses_xbib.csv", header = TRUE)
head(xbib)

# define subcorpora
xbib$subcorp <- ifelse(xbib$book2 %in% c('1QM', '1QH','1QS'), 'Qumran Hebrew', 'Epigraphic Hebrew')
xbib$subcorp[xbib$book2 %in% c('Pirqe', 'Shirata')] <- 'Rabbinic Hebrew'

biblical$subcorp <- "Biblical Hebrew"
###########################################################################

# merge data

all_hyh <- rbind(biblical, xbib)
head(all_hyh)
dim(all_hyh)

# change some genres (poetry within prose books, etc)
all_hyh$book.ch <- paste(all_hyh$book, all_hyh$chapter, sep = '_')

all_hyh$genre <- as.character(all_hyh$genre)

# import adapt_genre(), file can be found in folder "various"
source("adapt_genre.R")

# adapt genre for specific verses
all_hyh <- adapt_genre(all_hyh)

##############################################

# select only Hebrew data
head(all_hyh)
all_hyh <- all_hyh[all_hyh$language == "Hebrew",]
dim(all_hyh)

# make some mosiacplots to explore data

# language phase 4.1
phase <- table(all_hyh$ebh_lbh, all_hyh$cl_type)
phase
colnames(phase) <- c("HYH", "no HYH")
mosaicplot(phase, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# Main_sub 4.2
all_hyh.ms <- all_hyh[all_hyh$main_sub != 'Undc',]
all_hyh.ms <- droplevels(all_hyh.ms)
m.s <- table(all_hyh.ms$main_sub, all_hyh.ms$cl_type)
m.s
colnames(m.s) <- c("HYH", "no HYH")
mosaicplot(m.s, main = '', cex.axis = 1.2, color = c('#E6B0AA', '#5DADE2'), las = 2)


# discourse 4.3
discourse <- table(all_hyh$txt_type, all_hyh$cl_type)
colnames(discourse) <- c("HYH", "no HYH")
mosaicplot(discourse, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# genre 4.4
genre <- table(all_hyh$genre, all_hyh$cl_type)
colnames(genre) <- c("HYH", "no HYH")
mosaicplot(genre, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

