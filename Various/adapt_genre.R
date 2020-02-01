
adapt_genre <- function(dat) {
  # relabel genre of some verses
  
  dat$book.ch <- paste(dat$book, dat$chapter, sep = '_')
  
  dat$genre <- as.character(dat$genre)
  dat$genre[dat$book.ch == 'Genesis_49' & as.numeric(dat$verse) > 1 & as.numeric(dat$verse) < 28] <- 'poetry'
  dat$genre[dat$book.ch == 'Exodus_15' & as.numeric(dat$verse) > 1 & as.numeric(dat$verse) < 19] <- 'poetry'
  dat$genre[dat$book.ch == 'Numbers_6' & as.numeric(dat$verse) > 23 & as.numeric(dat$verse) < 27] <- 'poetry'
  dat$genre[dat$book.ch == 'Numbers_21' & as.numeric(dat$verse) > 27 & as.numeric(dat$verse) < 31] <- 'poetry'
  dat$genre[dat$book.ch == 'Numbers_23' & as.numeric(dat$verse) > 7 & as.numeric(dat$verse) < 11] <- 'poetry'
  dat$genre[dat$book.ch == 'Numbers_23' & as.numeric(dat$verse) > 18 & as.numeric(dat$verse) < 25] <- 'poetry'
  dat$genre[dat$book.ch == 'Numbers_24' & as.numeric(dat$verse) > 3 & as.numeric(dat$verse) < 10] <- 'poetry'
  dat$genre[dat$book.ch == 'Numbers_24' & as.numeric(dat$verse) > 15 & as.numeric(dat$verse) < 25] <- 'poetry'
  dat$genre[dat$book.ch == 'Deuteronomy_32' & as.numeric(dat$verse) < 44] <- 'poetry'
  dat$genre[dat$book.ch == 'Deuteronomy_33' & as.numeric(dat$verse) > 2] <- 'poetry'
  dat$genre[dat$book.ch == 'Judges_5' & as.numeric(dat$verse) > 1 & as.numeric(dat$verse) < 31] <- 'poetry'
  dat$genre[dat$book.ch == 'Judges_9' & as.numeric(dat$verse) > 7 & as.numeric(dat$verse) < 16] <- 'poetry'
  dat$genre[dat$book.ch == '1_Samuel_2' & as.numeric(dat$verse) < 11] <- 'poetry'
  dat$genre[dat$book.ch == '2_Samuel_22' & as.numeric(dat$verse) > 2] <- 'poetry'
  dat$genre[dat$book.ch == '2_Samuel_23' & as.numeric(dat$verse) > 2 & as.numeric(dat$verse) < 8] <- 'poetry'
  dat$genre[dat$book.ch == '2_Samuel_1' & as.numeric(dat$verse) > 18 & as.numeric(dat$verse) < 28] <- 'poetry'
  
  dat$genre[dat$book.ch == 'Jonah_2' & as.numeric(dat$verse) > 3 & as.numeric(dat$verse) < 11] <- 'poetry'
  
  dat$genre[dat$book.ch == 'Daniel_2' & as.numeric(dat$verse) > 19 & as.numeric(dat$verse) < 24] <- 'poetry'
  dat$genre[dat$book.ch == 'Daniel_8' & as.numeric(dat$verse) > 22 & as.numeric(dat$verse) < 27] <- 'poetry'
  dat$genre[dat$book.ch == 'Daniel_12' & as.numeric(dat$verse) < 4] <- 'poetry'
  dat$genre[dat$book.ch == 'Nehemiah_9' & as.numeric(dat$verse) > 5 & as.numeric(dat$verse) < 38] <- 'poetry'
  dat$genre[dat$book.ch == '1_Chronicles_16' & as.numeric(dat$verse) > 7 & as.numeric(dat$verse) < 37] <- 'poetry'
  dat$genre[dat$book.ch == 'Job_1' & as.numeric(dat$verse) < 21] <- 'prose'
  dat$genre[dat$book.ch == 'Job_2' & as.numeric(dat$verse) < 14] <- 'prose'
  dat$genre[dat$book.ch == 'Job_42' & as.numeric(dat$verse) > 6 & as.numeric(dat$verse) < 18] <- 'prose'
  dat$genre[dat$book.ch == 'Isaiah_36' & as.numeric(dat$verse) < 23] <- 'prose'
  dat$genre[dat$book.ch == 'Isaiah_37' & as.numeric(dat$verse) < 22] <- 'prose'
  dat$genre[dat$book.ch == 'Isaiah_38' & as.numeric(dat$verse) < 10] <- 'prose'
  dat$genre[dat$book.ch == 'Isaiah_38' & as.numeric(dat$verse) > 20] <- 'prose'
  dat$genre[dat$book.ch == 'Isaiah_39'] <- 'prose'
  dat$genre[dat$book.ch == 'Jeremiah_52'] <- 'prose'
  dat$genre[dat$book.ch == 'Jeremiah_39' | dat$book.ch == 'Jeremiah_40'] <- 'prose'
  
  dat$genre[dat$book == 'Daniel' &  as.numeric(dat$chapter) > 6  & as.numeric(dat$chapter) < 13 ] <- 'prophecy' 
  
  return(dat)
  
  }