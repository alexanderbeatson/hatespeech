library (tidyverse)
library (openxlsx)

#----------------------------------------------------------------------------------------
#CTHP cleansing and sampling
CTHP <- read.csv ("./CTHP.csv", stringsAsFactors = F)
CTHP <- CTHP %>% filter (Comments > 100) %>% mutate (prob = Comments/sum(Comments))
Today <- format(Sys.time(), "D%dM%m")
if (nrow(CTHP) > 100) {
  lexicon <- read.csv ("https://docs.google.com/spreadsheets/d/e/2PACX-1vTyhO1LfxmVwkA_6QJnGRlV0Y0269WMcOnJymkA6ropJZH5i29Ob56rNuAWYwv4aAakXjsiqOBET2Gn/pub?gid=2071343638&single=true&output=csv", stringsAsFactors = F)
  lexicon <- unlist(lexicon)
  hs_row <- NULL
  for (i in 1:nrow(CTHP)) {
    hs_row[i] <- any(str_detect(CTHP[i,"Description"],lexicon))
  }
  hs_row[is.na(hs_row)] <- F
  if (sum(hs_row) > 100) {
    CTHP <- CTHP[hs_row,]
    CTHP <- CTHP[sample(1:nrow(CTHP),100, prob = CTHP$prob),]
  } else {
    CTU <- CTHP[!hs_row,]
    CTHP <- rbind.data.frame(CTU[sample(1:nrow(CTU),(100-sum(hs_row)),prob = CTU$prob),],CTHP[hs_row,])
  }
}
system("mkdir sample")
write.csv(CTHP, "sample_CTHP.csv", row.names = F)
#-------------------------------------------------------------------------------------------
#comment sampling


item_list <- system("ls ./post/", intern = T)
TD_data <- data.table()
smp_func <- function (s = 5) {
  for (i in 1:length(item_list)) {
    assign(paste0("docs",i), read.xlsx(paste0(cdir,item_list[i]), sheet = 1, startRow = 6, colNames = T, detectDates = T))
    unirl <- read.xlsx(paste0(cdir,item_list[i]), sheet = 1, rows = 2, cols = 2, colNames = F)
    unirl <- unlist(unirl)
    temp_df <- eval(as.name(paste0("docs",i)))
    temp_df <- temp_df[sample(nrow(temp_df),s, prob = temp_df$prob),]
    temp_df$url <- unirl
    TD_data <- rbind.data.frame(TD_data,temp_df)
    rm(temp_df)
  }
  return(TD_data)
}
if (length(item_list)< 100) {
  s = ceiling(500/length(item_list))
  TD_data <- smp_func(s = s)
  TD_data <- TD_data [sample(nrow(TD_data), 500),]
} else {
  TD_data <- smp_func()
}

write.csv(TD_data, "comment_sampling.csv", row.names = F)