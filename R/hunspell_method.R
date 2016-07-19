# library(data.table)
# cpos_start <- 0
# cpos_end <- 100000
# corpus_to_check <- "PLPRBT"
# 
# DT <- data.table(
#   word=as.utf8(cqi_cpos2str(paste(corpus_to_check, "word", sep="."), c(cpos_start:cpos_end))),
#   pos=as.utf8(cqi_cpos2str(paste(corpus_to_check, "pos", sep="."), c(cpos_start:cpos_end)))
# )
# DT[, check := hunspell_check(DT[["word"]], dict="de_DE_neu")]
# 
# DT[grep("^NE$", DT[["pos"]]), check := TRUE]
# DT[grep("\\$.*", DT[["pos"]]), check := TRUE]
# DT[grep("UNDEF", DT[["word"]]), check := TRUE]
# 
# chunk <- 1
# chunkValue <- DT[1, check]
# chunkVector <- 1
# value <- DT[1, check]
# for (i in c(1:nrow(DT))){
#   if (i == 1) next
#   if (DT[i, check] != DT[i - 1, check]) {
#     chunk <- chunk + 1
#     chunkValue <- c(chunkValue, DT[i, check])
#   }
#   chunkVector <- c(chunkVector, chunk)
# }
# 
# DT[, chunk := chunkVector]
# 
# 
# .foo <- function(.SD){
#   if (.SD[1, check] == TRUE){
#     return(list(length=as.integer(0), what=paste(.SD[["word"]], collapse=" ")))
#   } else {
#     return(list(length=nrow(.SD), what=paste(.SD[["word"]], collapse=" ")))
#   }
# }
# test <- DT[,.foo(.SD), by=.(chunk)]
# 
# 
# sorted <- split(foo, as.factor(x))
# 
# 
