setGeneric("validate", function(.Object, ...) standardGeneric("validate"))

mismatches <- variable <- NULL

setMethod("validate", "data.table", function(.Object, bundle, ...){
  if ("TOTAL" %in% colnames(.Object)){
    .Object[, "TOTAL" := as.integer(.Object[["TOTAL"]]), with = TRUE]
  }
  DT <- data.table::melt.data.table(.Object, id.vars = "partition")
  setnames(DT, old="value", new="count")
  DT <- subset(DT, count > 0)
  DT[, "mismatches" := NA, with = TRUE]
  for (i in c(1:nrow(DT))){
    partitionToInspect <- as(bundle[[ DT[i, partition] ]], "plprPartition")
    read(
      partitionToInspect,
      highlight = list(yellow = c(as.character(DT[i, variable]))),
      meta = "text_name", cqp = TRUE
    )
    input <- readline(">> ")
    if (input == "exit") break
    if (input == ""){
      DT[i, mismatches] <- 0
    } else if(grepl("^[0-9]+$", input)){
      DT[i, mismatches] <- as.integer(input)  
    }
  }
})

