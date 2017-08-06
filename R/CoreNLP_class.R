#' @examples 
#' \dontrun{
#' library(data.table)
#' library(magrittr)
#' library(xml2)
#' library(stringi)
#' library(rJava)
#' library(pbapply)
#' library(parallel)
#' 
#' options(java.parameters = "-Xmx4g")
#' CNLP <- CoreNLP$new(method = "txt", filename = "/home/blaette/Lab/tmp/coreNLP.txt")
#' 
#' filenames <- Sys.glob(sprintf("%s/*.xml", "~/Lab/gitlab/plprbttxt_tei"))
#' filenames <- filenames[1:2]
#' 
#' metadata <- c(
#'   lp = "//legislativePeriod",
#'   session = "//titleStmt/sessionNo",
#'   date = "//publicationStmt/date"
#' )
#' dtList <- pblapply(filenames, function(x) xmlToDT(x, meta = metadata, mc = 1))
#' dt <- rbindlist(dtList, fill = TRUE)
#' rm(dtList)
#' dt2 <- dt[is.na(speaker)][, speaker := NULL] # remove text in speaker tag
#' dt2[, chunk := 1:nrow(dt2)]
#' for (x in c("div_what", "div_desc", "body", "TEI", "p")) dt2[[x]] <- NULL
#' dt2[["stage_type"]] <- ifelse(is.na(dt2[["stage_type"]]), "speech", "interjection")
#' rm(dt)
#' 
#' pbapply::pblapply(
#'   1:nrow(dt2),
#'   function(i) CNLP$annotate(dt2[["text"]][i]) # add chunks for matching with metadata table
#' )
#' tokenStreamDT <- rbindlist(annotationList)
#' tokenStreamDT[, cpos := 0:(nrow(tokenStreamDT) - 1)]
#' encode(tokenStreamDT[["token"]], corpus = "FOO", pAttribute = "word", encoding = "UTF-8")
#' encode(tokenStreamDT[["pos"]], corpus = "FOO", pAttribute = "pos")
#' 
#' cpos <- tokenStreamDT[,{list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))}, by = "chunk"]
#' setkeyv(cpos, cols = "chunk")
#' setkeyv(dt2, cols = "chunk")
#' dt3 <- dt2[cpos]
#' options("polmineR.cwb-regedit" = FALSE)
#' setnames(dt3, old = c("sp_party", "sp_name"), new = c("party", "name"))
#' for (col in c("party", "name", "lp", "session", "date")){
#'   dtEnc <- dt3[, c("cpos_left", "cpos_right", col), with = FALSE]
#'   encode(dtEnc, corpus = "FOO", sAttribute = col)
#' }
#' use()
#' @param filename if filename is not NULL (default), the object will be initialized with
#' a FileWriter, and new annotations will be appended
#' @param colsToKeep character vector with names of columens of the output data.table
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_match
CoreNLP <- setRefClass(
  
  "CoreNLP",
  
  fields = list(
    
    tagger = "jobjRef",
    xmlifier = "jobjRef",
    jsonifier = "jobjRef",
    writer = "jobjRef",
    append = "logical",
    method = "character",
    colsToKeep = "character"
    
  ),
  
  methods = list(
    
    initialize = function(
      stanfordDir = NULL, propertiesFile = NULL, 
      method = c("txt", "json", "xml"),
      colsToKeep = c("sentence", "id", "token", "pos", "ner"),
      filename = NULL
      ){

      .self$colsToKeep <- colsToKeep
      
      rJava::.jinit(force.init = TRUE) # does it harm when called again?

      # add stanford jars to classpath
      if (is.null(stanfordDir)){
        stanfordDir <- file.path(
          system.file("extdata", package = "coreNLP"),
          "stanford-corenlp-full-2015-12-09"
        )
      }
      stanfordPath <- Sys.glob(paste0(stanfordDir,"/*.jar"))
      rJava::.jaddClassPath(stanfordPath)

      .self$method <- method
      if (.self$method == "xml") .self$xmlifier <- rJava::.jnew("edu.stanford.nlp.pipeline.XMLOutputter")
      if (.self$method == "json") .self$jsonifier <- rJava::.jnew("edu.stanford.nlp.pipeline.JSONOutputter")

      if (is.null(propertiesFile)){
        propertiesFile <- file.path(
          system.file(package = "coreNLP", "extdata"),
          "StanfordCoreNLP-german.properties"
        )
      }
      rJava::.jaddClassPath(dirname(propertiesFile))

      .self$tagger <- rJava::.jnew(
        "edu.stanford.nlp.pipeline.StanfordCoreNLP",
        basename(propertiesFile)
      )
      
      if (is.null(filename)){
        .self$append <- FALSE
      } else {
        .self$append <- TRUE
        .self$writer <- new(
          J("java.io.PrintWriter"),
          .jnew("java.io.FileOutputStream",
                .jnew("java.io.File", filename),
                TRUE)
          )
      }

    },

    usingXML = function(anno){
      doc <- .jcall(.self$xmlifier, "Lnu/xom/Document;", "annotationToDoc", anno, .self$tagger)
      xml <- .jcall(doc, "Ljava/lang/String;", "toXML")
      df <- coreNLP::getToken(coreNLP:::parseAnnoXML(xml))
      colnames(df) <- tolower(colnames(df))
      as.data.table(df[, colsToKeep])
    },
    
    usingJSON = function(anno){
      j <- .jcall(.self$jsonifier, "Ljava/lang/String;", "print", anno)
      dat <- jsonlite::fromJSON(j)
      dt <- rbindlist(
        lapply(
          1:length(dat$sentences$tokens),
          function(i) as.data.table(dat$sentences$tokens[[i]])[, "sentence" := i]
        )
      )
      setnames(dt, old = c("index", "word"), new = c("id", "token"))
      dt[, .self$colsToKeep, with = FALSE]
    },
    
    usingTXT = function(anno){
      if (.self$append == FALSE){
        .self$writer <- .jnew("java.io.PrintWriter", filename <- tempfile())
      }
      .jcall(.self$tagger, "V", "prettyPrint", anno, .self$writer)
      # .jmethods(writer, "V", "close")
      if (.self$append == FALSE){
        return( .self$parsePrettyPrint(filename) )
      } else {
        return( NULL )
      }
    },
    
    parsePrettyPrint = function(x = NULL, filename = NULL, mc = 1){
      if (is.null(x)) x <- readLines(filename)
      chunks <- cut(
        1:length(x),
        c(grep("^Sentence\\s#\\d+", x), length(x)),
        include.lowest = TRUE, right = FALSE
      )
      dts <- lapply(
        split(x, f = chunks),
        function(chunk){
          txt <- chunk[grepl("^\\[.*\\]$", chunk)] # get lines with annotation
          regex <- "^.*?Text=(.*?)\\s.*\\sPartOfSpeech=(.*?)\\sNamedEntityTag=(.*?)\\]"
          df <- stringi::stri_match(txt, regex = regex)
          dt <- as.data.table(df)[,2:4, with = FALSE]
          colnames(dt) <- c("token", "pos", "ner")
          dt
        },
        cl = mc
      )
      # rbindlist(dts)
      dts
    },
    
    annotate = function(txt){
      anno <- rJava::.jcall(.self$tagger, "Ledu/stanford/nlp/pipeline/Annotation;", "process", txt)
      switch(.self$method,
             xml = .self$usingXML(anno),
             json = .self$usingJSON(anno),
             txt = .self$usingTXT(anno)
             )
    }
  )
)
