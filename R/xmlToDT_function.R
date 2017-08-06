#' Parse an XML/TEI file as data.table
#' 
#' @param filename the name of the file to read in
#' @param body an xpath expression defining the body of the xml document
#' @param meta a named character vector with xpath expressions
#' @export xml2dt
#' @importFrom xml2 read_xml xml_attrs xml_find_all xml_name xml_parents
xmlToDT <- function(filename, body = "//body", meta = NULL, mc = NULL){
  doc <- xml2::read_xml(filename)
  textnodes <- xml_find_all(doc, xpath = sprintf("%s//text()", body))
  evaluateTextnode <- function(textnode){
    meta <- lapply(
      xml2::xml_parents(textnode),
      function(ancestor){
        sattrs <- xml_attrs(ancestor)
        if (length(sattrs) > 0){
          names(sattrs) <- paste(xml_name(ancestor), names(sattrs), sep = "_")
          return( sattrs )
        } else {
          return( setNames(TRUE, xml_name(ancestor)) )
        }
        
      }
    )
    data <- as.list(unlist(meta))
    data[["text"]] <- xml_text(textnode)
    as.data.table(data)
  }
  if (!is.integer(mc)){
    dts <- lapply(textnodes, evaluateTextnode)
  } else {
    dts <- mclapply(textnodes, evaluateTextnode, mc.cores = mc)
  }
  dt <- rbindlist(dts, fill = TRUE)
  if (!is.null(meta)){
    for (x in names(meta)){
      dt[, eval(x) := xml_text(xml_find_first(doc, meta[x])), with = TRUE]
    }
  }
  dt
}
