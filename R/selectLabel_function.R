#' Select Label
#' 
#' Select label from data.frame!
#' 
#' @param x Story to be told.
#' @param labelCol Column to use for labelling.
#' @export selectLabel
selectLabel <- function(x, labelCol = "label"){
  if("is_subcategory" %in% colnames(x)){
    categorySelected <- select.list(
      choices = x[x[["is_subcategory"]] == FALSE, labelCol]
      )
    if (categorySelected == ""){
      return(NULL)
    } else {
      categorySelectedNo <- x[which(x[[labelCol]] == categorySelected), "category_no"]
      subcategoryOptions <- x[x[["category_no"]] == categorySelectedNo & x[["is_subcategory"]] == TRUE, labelCol]
      if (length(subcategoryOptions) > 0){
        label <- select.list(choices = subcategoryOptions)
        # subcategoryRowNo <- which(x[[labelCol]] == subcategorySelected)
        # subcategorySelectedNo <- x[subcategoryRowNo, "subcategory_no"]
      } else {
        message("no subcategories available, will return category label")
        label <- categorySelected
        # subcategorySelectedNo <- NA
      }
      return("")
    }
  } else {
    stop("procedure only implemented for subcategory choice")
  }
}
