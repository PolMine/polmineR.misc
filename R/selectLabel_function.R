#' Select Label
#' 
#' Select label from data.frame!
#' 
#' @param categories a \code{data.frame} with cols category_no, and, optionally, subcategory_logical, and subcategory_no
#' @param descriptionCol character vector
#' @export selectLabel
selectLabel <- function(x, labelCol = "label"){
  if("is_subcategory" %in% colnames(x)){
    categorySelected <- select.list(
      choices = subset(x, is_subcategory == FALSE)[,labelCol]
      )
    if (categorySelected == ""){
      return(NULL)
    } else {
      categorySelectedNo <- x[which(x[[labelCol]] == categorySelected), "category_no"]
      subcategoryOptions <- subset(
        x,
        category_no == categorySelectedNo & is_subcategory == TRUE
      )[,labelCol]
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
