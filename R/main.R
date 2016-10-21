
#' Find the candidates of bandwidths for locLinear2d
#'
#' @param DT A data.table containing list or vector in the cell.
#'   The cells in each row must have the same number of elements.
#' @param unnestCols The column names to unnest.
#' @return A unnested data.table.
#' @examples
#' require(data.table)
#' DT <- unnest(data.table(V1 = list(c(1,3,5), c(1,7)), V2 = list(c(2,5,3), c(4,6)), V3 = 1:2))
#' @importFrom data.table .SD
#' @export
unnest <- function(DT, unnestCols = NULL){
  # check the columns to unnest
  if (is.null(unnestCols)) {
    unnestCols <- names(DT)[sapply(DT, function(x) any(class(x) %in% "list"))]
    message("Automatically recognize the nested columns: ", paste0(unnestCols, collapse = ", "))
  }
  # check unnestCols is in the DT
  if (any(!unnestCols %in% names(DT)))
    stop(sprintf("The columns, %s, does not in the DT.",
                 paste0(unnestCols[!unnestCols %in% names(DT)], collapse = ", ")))
  # get the group by variable
  groupbyVar <- setdiff(names(DT), unnestCols)
  # generate the expression to remove group by variable
  chkExpr <- paste0(groupbyVar, "=NULL", collapse = ",") %>>% (paste0("`:=`(", ., ")"))
  # check the lengths of each cell in list-column are all the same
  chkLenAllEqual <- DT[ , lapply(.SD, function(x) sapply(x, length)), by = groupbyVar] %>>%
    `[`(j = eval(parse(text = chkExpr))) %>>% as.matrix %>>% apply(1, diff) %>>% `==`(0) %>>% all
  if (!chkLenAllEqual)
    stop("The length in each cell is not equal.")

  # generate unnest expression
  expr <- unnestCols %>>% (paste0(., "=unlist(",  ., ")")) %>>%
    paste0(collapse = ",") %>>% (paste0(".(", ., ")"))
  # return unnested data.table
  return(DT[ , eval(parse(text = expr)), by = groupbyVar])
}

#' convert nested json input to unnested format
#'
#' @param input Input from opencpu
#' @importFrom data.table data.table
#' @importFrom jsonlite fromJSON
#' @export
convertJSON <- function(input){
  return(unnest(data.table(fromJSON(input))))
}

