#' Create a 508-compliant HTML table from tabular data
#'
#' \code{create_508_table} returns an HTML string representing a 508-compliant
#' version of the tabular data \code{.data}.
#' @param .data A data frame
#' @param table_id A string representing the \code{id} attribute of the
#'   resulting \code{<table>} node
#' @param rownames_as_headers A boolean toggling whether to include row headers
#'   based on the \code{rownames(.data)}
#' @param top_left_cell A string indicating value of top-left cell; used when
#'   rownames_as_headers if TRUE
#' @param summary_attr A string used as the table's summary-attribute
#' @param caption A string used to create a \code{<caption>} tag
#' @return xml2 node containing \code{<table>}
#'
#' @export
create_508_table <- function(.data, table_id = "table",
                             rownames_as_headers=FALSE, top_left_cell=NULL,
                             summary_attr=NULL, caption=NULL){

  # Validate arguments
  if (rownames_as_headers && is.null(top_left_cell)){
    stop("Can't use rownames_as_headers without setting top_left_cell as well")
  }

  COL_ID_TEMPLATE <- "columnHead%i"
  ROW_ID_TEMPLATE <- "rowHead%i"

  # Create the root xml node
  r <- xml2::xml_new_root("root")

  # Create the table node
  table <- xml2::xml_add_child(r, "table", id = table_id)
  if (!is.null(summary_attr)){
    # Add summary attribute to table tag
    xml2::xml_set_attr(table, "summary", summary_attr)
  }

  # Add caption, if requested
  if (!is.null(caption)){
    xml2::xml_add_child(table, "caption", caption)
  }

  # Create the table header
  header <- xml2::xml_add_child(table, "thead")
  header_row <- xml2::xml_add_child(header, "tr")

  if (rownames_as_headers){
    xml2::xml_add_child(header_row, "th", top_left_cell,
                        id = sprintf(COL_ID_TEMPLATE, 0))
  }
  for (i in seq_along(colnames(.data))){
    element_id <- sprintf(COL_ID_TEMPLATE, i)
    xml2::xml_add_child(header_row, "th", colnames(.data)[i], id = element_id)
  }

  # Create each data row
  body <- xml2::xml_add_child(table, "tbody")

  current_row_header <- NULL
  for (i in seq_len(nrow(.data))){
    xml_row <- xml2::xml_add_child(body, "tr")

    if (rownames_as_headers){
      row_header_id <- sprintf(ROW_ID_TEMPLATE, i)
      xml2::xml_add_child(xml_row, "th", rownames(.data)[i], id = row_header_id)
      current_row_header <- row_header_id
    }

    row <- .data[i, ]
    for (j in seq_len(ncol(row))) {
      headers <- trimws(paste(sprintf(COL_ID_TEMPLATE, j), current_row_header))
      cell_value <- as.character(row[j])
      xml2::xml_add_child(xml_row, "td", cell_value, headers = headers)
    }
  }

  # Return final XML node
  return(table)
}
