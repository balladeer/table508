#' Create a 508-compliant HTML table from tabular data
#'
#' \code{create_508_table} returns an xml2 Node representing a 508-compliant
#' version of the tabular data \code{.data}, formatted to qualify for
#' requirement H6 of the [HHS section-508 compliance checklist](https://www.hhs.gov/web/section-508/making-files-accessible/checklist/html/index.html).
#'
#' Note that this package (currently) supports only tables with exactly one set
#' of row name and one set of column names. Support for requirement H7, which
#' defines how to make tables with nested column and row headers compliant, is
#' not included.
#'
#' @param .data A data frame
#' @param table_id A string representing the \code{id} attribute of the
#'   resulting \code{<table>} node
#' @param top_left_cell A string indicating value of top-left cell, defaults to
#' the word `Row`
#' @param summary_attr A string used as the table's summary-attribute
#' @param caption A string used to create a \code{<caption>} tag
#' @return xml2 node containing \code{<table>}
#'
#' @examples
#' html_node <- table508::create_508_table(trees)
#' writeLines(paste(html_node))
#'
#' @export
create_508_table <- function(.data, table_id = "table",
                             top_left_cell="Row", summary_attr=NULL,
                             caption=NULL){

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

  # Add the top-left-cell (aka the column header for the row names)
  xml2::xml_add_child(header_row, "th", top_left_cell,
                      scope = "col")
  for (i in seq_along(colnames(.data))){
    xml2::xml_add_child(header_row, "th", colnames(.data)[i], scope = "col")
  }

  # Create each data row
  body <- xml2::xml_add_child(table, "tbody")

  for (i in seq_len(nrow(.data))){
    xml_row <- xml2::xml_add_child(body, "tr")

    # Add the <th> entry for this row
    xml2::xml_add_child(xml_row, "th", rownames(.data)[i], scope = "row")

    row <- .data[i, ]
    for (j in seq_len(ncol(row))) {
      cell_value <- as.character(row[j])
      xml2::xml_add_child(xml_row, "td", cell_value)
    }
  }

  # Return final XML node
  return(table)
}
