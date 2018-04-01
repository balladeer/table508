#' Create a 508-compliant HTML table from tabular data
#'
#' \code{create_508_table} returns an HTML string representing a 508-compliant version of the tabular data \code{.data}.
#'
#' @examples
#' df <- tibble('location'=c('Denver', 'DC', 'Miami'), 'meal'= c('dinner', 'lunch', 'lunch'))
#' html_text <- create_508_table(df)
#'
#' @export
create_508_table <- function(.data, table_id='table'){
  # Create the root xml node
  r <- xml2::xml_new_root('root')

  # Create the table node
  table <- xml2::xml_add_child(r, 'table', id=table_id, class='regular', summary='Long-form summary')

  # Create the table header
  header <- xml2::xml_add_child(table, 'thead')
  header_row <- xml2::xml_add_child(header, 'tr')

  for (i in seq_along(colnames(.data))){
    element_id <- sprintf('columnHead%i', i)
    xml2::xml_add_child(header_row, 'th', colnames(.data)[i], id=element_id)
  }

  # Create each data row
  body <- xml2::xml_add_child(table, 'tbody')

  for (i in seq_len(nrow(.data))){
    xml_row <- xml2::xml_add_child(body, 'tr')

    row <- .data[i, ]
    for (j in seq_len(ncol(row))) {
      headers <- sprintf('columnHead%i', j)
      td <- xml2::xml_add_child(xml_row, 'td', as.character(row[j]), headers=headers)
    }
  }

  # Display resulting XML
  writeLines(paste(table))

}
