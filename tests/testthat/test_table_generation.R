context("HTML table generation")


test_that("default arguments produce sensible output", {
  # Get the first 3 rows of `trees` and produce an HTML table from it
  actual <- create_508_table(trees[1:3, ])

  expected <- xml2::read_xml('
  <table id="table">
    <thead>
      <tr>
        <th id="columnHead1">Girth</th>
        <th id="columnHead2">Height</th>
        <th id="columnHead3">Volume</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td headers="columnHead1">8.3</td>
        <td headers="columnHead2">70</td>
        <td headers="columnHead3">10.3</td>
      </tr>
      <tr>
        <td headers="columnHead1">8.6</td>
        <td headers="columnHead2">65</td>
        <td headers="columnHead3">10.3</td>
      </tr>
      <tr>
        <td headers="columnHead1">8.8</td>
        <td headers="columnHead2">63</td>
        <td headers="columnHead3">10.2</td>
      </tr>
    </tbody>
  </table>')
  expect_equal(paste(actual), paste(xml2::xml_find_first(expected, "//table")))
})

test_that("error raised when rownames_as_headers set without top-left cell", {
  expect_error(create_508_table(trees[1:3, ], rownames_as_headers = T),
               'rownames_as_headers without setting top_left_cell')
})

test_that("row names used as row headers when argument set", {
  # Get the first 3 rows of `trees` and produce an HTML table from it
  data <- trees[1:3, ]
  rownames(data) <- c("Tree 1", "Tree 2", "Tree 3")
  actual <- create_508_table(data, rownames_as_headers = T,
                             top_left_cell='Specimen')

  expected <- xml2::read_xml('
  <table id="table">
    <thead>
      <tr>
        <th id="columnHead0">Specimen</th>
        <th id="columnHead1">Girth</th>
        <th id="columnHead2">Height</th>
        <th id="columnHead3">Volume</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <th id="rowHead1">Tree 1</th>
        <td headers="columnHead1 rowHead1">8.3</td>
        <td headers="columnHead2 rowHead1">70</td>
        <td headers="columnHead3 rowHead1">10.3</td>
      </tr>
      <tr>
        <th id="rowHead2">Tree 2</th>
        <td headers="columnHead1 rowHead2">8.6</td>
        <td headers="columnHead2 rowHead2">65</td>
        <td headers="columnHead3 rowHead2">10.3</td>
      </tr>
      <tr>
        <th id="rowHead3">Tree 3</th>
        <td headers="columnHead1 rowHead3">8.8</td>
        <td headers="columnHead2 rowHead3">63</td>
        <td headers="columnHead3 rowHead3">10.2</td>
      </tr>
    </tbody>
  </table>')
  expect_equal(paste(actual), paste(xml2::xml_find_first(expected, "//table")))
})
