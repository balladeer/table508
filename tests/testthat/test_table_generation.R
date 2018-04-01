context("HTML table generation")

stringify_expected <- function(expected_xml){
  return(paste(xml2::xml_find_first(expected_xml, "//table")))
}

test_that("default arguments produce sensible output", {
  # Get the first 3 rows of `trees` and produce an HTML table from it
  actual <- table508::create_508_table(trees[1:3, ])

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
