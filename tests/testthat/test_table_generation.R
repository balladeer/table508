context("HTML table generation")


test_that("default arguments produce sensible output", {
  # Get the first 3 rows of `trees` and produce an HTML table from it
  actual <- create_508_table(trees[1:3, ])

  expected <- xml2::read_xml('
  <table id="table">
    <thead>
      <tr>
        <th scope="col">Row</th>
        <th scope="col">Girth</th>
        <th scope="col">Height</th>
        <th scope="col">Volume</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <th scope="row">1</th>
        <td>8.3</td>
        <td>70</td>
        <td>10.3</td>
      </tr>
      <tr>
        <th scope="row">2</th>
        <td>8.6</td>
        <td>65</td>
        <td>10.3</td>
      </tr>
      <tr>
        <th scope="row">3</th>
        <td>8.8</td>
        <td>63</td>
        <td>10.2</td>
      </tr>
    </tbody>
  </table>')
  expect_equal(paste(actual), paste(xml2::xml_find_first(expected, "//table")))
})


test_that("row names used as row headers and top_left_cell arg respected", {
  # Get the first 3 rows of `trees` and produce an HTML table from it
  data <- trees[1:3, ]
  rownames(data) <- c("Tree 1", "Tree 2", "Tree 3")
  actual <- create_508_table(data, top_left_cell = "Specimen")

  expected <- xml2::read_xml('
  <table id="table">
    <thead>
      <tr>
        <th scope="col">Specimen</th>
        <th scope="col">Girth</th>
        <th scope="col">Height</th>
        <th scope="col">Volume</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <th scope="row">Tree 1</th>
        <td>8.3</td>
        <td>70</td>
        <td>10.3</td>
      </tr>
      <tr>
        <th scope="row">Tree 2</th>
        <td>8.6</td>
        <td>65</td>
        <td>10.3</td>
      </tr>
      <tr>
        <th scope="row">Tree 3</th>
        <td>8.8</td>
        <td>63</td>
        <td>10.2</td>
      </tr>
    </tbody>
  </table>')
  expect_equal(paste(actual), paste(xml2::xml_find_first(expected, "//table")))
})


test_that("summary keyword argument adds attribute to table tag", {
  data <- trees[1, ]
  actual <- create_508_table(data, summary = "Some summary text")

  expected <- xml2::read_xml('
  <table id="table" summary="Some summary text">
    <thead>
      <tr>
        <th scope="col">Row</th>
        <th scope="col">Girth</th>
        <th scope="col">Height</th>
        <th scope="col">Volume</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <th scope="row">1</th>
        <td>8.3</td>
        <td>70</td>
        <td>10.3</td>
      </tr>
    </tbody>
  </table>')
  expect_equal(paste(actual), paste(xml2::xml_find_first(expected, "//table")))
})

test_that("caption keyword argument adds <caption> tag when string supplied", {
  data <- trees[1, ]
  actual <- create_508_table(data, caption = "Caption text")

  expected <- xml2::read_xml('
  <table id="table">
    <caption>Caption text</caption>
    <thead>
      <tr>
        <th scope="col">Row</th>
        <th scope="col">Girth</th>
        <th scope="col">Height</th>
        <th scope="col">Volume</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <th scope="row">1</th>
        <td>8.3</td>
        <td>70</td>
        <td>10.3</td>
      </tr>
    </tbody>
  </table>')
  expect_equal(paste(actual), paste(xml2::xml_find_first(expected, "//table")))
})
