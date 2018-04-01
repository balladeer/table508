context("HTML table generation")

stringify_expected <- function(expected_xml){
  writeLines(paste(xml2::xml_find_first(expected_xml, './/table')))
}

test_that("default arguments produce sensible output", {
  # Get the first 3 rows of `trees` and produce an HTML table from it
  actual <- table508::create_508_table(trees[1:3, ])

  expected = xml2::read_xml('
    <table id="table">
    	<thead>
    		<tr>
    			<th id="columnHead0">Girth</th>
    			<th id="columnHead1">Height</th>
    			<th id="columnHead2">Volume</th>
    		</tr>
    	</thead>
    	<tbody>
    		<tr>
    			<td headers="columnHead0">8.3</td>
    			<td headers="columnHead1">70</td>
    			<td headers="columnHead2">10.3</td>
    		</tr>
    		<tr>
    			<td headers="columnHead0">8.6</td>
    			<td headers="columnHead1">65</td>
    			<td headers="columnHead2">10.3</td>
    		</tr>
    		<tr>
    			<td headers="columnHead0">8.8</td>
    			<td headers="columnHead1">64</td>
    			<td headers="columnHead2">10.2</td>
    		</tr>
    	</tbody>
    </table>')
  expect_equal(actual, stringify_expected(expected))
})
