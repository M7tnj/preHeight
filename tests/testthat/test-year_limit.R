library(mockery)


mock_importer <- function() {
  data.frame(
    Year = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2018),
    Sex = c("Girls", "Boys", "Girls", "Boys", "Girls", "Boys", "Girls", "Boys"),
    Age.group = c(5, 6, 7, 8, 9, 10, 11, 12),
    Mean.height = c(120, 130, 140, 150, 160, 170, 180, 190)
  )
}

test_that("year_limit function issues a warning for empty input", {
  with_mock(
    importer = mock_importer,
    `base::readline` = mock("", "", "1985"),  # Simulate empty input for both years
    {
      expect_warning(year_limit(mock_importer()), "\tDue to empty entry all values will be included")
    }
  )
})