library(mockery)


mock_importer <- function() {
  data.frame(
    Year = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2018),
    Sex = c("Girls", "Boys", "Girls", "Boys", "Girls", "Boys", "Girls", "Boys"),
    Age.group = c(5, 6, 7, 8, 9, 10, 11, 12),
    Mean.height = c(120, 130, 140, 150, 160, 170, 180, 190)
  )
}

test_that("Age_limit function returns filtered data for valid age", {
  with_mock(
    importer = mock_importer,
    `base::readline` = mock("10"), 
    {
      result <- Age_limit()
      expect_equal(nrow(result), 1)  
      expect_equal(result$Age.group, "10")  
    }
  )
})


test_that("Age_limit function throws error for invalid age", {
  with_mock(
    importer = mock_importer,
    `base::readline` = mock("25"), 
    {
      expect_error(Age_limit(), "Invalid age entered. Please enter an age between 5 and 19.")
    }
  )
})