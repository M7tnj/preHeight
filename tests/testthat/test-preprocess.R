library(mockery)




mock_importer <- function() {
  data.frame(
    Year = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2018),
    Sex = c("Girls", "Boys", "Girls", "Boys", "Girls", "Boys", "Girls", "Boys"),
    Age.group = c(5, 6, 7, 8, 9, 10, 11, 12),
    Mean.height = c(120, 130, 140, 150, 160, 170, 180, 190)
  )
}



test_that("preprocess function returns filtered data", {
  with_mock(
    importer = mock_importer,
    {
      result <- preprocess()
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 8)  
    }
  )
})