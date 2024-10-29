library(mockery)
library(dplyr)


describe("importer function", {
  it("should download and read the data correctly", {
    # Mocking the download.file function to avoid actual download
    mockery::stub(importer, "download.file", TRUE)
    
    # Mocking the read.csv function to return a sample data frame
    sample_data <- data.frame(Country = c("CountryA", "CountryB"),
                              Mean.height = c(150, 160),
                              Year = c(2000, 2005),
                              Sex = c("Girls", "Boys"),
                              Age.group = c(10, 15))
    
    mockery::stub(importer, "read.csv", sample_data)
    
    # Simulating user input
    mockery::stub(importer, "readline", "CountryA")
    
    result <- importer()
    
    expect_equal(nrow(result), 1)  # Check if the result has one row
    expect_equal(result$Country[1], "CountryA")  # Check if the country is correct
  })
  
})