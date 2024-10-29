\name{preprocess}
\alias{preprocess}
\title{preprocessing step for dataframe}
\description{The data that got downloaded is messy and needs to by tidyed in order to be used for modeling(aka training) process. The preprocess function will make the changes as you desire}
\usage{
"set your working directory"
setwd()

"Install and Insert all the necessary libraries"
library(tidyverse)
library(stringr)
library(dplyr)

}
\value{
The dataframe should be in form of .csv file. 
}
\examples{
preprocess("example.csv")
}