\name{preHeight}
\alias{preHeight}
\title{Height Predictor}
\description{A program based on neural network that can predict the height of people between 5 to 19 in given country in desired years based on recorded data between 1985 to 2019}
\usage{
"set your working directory"
setwd()

"Insert all the necessary libraries"
library(tidyverse)
library(stringr)
library(ggplot2)
library(dplyr)
library(viridis)
library(plotly)
library(ggrepel)
library(keras3)
library(tensorflow)
library(reticulate)

}
\value{
some graphs of age specific and non specific distribution of height in desired country before and past training and prediction will be in before_prediction and prediciton files in the directory.
}
\examples{
"Just run predictVis function :)"
}