\name{modelDf}
\alias{modelDf}
\title{Modeling process based on Dataframe}
\description{This function is designed to train a model based on the preprocessed data that will be passed fro the preprocessing part. The model will be designed with Neural Network algorithm using Tensorflow and Keras}
\usage{
"Install and Insert all the necessary libraries"

library(keras3)
library(tensorflow)
library(reticulate)

}
\value{
The grapgh of training process can be stored in your directory if needed
}
\examples{
modelDf(df)
}