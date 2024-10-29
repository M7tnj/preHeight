# preHeight
preHeight is an R package that predicts the height of people with desired age in desired country based on training a neural network model.

Package: preHeight
Title: Height Prediction
Description: preHeight is a prediction package based on neural networks which takes recorded data from 1985 to 2019 to train the model and output the value for desired year in form of plots.
Version: 1.0.0
Authors@R: person("Mostafa", "Tamiminezhad", email = "tamiminezhad.m@yahoo.com", role = c("aut", "cre", "cph"))
License: MIT + file LICENSE
Requires: 
Imports: 
    tidyverse,
    dplyr,
    ggplot2,
    ggrepel,
    keras3,
    stringr,
    tensorflow,
    viridis
Suggests:
  mockery,
    plotly,
    reticulate,
    testthat (>= 3.0.0)
Config/testthat/edition: 3
Depends: 
    R (>= 2.10)
LazyData: true
SystemRequirements: TensorFlow (https://www.tensorflow.org/)
