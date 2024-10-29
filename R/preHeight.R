


importer <- function(){
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
  # url to download
  url <- "https://www.ncdrisc.org/downloads/bmi_height/height/all_countries/NCD_RisC_Lancet_2020_height_child_adolescent_country.zip"
  # destination name
  destfile <- "global.zip"
  
  # download 
  download.file(url, destfile, method = "curl")
  message("Downloading Data")
  
  
  globalDf <- read.csv(unzip(destfile))
  
  
  # remove the zip file
  file.remove(destfile)

  # get country name as input
  region <- readline(prompt = "Please enter the name of desired country: ")
  
  globalDf$Country[1]
  
  # Check if the file can be read
  if (!(region %in% globalDf$Country)) {
    warning("\t\tData to that country is currntly unavailable")
  } else {
    message(paste("\t\tProgram in progress >>>>>>>>"))
    # reform the data based on the answer
    globalDf <- globalDf |>
      filter(Country == region)
    
    return(globalDf)
    
  }
  
}


preprocess <- function(){
  # reorder the data only for age, sex, year and height  
  file <- importer() |>
    select(-c(Mean.height.lower.95..uncertainty.interval, 
              Mean.height.upper.95..uncertainty.interval,
              Mean.height.standard.error)) |>
    mutate(Mean.height = round(Mean.height, 2))
  
  year_limit <- function(file){
    # prompt the user for the year interval wants to base the prediction on (1985 - 2019)
    startingY <- readline(prompt = "Enter the starting year you want to start analysing the heights for prediction from 1985 to 2018: ")
    endY <- readline(prompt = "Enter the edning year you want to start analysing the heights for prediction from 2005 to 2019: ")
    
    if (startingY == "" & endY == ""){
      warning("\tDue to empty entry all values will be included")
      startingY <- 1985 
      endY <- 2019
    }
    while(!((as.numeric(endY) - as.numeric(startingY)) > 20)){
      stop("\n\tPlease enter the desired years with at least 20 year interval for better prediction")
    }
    # reorder the data
    y_limit <- file |>
      filter(Year >= startingY & Year <= endY)
    return(y_limit)
  }   
  
  gender_limit <- function(){
    y_limit <- year_limit(file)
    # prompt the user for the sex of the study
    gender <- readline(prompt = "Do you want the prediction be performed on exclusive gender? (g = Girls | b = Boys): ")
    # reorder the data
    if (gender == "g"){
      g_limit <- y_limit |>
        filter(Sex == "Girls")
    }else if (gender == "b"){
      g_limit <- y_limit |>
        filter(Sex == "Boys")
    }else{
      stop("Please specify the gender either boys or girls")
    }
  }  
  filtered_gender <- gender_limit()
  
  
  # get age user want to get prediction for (5 to 19)
  age <- str_squish(readline(prompt = "Enter your desired age to focus(5 - 19): "))
  
  
  if (!age %in% as.character(5:19)) {
    stop("Invalid age entered. Please enter an age between 5 and 19.")
  }
  # reorder the data  
  train_data <- filtered_gender |>
    filter(Age.group == age)
  
  
  return(list(filtered_gender, train_data))
}

visualization <- function(){
  
  # use full data to graph
  
  value <- preprocess()
  dfg <- value[[1]] 
  df <- value[[2]]
  
  avg_height <- df |>
    group_by(Year, Age.group) |>
    summarise(avg_height = mean(Mean.height), .groups = 'drop')
  
  
  # Create an advanced heatmap
  heatmapDf <-  ggplot(avg_height, aes(x = Year, y = Age.group, fill = avg_height)) +
    geom_tile(color = "white", size = 0.5) +  # Add white borders to tiles
    scale_fill_viridis(option = "C", direction = -1, name = "Avg Height (cm)") +  # Use viridis color scale
    geom_text(aes(label = round(avg_height, 1)), color = "black", size = 1) +  # Add text annotations
    labs(title = paste0("Average Height by Age Group Over Years in ", df$Country[1]," " ,df$Sex[1]),
         x = "Year",
         y = "Age Group") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
      legend.position = "right"  # Position the legend on the right
    )
  
  ggsave(filename = paste0(getwd(), "/before_prediction/heatmapdf.png"),  heatmapDf, units = "px", create.dir = TRUE)
  
  
  
  df$Age.group <- as.factor(df$Age.group)
  
  
  # Enhanced ggplot
  facetedDf <- ggplot(df, aes(x = Year, y = Mean.height, color = Age.group)) +
    geom_line(size = 1.2) +  # Increased line size
    geom_point(size = 3, shape = 21, fill = "white") +  # Points with a white fill
    facet_wrap(~ Age.group) +
    labs(
      title = paste0("Height Over Age Group by Years in ", df$Country[1], " ", df$Sex[1]),
      subtitle = "Data visualized by Age Group",
      x = "Year",
      y = "Height (cm)"
    ) +
    scale_color_viridis_d(option = "D") +  # Use discrete color scale
    theme_minimal(base_size = 15) +  # Base font size
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 15, face = "italic"),
      legend.position = "bottom",  # Move legend to the bottom
      panel.grid.major = element_line(color = "grey80"),  # Customize major grid lines
      panel.grid.minor = element_blank()  # Remove minor grid lines
    ) +
    geom_text(aes(label = Mean.height), vjust = -0.5, size = 3.5, show.legend = FALSE)
  ggsave(filename = paste0(getwd(), "/before_prediction/faceted.png"),facetedDf ,units = "px", create.dir = TRUE)# Add text labels to points
  
  
  
  
 
  
  scatterDf <- ggplot(df, aes(x = Year, y = Mean.height)) +
    geom_point(size = 3, color = "blue", alpha = 0.7) +  # Points with transparency
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1, color = "red") +  # Add linear regression line
    labs(
      title = "Scatter Plot of Height Over Years",
      subtitle = paste0("Showing the relationship between Year and Mean Height in ", df$Country[1], " ", df$Sex[1] ),
      x = "Year",
      y = "Height (cm)"
    ) +
    theme_minimal(base_size = 15) +  # Minimal theme with larger base font size
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 15, face = "italic"),
      panel.grid.major = element_line(color = "grey80"),  # Customize major grid lines
      panel.grid.minor = element_blank()  # Remove minor grid lines
    ) +
    geom_text_repel(aes(label = ifelse(Mean.height > 150, as.character(Mean.height), "")),  # Label only certain points
                    size = 3, show.legend = FALSE, nudge_y = 3)
  ggsave(filename = paste0(getwd(), "/before_prediction/scatter.png"),scatterDf ,units = "px", create.dir = TRUE)# Adjust label position
  
  return(df)
}

modelDf <- function(){

  
  train_df <- visualization()
  # Example data
  years <- train_df$Year  # Input (x)
  heights <- train_df$Mean.height  # Output (y)
  
  # Convert to matrix
  x <- matrix(years, ncol = 1)
  y <- as.matrix(heights)  # Ensure y is also a matrix
  
  # Initialize and compile the model
  model <- keras_model_sequential() %>%
    layer_dense(units = 10, activation = 'relu', input_shape = c(1)) %>%  # Define input shape explicitly
    layer_dense(units = 1)
  
  
  model |> compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam(),
    metrics = c('mean_absolute_error')
  )
  
  
  
  # Fit the model
  history <- model %>% fit(
    x,
    y,
    epochs = 100,
    batch_size = 2,
    validation_split = 0.2
  )
  
  # Evaluate the model
  score <- model %>% evaluate(x, y)
  print(score)
  
  return(list(model,train_df))
}

predictDf <- function(){
  value <- modelDf()
  model <- value[[1]]
  train_df <- value[[2]]
  
  # get user input what years want to get prediction for 
  yearsVec <- readline(prompt = "Write a vector of years you want to get predictions for: ")
  yearsVec <- as.numeric(unlist(strsplit(gsub(",", "", yearsVec), " ")))
  
  
  new_x <- matrix(yearsVec, ncol = 1)
  
  predicted_heights <- model %>% predict(new_x)
  
  predicted_heights_vector <- as.numeric(unlist(strsplit(gsub(",", "", predicted_heights), " ")))
  predictedY <- data.frame(
    Country = rep(train_df$Country[1], length(yearsVec)),  # Assuming same country
    Sex = rep(train_df$Sex[1], length(yearsVec)),      # Assuming same sex
    Year = yearsVec,
    Age.group = rep(train_df$Age.group[1], length(yearsVec)) , # Assuming same age group
    Mean.height = predicted_heights
  )
 
  
  return(predictedY)
}

predictVis <-  function(){
  
  final_df <- predictDf()
  scatterDf <- ggplot(final_df, aes(x = Year, y = Mean.height)) +
    geom_point(size = 3, color = "blue", alpha = 0.7) +  # Points with transparency
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1, color = "red") +  # Add linear regression line
    labs(
      title = "Scatter Plot of Height Over Years",
      subtitle = paste0("Showing the relationship between Year and Mean Height in ", final_df$Country[1], " ", final_df$Sex[1] ),
      x = "Year",
      y = "Height (cm)"
    ) +
    theme_minimal(base_size = 15) +  # Minimal theme with larger base font size
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 15, face = "italic"),
      panel.grid.major = element_line(color = "grey80"),  # Customize major grid lines
      panel.grid.minor = element_blank()  # Remove minor grid lines
    ) +
    geom_text_repel(aes(label = ifelse(Mean.height > 150, as.character(Mean.height), "")),  # Label only certain points
                    size = 3, show.legend = FALSE, nudge_y = 3)
  ggsave(filename = paste0(getwd(), "/prediction/scatter.png"),scatterDf ,units = "px", create.dir = TRUE)# Adjust label position
  
  final_df$Age.group <- as.factor(final_df$Age.group)
  
  
  # Enhanced ggplot
  facetedDf <- ggplot(final_df, aes(x = Year, y = Mean.height, color = Age.group)) +
    geom_line(size = 1.2) +  # Increased line size
    geom_point(size = 3, shape = 21, fill = "white") +  # Points with a white fill
    facet_wrap(~ Age.group) +
    labs(
      title = paste0("Height Over Age Group by Years in ", final_df$Country[1], " ", final_df$Sex[1]),
      subtitle = "Data visualized by Age Group",
      x = "Year",
      y = "Height (cm)"
    ) +
    scale_color_viridis_d(option = "D") +  # Use discrete color scale
    theme_minimal(base_size = 15) +  # Base font size
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 15, face = "italic"),
      legend.position = "bottom",  # Move legend to the bottom
      panel.grid.major = element_line(color = "grey80"),  # Customize major grid lines
      panel.grid.minor = element_blank()  # Remove minor grid lines
    ) +
    geom_text(aes(label = Mean.height), vjust = -0.5, size = 3.5, show.legend = FALSE)
  ggsave(filename = paste0(getwd(), "/prediction/faceted.png"),facetedDf ,units = "px", create.dir = TRUE)# Add text labels to points
  
  print("Sucessfully finished!")
}

