# Stats205 Project

This repo contains the code, data and output for the class project in Stats 205. The goal is to predict the number of bike users per day. 

## Folder Code: 

Contains code to reproduce outputs
  - load_clean: R code to load, clean and pre-process the data (is sourced in methods.R)
  - descriptives: R code to produce the descriptives (is sourced in methods.R)
  - methods.R: R code to create forecasts based on all methods except LSTM 
  - lstm_forecasting.ipynb: Python code for forecasting using an LSTM NN

## Folder Output:
  - subfolder plots: Contains all plots created. This includes descriptives as well as plots of the fitted and forecasted values for each method. 
  - subfolder tables: Contains summary statistics as well as a table comparing the RMSE for each method. 
  
## Folder Lib: 
Contains extra packages.

## Folder Literature:
Contains some papers and books related to this project.
