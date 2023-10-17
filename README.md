# STAT628_M2-BodyFat
STAT628 Module2 group project-- Group 13

Contributors：Shan Leng, Siyan Wang, Ruotong Zhang


## Data folder
The data folder includes the raw data(`BodyFat.csv`) and the cleaned data(`cleaned_data.csv`).The `Test.csv` and `Train.csv` are data splited from `cleaned_data.csv`, to be used for model training and tresting respectively.

## Code folder
The `data_cleaning.ipynb` is a jupyter notebook for data processing implementation.
The `python_model.ipynb` is a jupyter notebook contains the analysis of 4 different models:`Basic linear regression`, `Ridge regression`, `LassoCV regression` and `SVM regression`. We choose `LassoCV` as our final model; we do relevant residual analysis and outlier analysis as well.
The Code folder contains 3 scripts. The `app.r` is the shiny app R script, both `ui` and `server` are inside. To call the shiny app, the user just need to run it.

## Image folder
Image folder contains all the figures and tables produced in our analysis.



