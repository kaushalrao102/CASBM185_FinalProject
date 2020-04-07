# CASBM185_FinalProject
Final project for course CASB M185 (at UCLA) involving training random forest statistical models on simulated clinical data to ultimately predict mortality in patients. Following are brief descriptions of the files included in this repository:

1. Ocelot_CaSB185_Project.pdf: Final research report for the project, including an abstract, introduction, methods, results, discussion, figures, & references. 
2. Data-Generation_MIMIC.ipynb: Python & SQL script used for initially querying data from the public MIMIC-III database. Ultimately, we did not get the results we wanted and then decided to simulate clinical data instead for our purposes. 
3. CaSB185_data_simulation_var0.r: R script used for generating the simulated clinical data for patients. 
4. newRandomForest.ipynb: Python script used for further processing the simulated data, creating & training the random forest classification and regression models, and creating visualizations of our results. 
5. DataCreation.ipynb: Python script for cleaning and merging dataframes before switching over to simulated data.
