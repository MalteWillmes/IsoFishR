
![IsoFishR](https://github.com/MalteWillmes/IsoFishR/blob/master/www/logo.png)

Data reduction and analysis of 87Sr/86Sr isotope ratios obtained via laser-ablation mass spectrometry. 
The aim of this application is to provide a rapid, reliable, and reproducible data reduction and analysis tool, that can be applied across disciplines. 

Instructions:
- Download the app from github as a .zip file and extract to a convenient location
- Open the IsoFish.R file with R or R Studio
- Make sure you are in an empty R environment (no previous workspace or packages loaded as this can cause conflicts)
- Run the IsoFish.R file (in R studio: small green arrow on the right called "run app", make sure this is set to "run external")
![Rstudio](https://github.com/MalteWillmes/IsoFishR/blob/master/www/R_studio.jpg)

Projects:
- Projects allow the user to modify and store the import data settings and to change the default data reduction and data analysis parameters
- Each project lives in its own folder in the app directory and any data and plots will be saved within the project folder

Data Reduction
- Select runfile/s for data reduction and wait for the data processing to finish. Depending on the number of files this may take a while
- Use the sample selector to view individual run files and check for data quality 
- Export the data (either appending to existing data or overwriting all files in the project folder)

Data Analysis
- Upload already reduced data (a single .csv file). Depending on the number of samples in your file this may take a while.
- Use the sample selector to view individual profiles
- When done editing save the edits to each profile and finally export the analyzed data at the end of your session
- The analyzed data.csv file can be reuploaded to keep working on an already started project


Data Table
- A tabular view of the current data being analyzed
