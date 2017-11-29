
![IsoFishR](https://github.com/MalteWillmes/IsoFishR/blob/master/www/logo.png)

Data reduction and analysis of 87Sr/86Sr isotope ratios obtained via laser-ablation mass spectrometry. 
The aim of this application is to provide a rapid, reliable, and reproducible data reduction and analysis tool, that can be applied across disciplines. 

Instructions:
- Download the app from github and extract the zip file
- Run the IsoFish.R file (it should automatically install any required packages)

Projects:
- Projects allow the user to modify and store the import data settings and to change the default data reduction and data analysis parameters
- Each project lives in its own folder in the app directory and any data and plots will be saved within the project folder

Data Reduction
- Select runfile/s for data reduction and wait for the data processing to finish. 
- Use the sample selector to view individual run files and check for data quality 
- Export the data (either appending to exisiting data or overwriting all files in the project folder)

Data Analysis
- Select already reduced data (a single .csv file)
- Use the sample selector to view individual profiles
- When done editing save the edits to each profile and finally export the analyzed data at the end of your session
- The analyzed data.csv file can be reuploaded to keep working on an already started project


Data Table
- A tabular view of the current data being analyzed
