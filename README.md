
![IsoFishR](https://github.com/MalteWillmes/IsoFishR/blob/master/www/logo.png)

Data reduction and analysis of 87Sr/86Sr isotope ratios obtained via laser-ablation mass spectrometry. 
The aim of this application is to provide a rapid, reliable, and reproducible data reduction and analysis tool, that can be applied across disciplines.

Requirements
- R version 3.2 or higher. Download at https://www.r-project.org/
- Rstudio (optional but recommended). Download at https://www.rstudio.com/

Instructions
- Download the app from github as a .zip file and extract to a convenient location
- Open the IsoFish.R file with R or R Studio
- Make sure you are in an empty R environment (no previous workspace or packages loaded as this can cause conflicts)
- Run the IsoFish.R file (in R studio: small green arrow on the right called "run app", make sure this is set to "run external")
![Rstudio](https://github.com/MalteWillmes/IsoFishR/blob/master/www/R_studio.JPG)

Projects
- Projects allow the user to modify and store the ‘import data settings’ and to change the default ‘data reduction’ and ‘data analysis’ parameters
- Each project lives in its own folder in the app directory and all data and plots related to that project will be saved within the project folder
- Projects are used to group similar analyses together. Important settings to change project wide are:
	Analyses type: Spot or Line
-	Run speed: Used in the calculation from time to distance when using line transacts
-	Smoothing type (No smoothing, Moving average, Spline)
-	Integration: How many raw datapoints should be averaged before performing any further analyses
-	Min and Max 88Sr voltage: These are hard cutoffs to remove data that is either below the detection limit or above the range of the detectors. 
-	Runfile import setting: Match these to the standard output from the MC-ICP-MS used in the analyses. The default works well for NU instruments, though the order of the columns may be different
-	Default manual filter names: If you are planning on using the manual filter options later on you can define their default names here (e.g. Core, Natal, Adult region). These can still be changed individually for each sample
-	A lot of the other settings (Analysis direction, Laser energy, Fluency) do not influence any of the data reduction procedures but are stored as meta data and should thus be adjusted to represent the analyzed samples.
-	The project folder can be moved to different instances of IsoFishR which can be useful for sharing the data and working on the same dataset. Always move the whole project folder rather than individual subfolders

Data Reduction
-	Select runfile/s for data reduction and wait for the data processing to finish. You can select multiple files from within the same folder. Depending on the number of files this may take a while
-	Use the sample selector to view individual run files and check for data quality. Obvious signs of spurious data are for example spikes in the 88Sr voltage.
-	Export the data (either appending to existing data or overwriting all files in the project folder). Use appending when merging multiple datasets (from different folders) into a single project.

Data Analysis
-	Upload already reduced data (a single .csv file). Depending on the number of samples in your file this may take a while.
-	Use the sample selector to view individual profiles
-	Edit each profile
-	Trim can be used to remove data from the beginning or end of each profile. Save edits after entering a trim value will remove these data points permanently from the analyzed data file.
- Reverse profile
-	Manual filters: Clicking enable manual filters allows the selection of ranges along the sample profile
-	When done editing save the edits to each profile and finally export the analyzed data at the end of your session
-	The analyzed data.csv file can be reuploaded to keep working on an already started project
-	You nearly always want to use overwrite when exporting the Data Analysis file so that you do not create duplicate entries for each sample

Data Reporting
-	A tabular view of the current data being analyzed
