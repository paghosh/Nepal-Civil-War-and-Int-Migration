This folder contains the stata and a R code which produces the final dataset for the analysis.<br>
Run the STATA code in the *[00_Master_File.do](/source_code/STATA_Codes/00_Master_File.do)* which will run all the other codes from the codes *[01_Conflict_Data.do](/source_code/STATA_Codes/01_Conflict_Data.do)* to *[04_Personal_NLFS.do](/source_code/STATA_Codes/04_Personal_NLFS.do)*. Remember to change the path.<br>
Then run the R code *[05_NLFS_Conflict_data.R](/source_code/STATA_Codes/05_NLFS_Conflict_data.R)* and *[06_Final_Data.do](/source_code/STATA_Codes/06_Final_Data.do)* and remember to change the path in the R file.
> The data related to conflict is imported from .xlsx file by the stata code file *00_Conflict_Data.do* and it produces the dataset *conflict_data.dta* in the following file path in Dropbox:<br>
**/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Modified_Data** 

<br>
The stata code file *[06_Final_Data.do](/source_code/STATA_Codes/06_Final_Data.do)* defines *international_absentee_only*, *international_migrant*,and *present_ind_migrant*.