clear all
version 19
capture log close
*set maxvar 32767, permanently
********************************************************************************
*Project Title:  Importing the Conflict Dataset								   *
*Description:    															   *
*Dataset: INSEC Nepal Maoist Conflict Dataset		  						   *
*Author: Ramesh Dulal 														   *
********************************************************************************


*==============================================================================*
*          		        DEFINE THE PATH HERE        		                   *
*------------------------------------------------------------------------------*

if "`c(username)'" == "rameshdulal" {
    global code "/Users/rameshdulal/Documents/Web Portfolio/Nepal-Civil-War-and-Int-Migration/source_code/STATA_Codes"
    global data    "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Raw_Data"
    global nlfs "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Raw_Data/NLFS 3/NLFS_dataset"
    global results "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Modified_Data"
}

*Please put the stata's username in place of yourname. To finde stata's username type
* display c(username)
*in stata. And chnage the directory for workdir, data, results
if "`c(username)'" == "yourname" {
    global code "" //Please include the path of the GitHub Repository in your local machine//
    global data    ""
    global nlfs     "" //Please include the file path for NLFS dataset in the Raw Data of the GitHub Repository in your local machine//
    global results ""
}
* ============================================================

* Run scripts in order
do "$code/01_Conflict_Data.do"
do "$code/02_Conflict_Intensity.do"
