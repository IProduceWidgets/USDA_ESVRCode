EVRI_Sorter_Read_Me.txt

The EVRI_Sorter.R script takes in any number of .csv files exported from the 
EVRI database and formats them in an easily accessible way for more detailed
literature searchers than are possible throught the website's standard user
interface.

There are two types of data in the EVRI metadata files which are of interest
for sorting over. 
The first are what EVRI call 'facets,' these are lists of
categorical variables. This script seperates them out into dummies so that they
can be included or dropped from the final dataset of interest easily.
The second are text fields. This includes author names, paper titles, as well as
long paragraph-form descriptions of parts of the paper including the asbtract 
and descriptions of the methods used. 

The easiest way to use this script is to collapse all the comment headers except
for:

#### HOW TO USE THIS SCRIPT: #### (line 130)

This section contains some flags which need to be set, such as file source
directory, and export file names. 
It also contains a flag for running all the tables in the writup of the 2022 gap
anlysis of ecosystem services. You can set this to False to prevent it from
generating those files.

##### Your Query ##### (Line 458)
    
Here you can add the various boolean operators over facets and regex searches to
form your final data query.

There are a couple of useful examples listed in the code immediately prior to
this section. You could also look at the file Gap_Analysis_Queries.R which
contains many queries that were used in the 2022 gap anlysis project.

#### Summary and Table Outputs #### (Line 475)

Here you can specify different meta data you would like to keep as well as
different summary statistics you would like to form if the defaults are not to
your liking.

The exported .xlsx files will appear in the OutputDirectory you specified
earlier, and are exported as .xslx for ease of incorporation into the microsoft
office suite.