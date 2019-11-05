# CSO-Activation-Function
In this file, I have presented some developed functions. These functions help the user to extract, analyze and interpret the result from the Stormwater Management Model (SWMM) files, especially for the SWMM models which are huge and consist of thousands of lines of codes.
This code has some sections:

- The first section is to build the management scenarios which are consist of adding different type of Green Infrastructures (GIs) to the targeted subcatchments. 

- Then the functions are presented which are: 
    -- Cost Function: The cost of each management scenario
    -- Water Balance Function: The water balance of the whole watershed related to each management scenario
    -- Link Activation Function: To calculate the link activation events. The output of this functio consists of the activation event   time, volume, number of event, peak runoff volume, and so on.
    -- Node Activation Function: To calculate the node activation events like CSOs (Combined Sewer Overflow).
 
 - The last part is for interpreting the results which come from the above-mentioned functions. 
 
 When the SWMM input file (“.inp”) is run, the SWMM engine produces two outcome files; report file (“.rpt”) and output file (“.out”). The “.rpt” file is a plain text file that contains the contents of both “Status Report” and “Summary Results”. Whereas the “.out” file is in a binary format, which contains the time series datasets of any defined subcatchment, link or node.
 
 In this project, the .out dataset is used and the source of data. 

