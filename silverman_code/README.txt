







Multiple Systems Analysis for the Quantification of Modern Slavery:  Classical and Bayesian Approaches

Bernard W. Silverman
www.bernardsilverman.co.uk

SUPPLEMENTARY MATERIAL


The data and R functions in this material are also available as an R package modslavmse, available from github.   To install the package from an R session, use the command:

devtools::install_github("bernardsilverman/modslavmse")


The files in this material are as follows:

functions.R    The source of all the functions in the package, together with comments in Roxygen format.    The R command source("functions.R") will load all the functions into the current workspace.   The following scripts will produce all the figures and tables in the paper:

make_AIC_stepwise_table1()
make_AIC_stepwise_table2()
make_allmodels_plots_script()
make_LCMCR_table_script()       
make_madyor_table_script()      
make_MCMCeffects_table_script()
make_MCMCfit_tables_script() 

Note that make_madyor_table_script() produces the plots in Figures 5 and 6 as well as Table 14.   


datasets.R    The source of all the datasets in the paper.  To load them into an R session use the command source("datasets.R")

datasetdocumentation.R     The Roxygen documentation of the datasets

modslavmse.R    The Roxygen introductory page for the package itself




 



