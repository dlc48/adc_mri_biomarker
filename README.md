README

###############################################################################
Source code and data for the manuscript "MRI biomarker ADC better predicts 
	tumor cellularity than necrosis in a multi-center multi-vendor study 
	of eleven rodent tumor models”, by Honess et al (2024)
###############################################################################

For questions, comments or remarks about the code please contact  
- Davina Honess (Davina.Honess@cruk.cam.ac.uk)
- Dominique-Laurent Couturier (dlc48@cam.ac.uk)

The code has been written using R version 4.3.0 (platform: aarch64-apple-darwin20, 
64-bit) with package gamlss_5.4-20


File organisation:
- input/data contains the required pre-processed datasets
- input/r contains R scripts to be sourced to load required functions and
  run specific analyses (like cross-validation, for example)
- (input/html contains the html style preferences)
- output/figures/ empty directory in which figures will be saved 
- output/rdata/ empty directory in which rdata related to intermediary results
  will be saved

To reproduce the results presented in the manuscript (i.e, Tables 1 and S2, 
as well as Figures 2, 3, 4, 5 and S3), just run the code of the R markdown document 
'main.Rmd'. The code will
- load the data
- print Tables in the resulting html output 
- save intermediary results as R data file in the directory 'output/rdata/'
- save Figures in pdf format in the directory 'output/figures/'





