serve:
	echo "Shiny document can be accessed at <server address>/main_document.Rmd";\
	R -e "library(rmarkdown); rmarkdown::run('src/main_document.Rmd')";