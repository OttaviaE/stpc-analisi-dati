# funzione per rederizzare i file ---- 
library(rmarkdown)   # per yaml_front_matter()

name_file = "LaboratorioRmod4_esercizi.qmd"
mydir = "/slides/modulo4/Laboratorio/"

setwd(paste0(getwd(), mydir))

myfile = name_file

# leggi il YAML dal file
meta <- rmarkdown::yaml_front_matter(myfile)
subtitle <- tolower(meta$subtitle)
name_file = tolower(name_file)

if (subtitle == "soluzioni") {
  name_file = gsub("esercizi", "soluzioni", name_file)
} 

name_file = gsub(".qmd",".pdf", name_file)

outfile <- name_file

# render
quarto::quarto_render(
  input = myfile,
  output_file = outfile
)
