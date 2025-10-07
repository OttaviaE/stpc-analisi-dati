library(roxygen2)

setwd("C:/Users/Ottavia/Documents/GitHub/stpc-analisi-dati/slides/modulo4/")

# genera la documentazione
roxygen2::roxygenise("DisegnaNormale.R")

# 1. Specifica il file con le funzioni
input_file <- "C:/Users/Ottavia/Documents/GitHub/stpc-analisi-dati/slides/modulo4/DisegnaNormale.R"   # <-- cambia con il tuo file
output_pdf <- "C:/Users/Ottavia/Documents/GitHub/stpc-analisi-dati/slides/modulo4/DisegnaNormale.pdf"

# 2. Leggi il contenuto del file
txt <- readLines(input_file)

# 3. Processa i blocchi roxygen
rd <- roc_proc_text(rd_roclet(), paste(txt, collapse = "\n"))

# 4. Crea cartella man/ per gli Rd
dir.create("man", showWarnings = FALSE)

# 5. Salva ogni Rd in un file
for (obj in rd) {
  name <- attr(obj, "Rdname")
  file <- file.path("man", paste0(name, ".Rd"))
  cat(format(obj), file = file)
}

# 6. Genera PDF unico
Rd2pdf("man", output = output_pdf)

cat("📄 Documentazione generata in:", output_pdf, "\n")