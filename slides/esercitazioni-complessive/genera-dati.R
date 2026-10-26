# Generazione del dataset "benessere_lavoro.csv"
# Esercitazione di pratica (moduli 2 e 3) - psicologia del lavoro
# Solo R base

set.seed(2610)
n <- 200

# Anzianita' in azienda: junior (1-3 anni), intermedio (4-6), senior (7-20)
anzianita <- sample(c("junior", "intermedio", "senior"), n,
                    replace = TRUE, prob = c(0.35, 0.30, 0.35))

anni_servizio <- numeric(n)
anni_servizio[anzianita == "junior"] <- sample(1:3, sum(anzianita == "junior"), replace = TRUE)
anni_servizio[anzianita == "intermedio"] <- sample(4:6, sum(anzianita == "intermedio"), replace = TRUE)
anni_servizio[anzianita == "senior"] <- sample(7:20, sum(anzianita == "senior"), replace = TRUE)

# Eta': almeno 23 anni piu' gli anni di servizio
eta <- 23 + anni_servizio + sample(0:15, n, replace = TRUE)

genere <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.6, 0.4))

titolo_studio <- sample(c("diploma", "laurea triennale", "laurea magistrale"), n,
                        replace = TRUE, prob = c(0.3, 0.4, 0.3))

reparto <- sample(c("amministrazione", "produzione", "commerciale", "risorse umane"), n,
                  replace = TRUE)

# Supporto sociale percepito (fattore di protezione), punteggio da 1 a 7
supporto_sociale <- round(rnorm(n, mean = 4.5, sd = 1.1), 1)
supporto_sociale <- pmin(pmax(supporto_sociale, 1), 7)

# Burnout (esaurimento emotivo), punteggio intero da 0 a 54:
# cresce con gli anni di servizio, un po' piu' alto per le donne,
# diminuisce con il supporto sociale
burnout <- 35 + 0.8 * anni_servizio + 1.5 * (genere == "F") -
  4.3 * supporto_sociale + rnorm(n, mean = 0, sd = 6)
burnout <- round(pmin(pmax(burnout, 0), 54))

dati <- data.frame(
  id = 1:n,
  genere = genere,
  eta = eta,
  anni_servizio = anni_servizio,
  anzianita = anzianita,
  titolo_studio = titolo_studio,
  reparto = reparto,
  supporto_sociale = supporto_sociale,
  burnout = burnout
)

# ---- Controlli -------------------------------------------------------------
str(dati)
summary(dati[, c("eta", "anni_servizio", "supporto_sociale", "burnout")])
table(dati$anzianita)

# correlazioni (attese: burnout-supporto circa -0.5; burnout-anni circa +0.5)
cor(dati$burnout, dati$supporto_sociale)
cor(dati$burnout, dati$anni_servizio)

# percentuale di burnout alto (cut-off 27) per anzianita' e per genere
# (attese circa: junior 15%, intermedio 20-25%, senior 50-55%; F e M vicine)
alto <- dati$burnout >= 27
round(prop.table(table(dati$anzianita, alto), margin = 1), 2)
round(prop.table(table(dati$genere, alto), margin = 1), 2)

# ---- Esportazione ----------------------------------------------------------
# separatore di colonna ";" e separatore decimale ","
write.csv2(dati, "data/benessere_lavoro.csv", row.names = FALSE, quote = FALSE)
