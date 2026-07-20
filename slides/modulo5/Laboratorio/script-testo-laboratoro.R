# LABORATORIO MODULO 5 ----- 
# Il dataset si chiama DATASW2.csv
## Esercizio 1: 
# Si assegni il dataset al data frame `D` 
# (utilizzare la funzione `read.csv()` e non `read.csv2()` 
# in quanto il file di dati è stato creato in un Excel che utilizzava il “.” come separatore decimale). 
# Visualizzare le statistiche descrittive di base delle variabili del data frame.

D = read.csv("slides/modulo5/Laboratorio/DATASW2.csv")
head(D)
str(D)
# l'importazione è andata a buon fine
# statistiche descrittive solo per le variabili quantitative 
# (considero come quantitative anche le variabili di Successo)
# codificate come zero 1

summary(D[,-1]) # tolgo la variabile gender dal calcolo 


## Esercizio 2: 

#Verificare se l’età media delle donne sia 
# significativamente differente da quella dei maschi. 
# H0: mu_m = mu_f oppure mu_m - mu_f = 0
# H1: mu_m != mu_f oppure mu_m - mu_f != 0

boxplot(D$Age ~ D$Gender)
points(c(mean(D$Age[D$Gender == "f"]),
         mean(D$Age[D$Gender == "f"])), 
       pch = 19)

t.test(D$Age ~ D$Gender)
t.test(D$Age ~ D$Gender, var.equal = T)
t.test((D$Age[D$Gender == "f"]),
       (D$Age[D$Gender == "f"]))
## Esercizio 3: 

# Verificare se il tempo medio impiegato dal gruppo dei 
# maschi nel primo task al tempo t1 sia significativamente differente da 
# quello impiegato (per lo stesso task e allo stesso tempo) dal gruppo delle donne. 
# Ripetere le analisi per il secondo task. 

boxplot(D$TT1t1 ~ D$Gender)
t.test(D$TT1t1 ~ D$Gender, var.equal = TRUE)
qt(.975, 46)
qt(.025, 46)
qt(1-.05/2, 46)

## Esercizio 4: 
# Valutare se il tempo complessivo nel primo task sia 
# significativamente superiore nel gruppo delle persone 
# con età superiore ai 40 anni (rispetto a $\leq 40$). Qui per 
# tempo complessivo si intende la somma dei time-on-task nei 
# due tempi t1 e t2. 
D$newtime = D$TT1t1 + D$TT1t2
D$age_cat = numeric(nrow(D))
D$age_cat = D$Age <= 40 

# H0: mu_>40 =  mu_<40
# H1: mu_>40 >  mu_<40
t.test(D$newtime ~ D$age_cat, 
       alternative = "greater")


## Esercizio 5: 
# Il tempo impiegato dalle persone che hanno successo nel 
# secondo task al tempo t1 è significativamente inferiore 
# rispetto a coloro che falliscono (nel secondo task al tempo t1)? 
  

## Esercizio 6: 
# Verificare se il tempo medio impiegato nel secondo task al 
# tempo t1 sia significativamente superiore rispetto al valore 
# della mediana campionaria calcolata sui tempi registrati al 
# secondo task sempre al tempo t1.


## Esercizio 7: 
# Vi è un miglioramento statisticamente significativo nella performance temporale 
# (time-on-task) del primo compito (task) nel passare dalla fase t1 alla 
# t2? Ripetere l’analisi anche per il secondo compito (task).

boxplot(D$TT1t1, D$TT1t2, 
        names = c("Tempo1", "Tempo2"))

# H0: mut1 = mut2 --> mu_d = 0
# H1: mut1 > mut2 --> mu_d > 0 

t.test(D$TT1t1, D$TT1t2, paired = TRUE, 
       alternative = "greater")

t.test(D$TT1t1, D$TT1t2, paired = TRUE, 
       alternative = "greater", mu = 3)

## Esercizio 8: 

# Ripetere l’esercizio 7, questa volta limitatamente al solo gruppo delle donne. 

## Esercizio 9: 

# Verificare se vi sia una associazione statisticamente 
# significativa tra età (espressa in due classi: $\leq 40$ e $> 40£) e 
# l’ottenimento di un successo nel completamento del primo task 
# alla fase t1. Ripetere l’analisi, questa volta relativamente alla fase t2. 


## Esercizio 10: 

# Vi è una qualche relazione tra successo al task 2 alla fase 
# t2 rispetto al genere della persona? E rispetto al task 1 alla fase 1? 