library(readr)
pacman::p_load(readr, tidyverse, dplyr, sjmisc, car, sjlabelled, stargazer, haven)

# ANTROPOLOGIA
antropo <- read_csv("input/antropo.csv")

antropo$Modalidad <- set_label(x = antropo$Modalidad,label = 
                                                   "Modalidad")

get_label(antropo$Modalidad)


antropo$Temática <- set_label(x = antropo$Temática,label = 
                                 "Temática")

get_label(antropo$Modalidad)

sjmisc::frq(antropo$`Número de sesión`)
sjmisc::frq(antropo$Modalidad)
sjmisc::frq(antropo$Temática)

summarytools::ctable(antropo$Modalidad, antropo$Temática)
antropo %>%
  sjPlot::sjtab(Modalidad,
                Temática,
                encoding = 'UFT-8',
                show.col.prc = TRUE
                )


sjmisc::frq(antropo$`Señale tutora/es asistentes`)

# SOCIOLOGIA
socio <- read_csv("input/socio.csv")

sjmisc::frq(socio$`Número de sesión`)
sjmisc::frq(socio$Modalidad)
sjmisc::frq(socio$Temática)

summarytools::ctable(socio$Modalidad, socio$Temática)
socio %>%
  sjPlot::sjtab(Modalidad,
                Temática,
                encoding = "UFT-8",
                show.col.prc = TRUE
  )


sjmisc::frq(socio$`Señale tutora/es asistentes`)

# PEDAGOGIA EN EDUCACION PARVULARIA 
pep <- read_csv("input/pep.csv")
pep %>%
  sjPlot::sjtab(Modalidad,
                Temática,
                encoding = "UFT-8",
                show.col.prc = TRUE
  )
# TRABJO SOCIAL 
ts <- read_csv("input/ts.csv")

sjmisc::frq(socio$`Número de sesión`)
sjmisc::frq(ts$Modalidad)
sjmisc::frq(ts$Temática)

ts %>%
  sjPlot::sjtab(Modalidad,
                Temática,
                encoding = "UFT-8",
                show.col.prc = TRUE
  )

# PSICOLOGIA
psico <- read_csv("input/psico.csv")
psico %>%
  sjPlot::sjtab(Modalidad,
                Temática,
                encoding = "UFT-8",
                show.col.prc = TRUE
  )