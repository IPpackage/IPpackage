
# Innovare Pesquisa - Pacote R (IPpackage)

Este pacotece é uma coleção abrangente de funcionalidades desenvolvidas para a aplicação de diversas funções do R, que são amplamente utilizadas pelos colaboradores.

   
## Instalação

- Primeira instalação


```bash
  # Verifica se o pacote devtools está instalado
  if (!requireNamespace("devtools", quietly = TRUE)) 
    {

    # Se não estiver instalado, instala o pacote devtools
    install.packages("devtools")

    }
  #Carregando devtools
  library(devtools)

  #Baixando IPpackage
  devtools::install_github("IPpackage/IPpackage",force = TRUE)

  #Carregando IPpackage
  library(IPpackage)
```

- Se o pacote _IPpackage_ já estiver instalado em sua máquina e você desejar atualizá-lo.
```bash
  # Verifica se o pacote devtools está instalado
  if (!requireNamespace("devtools", quietly = TRUE)) 
    {

    # Se não estiver instalado, instala o pacote devtools
    install.packages("devtools")

    }
  #Carregando devtools
  library(devtools)

  #Descarregar IPpackage
  detach("package:IPpackage", unload = TRUE)

  #Desinstalar IPpackage
  remove.packages("IPpackage")

  #Baixando IPpackage
  devtools::install_github("IPpackage/IPpackage",force = TRUE)

  #Carregando IPpackage
  library(IPpackage)
```

![RStudio](https://img.shields.io/badge/RStudio-R-blue.svg)
