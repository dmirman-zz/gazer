FROM rocker/tidyverse

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "6c8fdaa")'
RUN R -e 'install.packages("data.table")'
RUN R -e 'install.packages("zoo")'
RUN R -e 'remotes::install_github("tmalsburg/saccades")'
COPY gazer_*.tar.gz /app.tar.gz
RUN remotes::install_local('/app.tar.gz')
CMD R -e 'library(dockerfiler)'
