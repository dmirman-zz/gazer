FROM rocker/rstudio
RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("ggplot2")'
RUN R -e 'install.packages("data.table")'
RUN R -e 'install.packages("zoo")'
COPY gazer_*.tar.gz /app.tar.gz
RUN remotes::install_local('/app.tar.gz')
CMD R -e 'library(dockerfiler)'
