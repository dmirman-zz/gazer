FROM rocker/rstudio

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "6c8fdaa")'
RUN R -e 'install.packages("data.table")'
RUN R -e 'install.packages("tidyverse")'
RUN R -e 'install.packages("zoo")'

COPY . github.com/dmirman/gazer

