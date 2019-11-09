FROM rocker/rstudio
RUN R -e "install.packages("remotes", dependencies=TRUE, repos='cran.rstudio.com')"
RUN R -e "install.packages("ggplot2", dependencies=TRUE, repos='cran.rstudio.com')"
RUN R -e "install.packages("data.table", dependencies=TRUE, repos='cran.rstudio.com')"
RUN R -e "install.packages("zoo", dependencies=TRUE, repos='cran.rstudio.com')"
COPY gazer_*.tar.gz /app.tar.gz
RUN remotes::install_local('/app.tar.gz')
CMD R -e 'library(dockerfiler)'
