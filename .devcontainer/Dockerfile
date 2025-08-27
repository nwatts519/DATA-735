FROM rocker/rstudio:latest

RUN R -e "install.packages(c('tidyverse', 'data.table', 'ggplot2'), repos='https://cloud.r-project.org')"

ENV USER vscode
WORKDIR /home/${USER}
