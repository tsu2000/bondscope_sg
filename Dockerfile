FROM rocker/shiny:4.3.1
RUN install2.r rsconnect shinythemes tidyverse plotly DT lubridate forecast xts gt
WORKDIR /home/exbook
COPY app.R app.R 
COPY data.csv data.csv
COPY deploy.R deploy.R
CMD Rscript deploy.R
