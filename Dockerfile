
# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
# RUN mkdir /home/shiny-app
WORKDIR /app
# Install R dependencies
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes', 'blockrand', 'tidyverse', 'DT', 'glue'), repos='https://cran.rstudio.com/')"

# Copy the Shiny app code
# COPY app.R /home/shiny-app/app.R
COPY ..

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript app.R
