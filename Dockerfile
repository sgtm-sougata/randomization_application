# Use an official R base image
FROM rocker/r-ver:latest

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes'), repos='https://cran.rstudio.com/')"

# Create and set the working directory
WORKDIR /srv/shiny-server/myapp

# Copy the Shiny app files into the Docker image
COPY . /srv/shiny-server/myapp

# Expose the Shiny port
EXPOSE 3838

# Run the Shiny app on container start
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/myapp')"]
