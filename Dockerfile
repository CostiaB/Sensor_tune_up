# Rocker shiny link:
# https://rocker-project.org/images/versioned/shiny.html

# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# Package System Prerequisites link (find "Install System Prerequisites" on the page):
# https://packagemanager.rstudio.com/client/#/repos/2/packages/reshape2


# Get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5

# Install system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libicu-dev \
    make \
    zlib1g-dev \
    pandoc
    
    
# Install R packages    
RUN R -e 'install.packages(c( \
    "shiny", \
    "ggplot2", \
    "plotly", \
    "formattable", \
    "shinydashboard", \
    "shinyjs", \
    "shinyWidgets", \
    "reshape2" \
    ), \
    repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23")'
    
    
# Copy the app into the image
COPY ./Sensor-adjustment/* /srv/shiny-server/


# Run app
CMD ["/usr/bin/shiny-server"]
    
	
