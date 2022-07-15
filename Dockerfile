# Base image
FROM zekemarshall/eed-app-base-image:latest

# Expose port
# Add additional packages to enable debugging and monitoring
RUN apt-get update \
    && apt-get clean

# Set the root password to a standard value, to enable SSH from Azure
RUN echo "root:Docker!" | chpasswd

# Copy necessary files
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## www folder
COPY /www /www
## ReboundTools documenetation .Rmd file
COPY /reboundtools_doc.Rmd /reboundtools_doc.Rmd

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
