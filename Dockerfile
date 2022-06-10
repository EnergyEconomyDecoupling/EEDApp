# Base image
FROM zekemarshall/eed-app-base-image:latest

# Expose port
# Add additional packages to enable debugging and monitoring
RUN apt-get update \
    && apt-get install --yes --no-install-recommends openssh-server curl vim less \
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

# Copy setup files for SSH
COPY sshd_config /etc/ssh/sshd_config
COPY init_container.sh /etc/services.d/sshd/run

# Expose SSH and Shiny ports
EXPOSE 2222 3838
