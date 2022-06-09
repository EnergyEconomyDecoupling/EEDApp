# Base image
FROM zekemarshall/eed-app-base-image:latest

ENV SSH_PASSWD "root:Docker!"
RUN apt-get update \
        && apt-get install -y --no-install-recommends dialog \
        && apt-get update \
  && apt-get install -y --no-install-recommends openssh-server \
  && echo "$SSH_PASSWD" | chpasswd

# Copy necessary files
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## www folder
COPY /www /www
## ReboundTools documenetation .Rmd file
COPY /reboundtools_doc.Rmd /reboundtools_doc.Rmd

# Expose port, 2222 port is used for SSH access
EXPOSE 3838 2222

COPY sshd_config /etc/ssh/
COPY init_container.sh /opt/startup
RUN chmod 755 /opt/startup/init_container.sh
ENTRYPOINT ["/opt/startup/init_container.sh"]

# Run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
