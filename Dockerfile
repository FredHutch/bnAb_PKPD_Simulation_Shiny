FROM fredhutch/r-shiny-server-base:latest
RUN apt-get update -y
RUN apt-get install -y pandoc
# RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny



RUN rm -rf /srv/shiny-server/
ADD ./app/*.R /srv/shiny-server/

# ADD system/. /home/shiny/system/
RUN R -e "install.packages(c('pracma', 'shinycssloaders'), repos = 'http://cran.us.r-project.org')"

# ADD app/. /home/shiny/
# RUN chown -R shiny:shiny /home/shiny 
# WORKDIR /home/shiny
# USER shiny
# EXPOSE 8888
# CMD ["/bin/sh", "-c", "/usr/bin/supervisord -c /home/shiny/system/sup.conf"]
WORKDIR /srv/shiny-server/
CMD /usr/bin/shiny-server.sh