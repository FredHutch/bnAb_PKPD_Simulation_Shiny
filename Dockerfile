FROM fredhutch/r-shiny-server-base:latest
RUN apt-get update -y
RUN apt-get install -y pandoc supervisor nginx
# RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny



RUN rm -rf /srv/shiny-server/
ADD ./app/*.R /srv/shiny-server/

ADD system/. /home/shiny/system/

# ADD app/. /home/shiny/
# RUN chown -R shiny:shiny /home/shiny 
# WORKDIR /home/shiny
# USER shiny
EXPOSE 8888
CMD ["/bin/sh", "-c", "/usr/bin/supervisord -c /home/shiny/system/sup.conf"]
