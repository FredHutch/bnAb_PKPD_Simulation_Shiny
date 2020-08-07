FROM fredhutch/r-shiny-base:latest
RUN apt-get update
RUN apt-get install -y pandoc
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
ADD app/. /home/shiny/
RUN chown -R shiny:shiny /home/shiny 
RUN R -e "install.packages('pracma')"
WORKDIR /home/shiny
USER shiny
EXPOSE 7777
CMD Rscript start.R 
