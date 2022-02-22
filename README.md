# bnAb PKPD Shiny Simulation

[https://bnabpkpd.fredhutch.org](https://bnabpkpd.fredhutch.org)

Shiny app for simulation bnAb behavior


Steps for updating (bash/cmd line):
- update code
- rebuild docker
  - docker ps -a
  - docker build -t bnabratioopt:latest .
  - docker stop bnabratioopt; docker rm bnabratioopt
- add, commit, push

To run locally:
 - docker run -d --name bnabratioopt -p 8888:8888 bnabratioopt:latest
 - open your browser and go to localhost:8888


