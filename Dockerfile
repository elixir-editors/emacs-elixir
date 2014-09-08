FROM ubuntu:14.04

MAINTAINER Matt DeBoard "https://twitter.com/matt_deboard"

CMD /bin/bash
RUN apt-get update && apt-get install -y emacs
    
