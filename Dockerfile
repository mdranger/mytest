FROM node:9-alpine
ADD . /src

RUN apk update && apk add bash && \
    cd /src; npm install 
    
EXPOSE  8091
CMD node /src/bin/www
