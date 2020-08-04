# Use node image as a parent image
FROM node:9-alpine

# Copy the local codes in the directory to the image
ADD . /src

# Setup the working directory and install the package
RUN apk update && apk add bash && \
    cd /src; npm install 

# Inform Docker that the container is listening on the specified port at runtime.
EXPOSE  8091

# Running the script after container start.
CMD node /src/bin/www
