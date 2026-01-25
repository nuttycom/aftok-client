FROM node:18-bookworm

LABEL maintainer="Kris Nuttycombe <kris@aftok.com>"

ENV LANG=C.UTF-8
ENV TZ=America/Denver
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install additional dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    git ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/aftok/client
WORKDIR /opt/aftok/client

ADD ./package.json /opt/aftok/client/package.json
ADD ./package-lock.json /opt/aftok/client/package-lock.json

RUN npm ci
ENV PATH="./node_modules/.bin:${PATH}"

# Add static assets via submodule
ADD ./aftok.com /opt/aftok/client/aftok.com
ADD ./dev /opt/aftok/client/dev
RUN ln -sf ../aftok.com/src/assets /opt/aftok/client/dev/assets

# Add purescript build config & sources
ADD ./spago.yaml /opt/aftok/client/spago.yaml
ADD ./src /opt/aftok/client/src

RUN npm run build-parcel

# Add dist-volume directory for use with docker-compose sharing
# of client executables via volumes.
ADD ./docker/aftok-client-cp.sh /opt/aftok/
RUN chmod 700 /opt/aftok/aftok-client-cp.sh
RUN mkdir /opt/aftok/client/dist-volume
