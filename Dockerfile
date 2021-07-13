FROM fpco/stack-build:lts-17.3 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8B1DA6120C2BF624 \
  && apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# ------------------------------------------------------------------------
FROM fpco/stack-build:lts-17.3 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -----------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

RUN apt update && apt install -y libpq-dev ca-certificates

COPY --from=build /opt/build/bin .
COPY --from=build /opt/build/static ./static
COPY --from=build /opt/build/config ./config
ENV YESOD_PORT 3333
EXPOSE 3333
CMD ["/opt/app/expense-tracker"]
