FROM ubuntu:16.04
RUN mkdir -p /opt/ikapi/
ARG BINARY_PATH
WORKDIR /opt/ikapi
RUN apt-get update && apt-get install -y \
  ca-certificates
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
COPY "$BINARY_PATH" /opt/ikapi
CMD ["/opt/ikapi/ikapi-exe"]