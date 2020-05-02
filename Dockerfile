FROM alpine:3.11

RUN apk update && apk add guile

COPY . /app

WORKDIR /app

EXPOSE 8080

CMD guile -l rip/rip.scm server.scm
