FROM node:15.14.0

ENV HOME=/app
WORKDIR /app

COPY package.json package-lock.json /app/

RUN npm install

COPY elm-package.json /app/

RUN node_modules/.bin/elm-make --yes

COPY . .

ENV SHELL=/bin/bash

CMD npm run watch
