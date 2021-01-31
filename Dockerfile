FROM python:2.7-slim

RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install jq sed curl

WORKDIR /usr/src/app

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

