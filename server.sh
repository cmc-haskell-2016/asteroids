#!/bin/bash


http_port=$1;
ws_port=$2;

stack build asteroids && stack exec server $http_port $ws_port;
