#!/bin/bash

http_port=$1;

stack build asteroids && stack exec server $http_port;
