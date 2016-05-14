#!/bin/bash


ip=$1;
http_port=$2;
ws_port=$3;

stack build asteroids && stack exec client $ip $http_port $ws_port;
