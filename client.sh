#!/bin/bash


ip=$1;
http_port=$2;

stack build asteroids && stack exec client $ip $http_port;
