#!/bin/bash

http_port=$1;
game_mode=$2

stack build asteroids && stack exec server $http_port $game_mode;
