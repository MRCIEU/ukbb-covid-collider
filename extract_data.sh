#!/bin/bash

phenfile=$(cat config.json | jq -r  '.["phenfile"]')
covidfile=$(cat config.json | jq -r  '.["covidfile"]')

row=$(head -n 1 "$phenfile" | tr ',' '\n' | grep -n "23104-0.0" | cut -d ":" -f 1)
cut -d "," -f 1,$row "$phenfile" > data/bmi.txt

row=$(head -n 1 "$phenfile" | tr ',' '\n' | grep -n "34-0.0" | head -n 1 | cut -d ":" -f 1)
cut -d "," -f 1,$row "$phenfile" > data/age.txt

row=$(head -n 1 "$phenfile" | tr ',' '\n' | grep -n "31-0.0" | head -n 1 | cut -d ":" -f 1)
cut -d "," -f 1,$row "$phenfile" > data/sex.txt

cp "$covidfile" data
