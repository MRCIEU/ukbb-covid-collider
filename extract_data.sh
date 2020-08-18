#!/bin/bash

phenfile=$(cat config.json | jq -r  '.["phenfile"]')
covidfile=$(cat config.json | jq -r  '.["covidfile"]')
linkerfile=$(cat config.json | jq -r  '.["linkerfile"]')
pcfile=$(cat config.json | jq -r  '.["pcfile"]')

row=$(head -n 1 "$phenfile" | tr ',' '\n' | grep -n "23104-0.0" | cut -d ":" -f 1)
cut -d "," -f 1,$row "$phenfile" > data/bmi.txt

row=$(head -n 1 "$phenfile" | tr ',' '\n' | grep -n "34-0.0" | head -n 1 | cut -d ":" -f 1)
cut -d "," -f 1,$row "$phenfile" > data/age.txt

row=$(head -n 1 "$phenfile" | tr ',' '\n' | grep -n "31-0.0" | head -n 1 | cut -d ":" -f 1)
cut -d "," -f 1,$row "$phenfile" > data/sex.txt

cut -d " " -f 1,5 "$pcfile" > data/pc1.txt

cp "$covidfile" data
cp "$linkerfile" data




