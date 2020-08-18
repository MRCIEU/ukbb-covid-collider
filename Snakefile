import os
os.makedirs("docs", exist_ok=True)
os.makedirs('data', exist_ok=True)

rule all:
	input:
		"docs/analysis.html"

rule extract:
	output:
		"data/bmi.txt"
	shell:
		"./extract_data.sh"

rule organise:
	input:
		"data/bmi.txt"
	output:
		"data/dat.rdata"
	shell:
		"Rscript organise_data.r"

rule analysis:
	input:
		"data/dat.rdata"
	output:
		"docs/analysis.html"
	shell:
		"Rscript -e 'rmarkdown::render(\"analysis.rmd\", output_dir=\"docs\")'"
