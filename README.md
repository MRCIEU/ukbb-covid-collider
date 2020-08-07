# UK Biobank example of collider bias in Covid-19 test data

This repo provides a set of example analyses as described in the paper [Collider bias undermines our understanding of COVID-19 disease risk and severity](https://www.medrxiv.org/content/10.1101/2020.05.04.20090506v3).

## Setup

To run the snakemake pipeline, which produces the `analysis.html` file, first create a `config.json` file with paths to phenotype and COVID-19 linked test data files e.g.

```
{
    "phenfile": "/path/to/data.csv",
    "covidfile": "/path/to/covid19_result_2020_06_05.txt"
}
```


## Info

IEU project portal: https://ieuportal.epi.bris.ac.uk/project_detail/29352005-dde2-47c1-b9af-47c513987078/
