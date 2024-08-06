# Mmrm Simulation

## Usage

### R simulation

R simulations can be done on any platform where R and snakemake is available. Current snakefile is also compatible with HPC where batch system like lsf is installed.

Make sure the lsf plugin for snakemake is installed.

```{sh}
pip install snakemake-executor-plugin-lsf
```

Then you can use snakemake to submit jobs.

```{sh}
snakemake --executor lsf all_r_analysis
```

The data generation step is using a specific seed so reproducibility is ensured.

### SAS simulation

The simulation in SAS can be completed on systems where SAS and R are both available.
If you don't have machines that both are available, the execution should take two steps.

1. Generate the data using `snakemake all_simulated_data` (this must run on R machine)
1. Generate the estimate using `snakemake all_sas_analysis` (this must run on SAS machine)
