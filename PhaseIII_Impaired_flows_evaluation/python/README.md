Calculate impaired flows
===

Original code provided by Daron Pedroja (CA State Water Resources Control Board, Division of Water Rights).

The code has been modified to make some corrections (for example, October was missing from the output), and extended to handle multiple datasets (`inputs.yaml`). We also added a process_inputs notebook to clean up input data, and a summarize notebook to help analyze why some of the reports contain negative impaired flows. The conda `environment.yml` has been modified by adding just a package or two.


## Some usage notes

We have been running these three scripts in order:
1. `impaired_flows_process_inputs` to clean up input data.
2. `impaired_flows_calculations_NC` to run the main calculations
3. `impaired_flows_summarize` to create some summary data

The code assumes you're running it from the `python` folder (that this README is in).

You can find commands to convert the Python notebooks into Python script files if you don't want to use the notebooks.

The code also assumes you have an inputs.yaml set up to specify the input files. As an example, it may look something like this:

```yaml
demand_data:
  demand_consumption: inputs/annual_demand_consumptive_only.csv
  demand_coordinates: inputs/annual_demand_coordinates_POD_IDs.csv
datasets:
- output_directory: outputs/nc_sites_final
  basins_filepath: processed_inputs/delineations/NC_sites_FINAL_watersheds_v2.shp
  flow_filepath: processed_inputs/NC_sites_FINAL_v2_unimpaired_flow.csv
- output_directory: outputs/biosites
  basins_filepath: inputs/LOI_delineations/LOI_delineations/Biosites_NCoast_delineations_QA.shp
  flow_filepath: processed_inputs/N_Coast_BioSites_Unimpaired_Flow.csv
```

You can add more datasets by following a similar pattern.
