# Impaired Flows Calculation for the North Coast

# Inputs -
# supply: NC_sites_FINAL_v2_unimpaired_flow.csv
# demand: annual_demand_consumptive_only.csv + annual_demand_coordinates_POD_IDs.csv
# Outputs -
# daily & monthly impaired flows (supply-demand)

# ## Processing steps:
# 1. merge demand dataset with coordinates dataset
# 2. make demand dataset a geodataframe
# 3. join demand geodataframe with basins geodataframe
# 4. sum the monthly demand for each basin (represented by uniqueID field)
# 5. get demand dataset into daily timesteps
# 6. convert unimpaired flows from cfs to acre-feet per day
# 7. subtract demand from unimpaired flows

import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point

# from pyproj import Transformer
import utm
from pathlib import Path
import yaml
import pprint

# READ YAML CONFIG:
INPUT_FILENAME = "inputs.yaml"
try:
    with open(INPUT_FILENAME) as config_file:
        input_config = yaml.safe_load(config_file)
except FileNotFoundError:
    print(f"Error loading '{INPUT_FILENAME}'. File not found.")
except yaml.YAMLError:
    print(f"Error parsing YAML file '{INPUT_FILENAME}'.")
except Exception as e:
    print(f"An unexpected error occurred: {e}")

print("Input config:")
pprint.pprint(input_config)


# combine coordinate dataset and demand dataset
demand_consumption = Path(input_config["demand_data"]["demand_consumption"])
demand_coordinates = Path(input_config["demand_data"]["demand_coordinates"])
demand_data = pd.read_csv(demand_consumption)
coordinates_data = pd.read_csv(demand_coordinates)

# Perform a left join on the two datasets using APPLICATION_NUMBER as the key
merged_demand = pd.merge(
    demand_data, coordinates_data, on="APPLICATION_NUMBER", how="left"
)

##### Convert the merged_demand table to a GeoDataFrame #####################
merged_demand["LONGITUDE"] = merged_demand["LONGITUDE"].astype("float")
merged_demand["LATITUDE"] = merged_demand["LATITUDE"].astype("float")
easting = []
northing = []

for _, row in merged_demand.iterrows():
    lon = row["LONGITUDE"]
    lat = row["LATITUDE"]

    # Convert lat/lon to UTM
    utm_coords = utm.from_latlon(latitude=lat, longitude=lon)

    # Append results to lists
    easting.append(utm_coords[0])
    northing.append(utm_coords[1])


merged_demand["EASTING"] = easting
merged_demand["NORTHING"] = northing
merged_demand["geometry"] = merged_demand.apply(
    lambda row: Point(row["EASTING"], row["NORTHING"]), axis=1
)

demand_gdf = gpd.GeoDataFrame(merged_demand, geometry="geometry", crs="EPSG:26910")


#### Function to process a watershed
def per_dataset(dataset):
    basins_filepath = Path(dataset["basins_filepath"])
    flow_filepath = Path(dataset["flow_filepath"])
    outputs_dir = Path(dataset["output_directory"])
    os.makedirs(outputs_dir, exist_ok=True)
    daily_filepath = outputs_dir / "daily.csv"
    monthly_filepath = outputs_dir / "monthly.csv"

    input_dateformat = r"%Y-%m-%d"
    output_dateformat = r"%m/%d/%Y"
    begin_data_cutoff_date = "2015-01-01"
    end_data_cutoff_date = "2023-12-31"

    #### Read in your basins geodataframe: ######################
    basins_gdf = gpd.read_file(basins_filepath)
    basins_gdf = basins_gdf.to_crs("EPSG:26910")

    if not "siteID" in basins_gdf.columns:
        if "UniquID" in basins_gdf.columns:
            basins_gdf = basins_gdf.rename(columns={"UniquID": "UniqueID"})
        basins_gdf["siteID"] = basins_gdf["UniqueID"]

    ##### Join the basins to the demand dataset ###########################
    joined_gdf = gpd.sjoin(basins_gdf, demand_gdf, how="inner", predicate="intersects")

    # Restructure the joined_df (demand table with basins) to simplify
    joined_gdf_restructured = joined_gdf[
        [
            "APPLICATION_NUMBER",
            "siteID",
            "REPORTING_YEAR",
            "JAN_EXPECTED_TOTAL_DIVERSION",
            "FEB_EXPECTED_TOTAL_DIVERSION",
            "MAR_EXPECTED_TOTAL_DIVERSION",
            "APR_EXPECTED_TOTAL_DIVERSION",
            "MAY_EXPECTED_TOTAL_DIVERSION",
            "JUN_EXPECTED_TOTAL_DIVERSION",
            "JUL_EXPECTED_TOTAL_DIVERSION",
            "AUG_EXPECTED_TOTAL_DIVERSION",
            "SEP_EXPECTED_TOTAL_DIVERSION",
            # Added OCT here; it was missing
            "OCT_EXPECTED_TOTAL_DIVERSION",
            "NOV_EXPECTED_TOTAL_DIVERSION",
            "DEC_EXPECTED_TOTAL_DIVERSION",
        ]
    ].copy()

    joined_gdf_restructured.rename(
        columns={
            col: col.replace("_EXPECTED_TOTAL_DIVERSION", "")
            for col in joined_gdf_restructured.columns
        },
        inplace=True,
    )

    # Restructure the dataframe
    joined_gdf_restructured_long = joined_gdf_restructured.melt(
        id_vars=["APPLICATION_NUMBER", "siteID", "REPORTING_YEAR"],
        value_vars=[
            "JAN",
            "FEB",
            "MAR",
            "APR",
            "MAY",
            "JUN",
            "JUL",
            "AUG",
            "SEP",
            # Added OCT here; it was missing
            "OCT",
            "NOV",
            "DEC",
        ],
        var_name="MONTH",
        value_name="MONTHLY_DEMAND_AF",
    )

    # Map month abbreviations to numbers
    month_mapping = {
        "JAN": "01",
        "FEB": "02",
        "MAR": "03",
        "APR": "04",
        "MAY": "05",
        "JUN": "06",
        "JUL": "07",
        "AUG": "08",
        "SEP": "09",
        # Added OCT here; it was missing
        "OCT": "10",
        "NOV": "11",
        "DEC": "12",
    }

    # Add a DATE column in yyyy-mm format
    joined_gdf_restructured_long["MONTH_NUM"] = joined_gdf_restructured_long[
        "MONTH"
    ].map(month_mapping)
    joined_gdf_restructured_long["DATE"] = (
        joined_gdf_restructured_long["REPORTING_YEAR"].astype(str)
        + "-"
        + joined_gdf_restructured_long["MONTH_NUM"]
    )

    # Drop the intermediate MONTH_NUM column
    joined_gdf_restructured_long.drop(columns=["MONTH_NUM"], inplace=True)

    # Reorder columns
    joined_gdf_restructured_long = joined_gdf_restructured_long[
        ["APPLICATION_NUMBER", "siteID", "DATE", "MONTHLY_DEMAND_AF"]
    ]

    print("JOINED_GDF_RECONSTRUCTED_LONG DATA: ")
    print(joined_gdf_restructured_long.head())

    # Group by 'siteID' and 'DATE' and sum the 'MONTHLY_DEMAND_AF'
    total_monthly_demand = joined_gdf_restructured_long.groupby(
        ["siteID", "DATE"], as_index=False
    )["MONTHLY_DEMAND_AF"].sum()
    # create a DAYS_IN_MONTH column to calculate the average daily demand
    total_monthly_demand["DAYS_IN_MONTH"] = pd.to_datetime(
        total_monthly_demand["DATE"]
    ).dt.days_in_month
    print("TOTAL_MONTHLY_DEMAND DATA:")
    print(total_monthly_demand.head())

    # Create the total_daily_demand DataFrame
    total_daily_demand = total_monthly_demand.copy()

    # Generate daily dates for each month
    total_daily_demand["DATE"] = total_daily_demand.apply(
        lambda row: pd.date_range(
            start=row["DATE"], periods=row["DAYS_IN_MONTH"], freq="D"
        ),
        axis=1,
    )

    # Explode the DATE column to create a row for each day
    total_daily_demand = total_daily_demand.explode("DATE", ignore_index=True)

    # Set the DAILY_DEMAND_AF value for each day
    total_daily_demand["DAILY_DEMAND_AF"] = (
        total_daily_demand["MONTHLY_DEMAND_AF"] / total_daily_demand["DAYS_IN_MONTH"]
    )

    # Drop unnecessary columns
    total_daily_demand = total_daily_demand[["siteID", "DATE", "DAILY_DEMAND_AF"]]

    print("TOTAL_DAILY_DEMAND DATA:")
    print(total_daily_demand.head())

    # read in unimpaired flow file

    print(f"Reading {flow_filepath}")
    flow = pd.read_csv(flow_filepath)

    ### make sure you replace stationcod with the column name that identifies your basins.  ####
    flow["UniqueID"] = flow["unique_ID"].astype("str")

    flow["date"] = pd.to_datetime(flow["date"], format=input_dateformat)
    flow_filtered = flow[
        (flow["date"] >= begin_data_cutoff_date)
        & (flow["date"] <= end_data_cutoff_date)
    ]
    flow_filtered = flow_filtered.rename(
        columns={
            "UniqueID": "siteID",
            "date": "DATE",
            "flow_cfs": "flow_cfs_unimpaired",
        }
    )

    # Perform an inner join on 'siteID' and 'DATE'
    daily_impaired_flows = pd.merge(
        flow_filtered, total_daily_demand, on=["siteID", "DATE"], how="left"
    )

    columns = ["DATE", "siteID", "flow_cfs_unimpaired", "DAILY_DEMAND_AF"]
    print("daily_impaired_flows columns:")
    print(daily_impaired_flows.columns)
    daily_impaired_flows = daily_impaired_flows[columns]

    # Convert flow_cfs to acre-feet per day and rename the column
    daily_impaired_flows["flow_af_unimpaired"] = (
        daily_impaired_flows["flow_cfs_unimpaired"] * 1.983
    )  
    # convert cfs to acre-feet per day
    daily_impaired_flows["flow_af_impaired"] = (
        daily_impaired_flows["flow_af_unimpaired"]
        - daily_impaired_flows["DAILY_DEMAND_AF"]
    )
    daily_impaired_flows = daily_impaired_flows.rename(
        columns={"DATE": "date", "DAILY_DEMAND_AF": "demand_af"}
    )

    daily_impaired_flows["flow_cfs_impaired"] = (
        daily_impaired_flows["flow_af_impaired"] / 1.983
    )

    # Match original output format column order
    daily_impaired_flows = daily_impaired_flows[
        [
            "date",
            "flow_cfs_unimpaired",
            "siteID",
            "flow_af_unimpaired",
            "demand_af",
            "flow_af_impaired",
            "flow_cfs_impaired",
        ]
    ]
    
    # Fill NaN with 0
    daily_impaired_flows = daily_impaired_flows.fillna(0)
    print("DAILY_IMPAIRED_FLOW DATA:")
    print(daily_impaired_flows.head())

    # Sort by siteID and date, then format the date as desired; finally write to CSV
    daily_impaired_flows = daily_impaired_flows.set_index(
        ["siteID", "date"]
    ).sort_index()
    df_reset = daily_impaired_flows.reset_index()
    df_reset["date"] = pd.to_datetime(df_reset["date"])
    df_reset["date"] = df_reset["date"].dt.strftime(output_dateformat)
    df_reset.to_csv(daily_filepath, index=False)

    # Add a 'MONTH' column to represent the year and month
    # Using .loc to explicitly create a new column, avoiding the SettingWithCopyWarning
    print(flow_filtered.dtypes)
    flow_filtered["DATE"] = flow_filtered["DATE"].dt.to_period("M")

    # Group by 'siteID' and 'MONTH', then sum the 'flow_cfs'
    monthly_flow = flow_filtered.groupby(["siteID", "DATE"], as_index=False)[
        "flow_cfs_unimpaired"
    ].sum()

    print("MONTHLY_FLOW DATA:")
    print(monthly_flow.head())

    total_monthly_demand["DATE"] = pd.to_datetime(total_monthly_demand["DATE"])
    monthly_flow["DATE"] = pd.to_datetime(total_monthly_demand["DATE"])
    monthly_impaired_flow = pd.merge(
        monthly_flow, total_monthly_demand, on=["siteID", "DATE"], how="left"
    )

    monthly_impaired_flow["flow_af_unimpaired"] = (
        # Fixed af calculation to account for days in month
        monthly_impaired_flow["flow_cfs_unimpaired"]
        * 1.983
        * monthly_impaired_flow["DAYS_IN_MONTH"]
    )

    # convert cfs to acre-feet per day
    monthly_impaired_flow["flow_af_impaired"] = (
        monthly_impaired_flow["flow_af_unimpaired"]
        - monthly_impaired_flow["MONTHLY_DEMAND_AF"]
    )

    monthly_impaired_flow.rename(
        columns={"DATE": "date", "MONTHLY_DEMAND_AF": "demand_af"}, inplace=True
    )

    # Added the impaired flow in cfs units for output
    monthly_impaired_flow["flow_cfs_impaired"] = monthly_impaired_flow[
        "flow_af_impaired"
    ] / (1.983 * monthly_impaired_flow["DAYS_IN_MONTH"])

    monthly_impaired_flow = monthly_impaired_flow.drop(columns=["DAYS_IN_MONTH"])
    monthly_impaired_flow = monthly_impaired_flow[
        [
            "siteID",
            "date",
            "flow_cfs_unimpaired",
            "flow_af_unimpaired",
            "demand_af",
            "flow_af_impaired",
            "flow_cfs_impaired",
        ]
    ]
    # Fill NaN with 0
    monthly_impaired_flow = monthly_impaired_flow.fillna(0)

    print("MONTHLY_IMPAIRED_FLOW DATA:")
    print(monthly_impaired_flow.head())

    # Sort by siteID and date, then format the date as desired; finally write to CSV
    monthly_impaired_flow = monthly_impaired_flow.set_index(
        ["siteID", "date"]
    ).sort_index()
    df_reset = monthly_impaired_flow.reset_index()
    df_reset["date"] = pd.to_datetime(df_reset["date"])
    df_reset["date"] = df_reset["date"].dt.strftime(output_dateformat)
    df_reset.to_csv(monthly_filepath, index=False)


def main():
    for dataset in input_config["datasets"]:
        print(f"Working on dataset {dataset['output_directory']}")
        per_dataset(dataset)


if __name__ == "__main__":
    main()
