#!/usr/bin/env python
# coding: utf-8

# Create summaries for datasets
# ===
# 
# We are interested in seeing the proportion of negative flows currently produced by the scripts.

# In[22]:


import pprint
import yaml
from pathlib import Path
import pandas as pd
import os

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


# In[23]:


OVERALL_DIR = Path('outputs') / 'OVERALL'
os.makedirs(OVERALL_DIR, exist_ok=True)

def summarize(data):
    total_days = data['flow_cfs_impaired'].groupby(['dataset', 'siteID']).count()
    days_with_negative_flow = data[data['flow_cfs_impaired'] < 0]['flow_cfs_impaired'].groupby(['dataset', 'siteID']).count()
    total = pd.DataFrame({'total_days': total_days})
    negative = pd.DataFrame({'negative_days': days_with_negative_flow})
    overall = negative.join(total, how="right").fillna(0)
    numerator = overall['negative_days']
    denominator = overall['total_days']
    if (numerator == 0).any() and (denominator == 0).any():
        percentage = 0.0
    else:
        percentage = (100 * numerator / denominator).fillna(0).round(2)
    overall['percentage_negative'] = percentage
    return overall

gather_daily = []
gather_monthly = []
for dataset in input_config['datasets']:
    output_path = Path(dataset['output_directory'])
    dataset_name = os.path.basename(output_path)
    print("Working on", dataset_name)
    print()


    daily_data = pd.read_csv(output_path / "daily.csv")
    daily_data['dataset'] = dataset_name
    daily_data = daily_data.set_index(['dataset', 'siteID', 'date'])
    gather_daily.append(daily_data)
    summary = summarize(daily_data)
    print("Daily (head)\n", summary.head())
    summary.to_csv(output_path / 'daily_summary.csv')
    
    monthly_data = pd.read_csv(output_path / "monthly.csv")
    monthly_data['dataset'] = dataset_name
    monthly_data = monthly_data.set_index(['dataset', 'date', 'siteID'])
    gather_monthly.append(monthly_data)
    summary = summarize(monthly_data)
    print("Monthly (head)\n", summary.head())
    summary.to_csv(output_path / 'monthly_summary.csv')

    print()


# In[24]:


overall_daily = pd.concat(gather_daily)
overall_daily.to_csv(OVERALL_DIR / 'daily_overall.csv')

overall_monthly = pd.concat(gather_monthly)
overall_monthly.to_csv(OVERALL_DIR / 'monthly_overall.csv')


# In[25]:


overall_daily.reset_index(level='date', drop=True).groupby(['dataset', 'siteID']).count().shape


# In[26]:


overall_monthly.reset_index(level='date', drop=True).groupby(['dataset', 'siteID']).count().shape


# In[27]:


overall_daily_summary = summarize(overall_daily)
print(overall_daily_summary.head())
overall_daily_summary.to_csv(OVERALL_DIR / 'daily_summary_overall.csv')

overall_monthly_summary = summarize(overall_monthly)
print(overall_monthly_summary.head())
overall_monthly_summary.to_csv(OVERALL_DIR / 'monthly_summary_overall.csv')

