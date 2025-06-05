#!/usr/bin/env python
# coding: utf-8

# In[9]:


import pandas as pd
from pathlib import Path
from os import makedirs

INPUTS_DIR = Path("inputs")
DELINEATION_DIR = INPUTS_DIR / "LOI_delineations" / "LOI_delineations"
PROCESSED_INPUTS_DIR = Path("processed_inputs")
PROCESSED_DELINEATIONS_DIR = PROCESSED_INPUTS_DIR / "delineations"

for dir in [
    INPUTS_DIR,
    DELINEATION_DIR,
    PROCESSED_INPUTS_DIR,
    PROCESSED_DELINEATIONS_DIR,
]:
    makedirs(dir, exist_ok=True)

flow_columns = ['date', 'unique_ID', 'flow_cfs']


# In[10]:


nc_sites_final = pd.read_csv(INPUTS_DIR / "NC_sites_FINAL_v2_unimpaired_flow.csv")
print(nc_sites_final.columns)
nc_sites_final.head()


# ## Write to processed_inputs

# In[11]:


nc_sites_final.to_csv(Path("processed_inputs/NC_sites_FINAL_v2_unimpaired_flow.csv"))


# ## Process and rewrite delineations

# In[12]:


nc_sites_final_shp = gp.read_file(DELINEATION_DIR / 'NC_sites_FINAL_watersheds_v2.shp')
nc_sites_final_shp.to_file(PROCESSED_DELINEATIONS_DIR / 'NC_sites_FINAL_watersheds_v2.shp')


# # Biosites

# In[13]:


biosites = pd.read_csv(INPUTS_DIR / "N_Coast_BioSites_Unimpaired_Flow.csv")
print(biosites.columns)


# ## Notes
# Missing `Source_File` column. Extra column **Unnamed: 0**.
# 
# ## Actions
# - Find out from Kris if the Source_File column should be added and filled
# - Remove extra column

# In[14]:


# Drop "Unnamed: 0" column / keep all other columns
biosites = biosites[flow_columns]


# In[15]:


biosites.head()


# ## Write processed input file
# 
# ...to `processed_inputs` directory.

# In[16]:


biosites.to_csv(PROCESSED_INPUTS_DIR / "N_Coast_BioSites_Unimpaired_Flow.csv")


# # Gages

# In[17]:


gages_unimpared = pd.read_csv(INPUTS_DIR / "All_gages_unimpaired_flow.csv")
gages_unimpared = gages_unimpared.rename({"model_ID": "unique_ID"}, axis=1)
gages_unimpared["date"] = pd.to_datetime(gages_unimpared["date"], format=r"%m/%d/%Y")
print(gages_unimpared.dtypes)
gages_unimpared = gages_unimpared[flow_columns]
gages_unimpared.head()


# In[18]:


gages_unimpared.to_csv(PROCESSED_INPUTS_DIR / "All_gages_unimpaired_flow.csv")


# # McBain

# Shapefile: Rename Site as siteID and 
# Flow: Convert date column appropriately

# In[19]:


import geopandas as gp
import pandas as pd


mcbain_shp = gp.read_file(DELINEATION_DIR / "McBain_sites_snapped_NAD83_watersheds.shp")
print(mcbain_shp.columns)
mcbain_shp = mcbain_shp.rename(columns={"Site": "siteID"})

mcbain_unimpaired = pd.read_csv(INPUTS_DIR / "McBain_Sites_Unimpaired_Flow.csv")
mcbain_unimpaired["date"] = pd.to_datetime(
    mcbain_unimpaired["date"], format=r"%m/%d/%Y"
)
mcbain_unimpaired = mcbain_unimpaired[flow_columns]
mcbain_unimpaired.head()


# In[20]:


mcbain_unimpaired.to_csv(PROCESSED_INPUTS_DIR / "McBain_Sites_Unimpaired_Flow.csv")

mcbain_shp.to_file(
    PROCESSED_DELINEATIONS_DIR / "McBain_sites_snapped_NAD83_watersheds.shp"
)


# # SFE sites high resolution

# In[21]:


import geopandas as gp
import pandas as pd
from datetime import datetime, timedelta

highresolution_shp = gp.read_file(
    DELINEATION_DIR / "SFE_sites_highresolution_watersheds.shp"
)

highresolution_unimpaired = pd.read_csv(
    INPUTS_DIR / "SFE_sites_highresolution_Unimpaired_Flow.csv"
)


def excel_serial_to_date(x):
    return datetime(1899, 12, 30) + timedelta(days=x)


highresolution_unimpaired["date"] = highresolution_unimpaired["date"].apply(
    excel_serial_to_date
)
highresolution_unimpaired = highresolution_unimpaired[flow_columns]


highresolution_unimpaired.to_csv(
    PROCESSED_INPUTS_DIR / "SFE_sites_highresolution_Unimpaired_Flow.csv"
)


# # SFE sites mainstem

# In[22]:


import geopandas as gp
import pandas as pd

mainstem_shp = gp.read_file(DELINEATION_DIR / "SFE_sites_mainstem_watersheds.shp")
print(mainstem_shp.columns)
mainstem_shp = mainstem_shp.rename(columns={"Site": "siteID"})

mainstem_shp.to_file(PROCESSED_DELINEATIONS_DIR / "SFE_sites_mainstem_watersheds.shp")


mainstem_unimpaired = pd.read_csv(INPUTS_DIR / "SFE_sites_mainstem_Unimpaired_Flow.csv")
mainstem_unimpaired["date"] = pd.to_datetime(
    mainstem_unimpaired["date"], format=r"%m/%d/%Y"
)

mainstem_unimpaired = mainstem_unimpaired[flow_columns]

mainstem_unimpaired.to_csv(
    PROCESSED_INPUTS_DIR / "SFE_sites_mainstem_Unimpaired_Flow.csv"
)

