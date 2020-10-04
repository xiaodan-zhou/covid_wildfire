## Python Version 3.5

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime
from matplotlib.backends.backend_pdf import PdfPages

dir = "/Users/mac/Desktop/PM2.5/wildfire" # os.getcwd()

pdf = PdfPages(dir + "/output/climateVis.pdf")
df = pd.read_csv(dir + "/data/TAVG.csv")


## clean time field
df['date'] = pd.to_datetime(df.date)
df['date0'] = df['date'] - pd.to_datetime(datetime.date(2020, 1, 1))
df['dayofweek'] = df.date.dt.dayofweek
df.loc[:, "dayofweek_str"] = df.dayofweek.replace({0: "Mo", 1: "Tu", 2: "We", 3: "Th", 4: "Fr", 5: "Sa", 6: "Su"})
df['month'] = df.date.dt.month



## daily temperature by state (2020)
df.set_index("date", inplace=True)

plt.figure(figsize=(10,8))
for istate in df.state.unique():
    x = df[df.state == istate].groupby(pd.Grouper(freq="d")).agg({"value": np.mean})
    plt.plot(x.index, x.value, label=istate)
plt.legend()
plt.title("daily temperature by state (2020)", size=20)
plt.xlabel("date")
plt.ylabel("temperature (celcius)")
pdf.savefig()


plt.figure(figsize=(10,8))
for istate in df.state.unique():
    x = df[df.state == istate].groupby(pd.Grouper(freq="d")).agg({"value": np.max})
    plt.plot(x.index, x.value, label=istate)
plt.legend()
plt.title("maximum of daily temperature by state (2020)", size=20)
plt.xlabel("date")
plt.ylabel("temperature (celcius)")
pdf.savefig()


plt.figure(figsize=(10,8))
for istate in df.state.unique():
    x = df[df.state == istate].groupby(pd.Grouper(freq="d")).agg({"value": np.min})
    plt.plot(x.index, x.value, label=istate)
plt.legend()
plt.title("minimum of daily temperature by state (2020)", size=20)
plt.xlabel("date")
plt.ylabel("temperature (celcius)")
pdf.savefig()


plt.figure(figsize=(10,8))
plt.scatter(df.long, df.lat)
plt.title("stations (WA, OR, CA)", size=20)
plt.xlabel("LONG")
plt.ylabel("LAT")
pdf.savefig()


pdf.close()

# import plotly.express as px
#
# fig = px.scatter_mapbox(df, lat="lat", lon="long", hover_name="FIPS", color_discrete_sequence=["fuchsia"], zoom=3, height=300)
# fig.update_layout(mapbox_style="open-street-map")
# fig.update_layout(margin={"r":0,"t":0,"l":0,"b":0})
# fig.show()