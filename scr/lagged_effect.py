import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from scipy.stats.stats import pearsonr
from scipy.signal import correlate

def cor_lag(l1, l2, maxlag=12):
    if len(l1) != len(l2):
        return -1
    if len(l1) < maxlag or len(l2) < maxlag:
        return -1
    lcor = [pearsonr(l1, l2)[0]]
    for ii in range(1, maxlag):
        lcor.append(pearsonr(l1[ii:], l2[:-ii])[0])
    return lcor

dir = "/Users/mac/Desktop/PM2.5/wildfire"

df = pd.read_csv(dir + "/data/moddat2.csv", sep=",")
df.sort_values(["countyFIPS", "date"], inplace=True)

days = df['date'].unique()
pdf = PdfPages(dir + "/lagged_effect.pdf")

## assumes no missing data
cor_value = []
maxlag = 24

plt.figure(figsize=(10,8))
for id, dy in df.groupby("countyFIPS"):
    # dy = df[df['countyFIPS'] == '21195']
    temp = cor_lag(dy['daily_cases'], dy['pm25'], maxlag=maxlag)
    plt.plot(range(maxlag), temp)
    cor_value.append(temp)
pdf.savefig()

pdf.close()