import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os
import random

################################ load data #################################
from scr.moddat2 import df as df
# os.chdir('..')
dir = os.getcwd()



################################ build model #################################
## model v0
variateY = "daily_cases"
n_bs = 5
bs_str = "bs(date_shifted_100case, "+str(n_bs)+")"
output_str = "GlobalPoissonModel"

## model v1
# variateY = "daily_deaths"
# n_bs = 5
# bs_str = "bs(date_shifted_100case, "+str(n_bs)+")"
# output_str = "GlobalPoissonModel_v1"


## model v2
# variateY = "daily_cases"
# n_bs = 8
# bs_str = "bs(date_shifted_100case, "+str(n_bs)+")"
# output_str = "GlobalPoissonModel_v2"

model_str1 = variateY + " ~ " + bs_str + " + pm25 + C(dayofweek) + C(countyFIPS)"
model = sm.GLM.from_formula(model_str1, data=df, family=sm.families.Poisson())
result = model.fit()
pred = result.predict(exog=df)
result.save(dir+"/results/"+output_str+".pickle")
# result = sm.load(dir+"/results/"+output_str+".pickle")



################################ save model results #################################
file1 = open(dir+"/results/"+output_str+".txt", "w+")
file1.readline()
file1.write(result.summary().as_text())
file1.close()


################################ visualize model results #################################
pdf = PdfPages(dir + "/results/"+output_str+".pdf")
df.reset_index(inplace=True)

p = 25*7
dp = df.iloc[0:p, :].copy()
dp.date_shifted_100case = np.linspace(-44, 130, 175)
dp.dayofweek = np.tile(np.arange(7), 25)
dp.countyFIPS = "9009"

plt.clf()
for pm in [2, 4, 6, 8, 10, 12]:
    dp.pm25 = pm
    pred = result.predict(exog=dp)
    plt.plot(dp['date_shifted_100case'], pred, label=pm)
plt.legend()
plt.xlabel("date_shifted_100case")
plt.ylabel("daily case count")
plt.title("Covid daily case in FIPS " + dp.countyFIPS[0] + " given PM2.5")
pdf.savefig()


## plot the prediction
plt.clf()
for fips in df['countyFIPS'].unique():
    pred = result.predict(exog=df[df['countyFIPS']==fips])
    plt.plot(df.index[df['countyFIPS']==fips], pred, alpha=.5)
plt.legend()
plt.xlabel("date")
plt.ylabel("daily case count")
plt.title("predicted daily case by county")
pdf.savefig()

pdf.close()



####################################### visualize prediction of sampled couties #################################
# result = sm.load(dir+"/results/"+output_str+".pickle")
pdf = PdfPages(dir + "/results/"+output_str+"_prediction.pdf")

random.seed(0)
FIPS = df['countyFIPS'].unique()
sampleFIPS = [random.randint(0, len(FIPS)) for i in range(20)]

for fips in FIPS[sampleFIPS]:
    df_subset = df[df['countyFIPS']==fips]
    pred_table = None
    try:
        pred = result.get_prediction(df_subset)
        pred_table = pred.summary_frame(alpha=0.05)
    except:
        print(fips)

    if pred_table is not None:
        plt.figure(figsize=(20, 8))
        xx = df_subset.date_shifted_100case
        plt.scatter(xx, df_subset[variateY], label="raw", color='orange')
        plt.plot(xx, pred_table["mean"], label="pred", color="blue")
        plt.plot(xx, pred_table["mean_ci_lower"], label="low", alpha=.5, color='red', linestyle="--")
        plt.plot(xx, pred_table["mean_ci_upper"], label="high", alpha=.5, color='red', linestyle="--")
        plt.legend()
        plt.xlabel("time")
        plt.ylabel("predicted")
        plt.title(fips)
        pdf.savefig()
pdf.close()