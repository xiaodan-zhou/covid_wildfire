import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os
import random


####################################### import and clean data #################################
from scr.moddat2 import df as df
# os.chdir('..')
dir = os.getcwd()

####################################### model #################################
variateY = "daily_cases"

## v0
# n_bs = 5
# output_str = "PoissonModelbyCounty"

## v2
n_bs = 8
output_str = "PoissonModelbyCounty_v2"


bs_str = "bs(date_shifted_100case, "+str(n_bs)+")"
model_str1 = variateY + " ~ " + bs_str + " + pm25 + C(dayofweek)"

nparam = 8 + n_bs
name_param = ["Intercept",
              "C(dayofweek)[T.1]", "C(dayofweek)[T.2]",
              "C(dayofweek)[T.3]", "C(dayofweek)[T.4]",
              "C(dayofweek)[T.5]", "C(dayofweek)[T.6]"]
for i in range(n_bs):
    name_param.append("bs(date_shifted_100case, " + str(n_bs) + ")[" + str(i) + "]")
name_param.append("pm25")


coefs = {}
for ii in range(nparam):
    coefs[ii] = []


for fips in df['countyFIPS'].unique():
    df_subset = df[df['countyFIPS']==fips]
    try:
        model = sm.GLM.from_formula(model_str1, data=df_subset, family=sm.families.Poisson())
        result = model.fit()
        for ii in range(nparam):
            coefs[ii].append([result.params[ii]] + list(result.conf_int().iloc[ii, :]))
    except:
        print(fips)


####################################### save model coefficients #################################
for ii in range(nparam):
    print(np.asarray(coefs[ii]).shape)

for ii in range(nparam):
    one_coef = np.asarray(coefs[ii])
    one_coef = pd.DataFrame({"coef": one_coef[:, 0],
                              "low": one_coef[:, 1],
                              "high": one_coef[:, 2]})
    one_coef.sort_values("coef", ascending=False, inplace=True)
    one_coef.to_csv(dir + "/results/"+output_str+"_" + name_param[ii] + ".csv")




####################################### visualized the coefficients #################################
pdf = PdfPages(dir + "/results/"+output_str+".pdf")

for ii in range(nparam):
    one_coef = pd.read_csv(dir + "/results/"+output_str+"_" + name_param[ii] + ".csv")

    ymin = min(np.percentile(one_coef["low"], 5), np.percentile(one_coef["high"], 5))
    ymax = max(np.percentile(one_coef["low"], 95), np.percentile(one_coef["high"], 95))

    plt.figure(figsize=(20, 8))
    plt.plot(np.arange(one_coef.shape[0]), one_coef["coef"], label="coef")
    plt.plot(np.arange(one_coef.shape[0]), one_coef["low"], label="low", alpha=.5)
    plt.plot(np.arange(one_coef.shape[0]), one_coef["high"], label="high", alpha=.5)
    plt.legend()
    plt.ylim((ymin, ymax))
    plt.hlines(xmin=0, xmax=one_coef.shape[0], y=0)
    plt.xlabel("counties")
    plt.ylabel("coef")
    plt.title(name_param[ii] + " coefficients by county (poisson model)")
    pdf.savefig()

pdf.close()


### another way to visualize
# one_coef = pd.read_csv(dir + "/results/"+output_str+"_" + name_param[-1] + ".csv")
# plt.figure()
# for ii in range(one_coef.shape[0]):
#     plt.hlines(y=ii, xmin=one_coef["low"][ii], xmax=one_coef["high"][ii], colors='grey')
# plt.scatter(x=one_coef["coef"], y=range(one_coef.shape[0]))
# plt.xlabel("coef of PM")
# plt.ylabel("county")
# plt.xlim(-.20, .20)
# plt.title(name_param[-1] + " coefficients by county (poisson model)")

### hist of coef of PM
# one_coef = pd.read_csv(dir + "/results/"+output_str+"_" + name_param[-1] + ".csv")
# plt.figure()
# plt.hist(one_coef["coef"], bins=500)
# plt.vlines(x=np.mean(one_coef["coef"]), ymin=0, ymax=250, colors='')
# plt.xlim(-.20, .20)
# plt.xlabel("coef of PM")
# plt.ylabel("count of county")
# plt.title(name_param[-1] + " coefficients by county (poisson model)")



####################################### visualize prediction of sampled couties #################################
pdf = PdfPages(dir + "/results/"+output_str+"_prediction.pdf")

random.seed(0)
FIPS = df['countyFIPS'].unique()
sampleFIPS = [random.randint(0, len(FIPS)) for i in range(20)]

for fips in FIPS[sampleFIPS]:
    df_subset = df[df['countyFIPS']==fips]
    pred_table= None
    try:
        model = sm.GLM.from_formula(model_str1, data=df_subset, family=sm.families.Poisson())
        result = model.fit()
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