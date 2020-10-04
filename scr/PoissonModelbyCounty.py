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

####################################### model setup #################################
variateY = "daily_cases"

## v0
n_bs = 5
output_str = "PoissonModelbyCounty"

## v2
# n_bs = 8
# output_str = "PoissonModelbyCounty_v2"


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



####################################### run model #################################
coefs = {}
for ii in range(nparam):
    coefs[ii] = []

FIPS = []
States = []

for fips in df['countyFIPS'].unique():
    df_subset = df[df['countyFIPS']==fips]
    try:
        model = sm.GLM.from_formula(model_str1, data=df_subset, family=sm.families.Poisson())
        result = model.fit()

        for ii in range(nparam):
            coefs[ii].append([result.params[ii]] + list(result.conf_int().iloc[ii, :]))

        FIPS.append(fips)
        States.append(df_subset.State.iloc[0])
    except:
        print(fips)


####################################### save model coefficients #################################
for ii in range(nparam):
    print(np.asarray(coefs[ii]).shape)

for ii in range(nparam):
    one_coef = np.asarray(coefs[ii])
    one_coef = pd.DataFrame({"coef": one_coef[:, 0],
                              "low": one_coef[:, 1],
                              "high": one_coef[:, 2],
                              "FIPS": FIPS,
                              "State": States})
    one_coef.sort_values("coef", ascending=False, inplace=True)
    one_coef.to_csv(dir + "/results/"+output_str+"_" + name_param[ii] + ".csv")



####################################### visualized the coefficients #################################
pdf = PdfPages(dir + "/results/"+output_str+".pdf")

for ii in range(nparam):
    one_coef = pd.read_csv(dir + "/results/"+output_str+"_" + name_param[ii] + ".csv")
    one_coef.sort_values(["coef"], ascending=False, inplace=True)
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






####################################### visualize prediction of sampled counties #################################
pdf = PdfPages(dir + "/results/"+output_str+"_prediction.pdf")

random.seed(0)
FIPS = df['countyFIPS'].unique()
sampleFIPS = [random.randint(0, len(FIPS)) for i in range(20)]

for fips in FIPS[sampleFIPS]:
    df_subset = df[df['countyFIPS']==fips]
    result= None
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





####################################### visualized the coefficients #################################
pdf = PdfPages(dir + "/results/"+output_str+".pdf")

for ii in range(nparam):
    one_coef = pd.read_csv(dir + "/results/"+output_str+"_" + name_param[ii] + ".csv")
    one_coef.sort_values(["coef"], ascending=False, inplace=True)
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




####################################### summarize the coef for PM2.5 by state #################################
def percentile(n):
    def percentile_(x):
        return np.percentile(x, n)
    percentile_.__name__ = 'percentile_%s' % n
    return percentile_

ii = nparam - 1
one_coef = pd.read_csv(dir + "/results/"+output_str+"_" + name_param[ii] + ".csv")
coef_summary = one_coef.groupby(["State"]).aggregate({"coef": [percentile(5), np.mean, percentile(95)]})
coef_summary.to_string()

file1 = open(dir+"/results/"+output_str+"_PMcoefbyState.txt", "w+")
file1.readline()
file1.write(coef_summary.to_string())
file1.close()



####################################### visualize prediction of sampled couties with fake PM #################################
pdf = PdfPages(dir + "/results/"+output_str+"_prediction_fake2PM.pdf")

random.seed(0)
FIPS = df['countyFIPS'].unique()
sampleFIPS = '36059' # [random.randint(0, len(FIPS)) for i in range(20)]
fips = sampleFIPS
PMs = [2, 12]

for fips in FIPS[sampleFIPS]:
    df_subset = df[df['countyFIPS']==fips]
    result= None
    try:
        model = sm.GLM.from_formula(model_str1, data=df_subset, family=sm.families.Poisson())
        result = model.fit()
    except:
        print(fips)

    if result is not None:
        plt.figure(figsize=(20, 8))
        xx = df_subset.date_shifted_100case
        plt.scatter(xx, df_subset[variateY], label="raw", color='black', s=2)

        df_subset = df_subset.copy()
        xx = df_subset.date_shifted_100case
        plt.scatter(xx, df_subset[variateY], label="raw", color='black', s=2)

        for ii in PMs:
            df_subset.pm25 = ii
            pred = result.get_prediction(df_subset)
            pred_table = pred.summary_frame(alpha=0.05)
            plt.plot(xx, pred_table["mean"], label="PM" + str(ii))
            plt.plot(xx, pred_table["mean_ci_lower"], alpha=.5, color='red', linestyle="--")
            plt.plot(xx, pred_table["mean_ci_upper"], alpha=.5, color='red', linestyle="--")
            plt.legend()
            plt.xlabel("time")
            plt.ylabel("predicted")
            plt.title(fips)

        pdf.savefig()
pdf.close()
