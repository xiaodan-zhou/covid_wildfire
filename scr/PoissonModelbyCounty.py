import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os


# os.chdir('..')
dir = os.getcwd()
df = pd.read_csv(dir + "/data/moddat2.csv")

# {0: "Mo", 1: "Tu", 2: "We", 3: "Th", 4: "Fr", 5: "Sa", 6: "Su"}
# Index(['Unnamed: 0', 'countyFIPS', 'aerosols', 'pm25', 'State', 'deaths',
#        'cases', 'death_rate', 'case_rate', 'daily_cases', 'daily_deaths',
#        'daily_case_rate', 'daily_death_rate', 'date_shifted_100case',
#        'date_shifted_10kth_rate', 'dayofweek', 'dayofweek_str'],
#       dtype='object')

df['date'] = pd.to_datetime(df['date'])
df['countyFIPS'] = df['countyFIPS'].astype(str)
df.sort_values("date", inplace=True)
df.reset_index()
df.dropna(inplace=True)
df = df[df.daily_cases >= 0] ## todo: check this

# daycount = np.asarray(df.groupby("countyFIPS").agg({"date_shifted_1case": "count"})).ravel()
# plt.hist(daycount)

nparam = 13
name_param = ["Intercept",
              "C(dayofweek)[T.1]", "C(dayofweek)[T.2]",
              "C(dayofweek)[T.3]", "C(dayofweek)[T.4]",
              "C(dayofweek)[T.5]", "C(dayofweek)[T.6]",
              "bs(date_shifted_100case, 5)[0]", "bs(date_shifted_100case, 5)[1]",
              "bs(date_shifted_100case, 5)[2]", "bs(date_shifted_100case, 5)[3]",
              "bs(date_shifted_100case, 5)[4]",
              "pm25"]
coefs = {}
for ii in range(nparam):
    coefs[ii] = []

model_str1 = "daily_cases ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek)"

for fips in df['countyFIPS'].unique():
    df_subset = df[df['countyFIPS']==fips]
    try:
        model = sm.GLM.from_formula(model_str1, data=df_subset, family=sm.families.Poisson())
        result = model.fit()
        for ii in range(nparam):
            coefs[ii].append([result.params[ii]] + list(result.conf_int().iloc[ii, :]))
    except:
        print(fips)
        # 19177
        # 42091
        # 31157
        # 31079


for ii in range(nparam):
    print(np.asarray(coefs[ii]).shape)






for ii in range(nparam):
    one_coef = np.asarray(coefs[ii])
    one_coef = pd.DataFrame({"coef": one_coef[:, 0],
                              "low": one_coef[:, 1],
                              "high": one_coef[:, 2]})
    one_coef.sort_values("coef", ascending=False, inplace=True)
    one_coef.to_csv(dir + "/results/PoissonModelbyCounty_" + name_param[ii] + ".csv")




pdf = PdfPages(dir + "/results/PoissonModelbyCounty2.pdf")

for ii in range(nparam):
    one_coef = pd.read_csv(dir + "/results/PoissonModelbyCounty_" + name_param[ii] + ".csv")

    ymin = min(np.percentile(one_coef["low"], 5), np.percentile(one_coef["high"], 5))
    ymax = max(np.percentile(one_coef["low"], 95), np.percentile(one_coef["high"], 95))

    plt.figure(figsize=(20, 8))
    plt.plot(np.arange(one_coef.shape[0]), one_coef["coef"], label="coef")
    plt.plot(np.arange(one_coef.shape[0]), one_coef["low"], label="low", alpha=.5)
    plt.plot(np.arange(one_coef.shape[0]), one_coef["high"], label="high", alpha=.5)
    plt.legend()
    # plt.ylim((-.25, .25))
    plt.ylim((ymin, ymax))
    plt.hlines(xmin=0, xmax=one_coef.shape[0], y=0)
    plt.xlabel("counties")
    plt.ylabel("coef")
    plt.title(name_param[ii] + " coefficients by county (poisson model)")
    pdf.savefig()

pdf.close()