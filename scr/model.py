import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os

dir = os.getcwd()
df = pd.read_csv(dir + "/data/moddat2.csv")

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
df = df[df.daily_cases >= 0]


## model with time shift
model_str1 = "case_rate ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek) + C(countyFIPS)"
model_gaussian = sm.GLM.from_formula(model_str1, data=df, family=sm.families.Gaussian())
result_gaussian = model_gaussian.fit()
print(result_gaussian.summary())
# result.save(dir+"/results/str1_Gaussian.pickle")
# result = sm.load(dir+"/results/str1_Gaussian.pickle")

pdf = PdfPages(dir + "/PoissonModel_bs(date_shifted_100case,5).pdf")

df.reset_index(inplace=True)
df.set_index("date_shifted_100case", inplace=True)
## plot the prediction
pred = result_gaussian.predict(exog=df)
plt.clf()
for fips in df['countyFIPS'].unique():
    pred = result_gaussian.predict(exog=df[df['countyFIPS']==fips])
    plt.plot(df.index[df['countyFIPS']==fips], pred, alpha=.3)
plt.legend()
pdf.savefig()

pdf.close()

## Plot the fitted means curves for two variables, holding the others fixed.
p = 251
dp = df.iloc[0:p, :].copy()
dp.date_shifted = np.linspace(-100, 150, 251)
dp.dayofweek = 1
dp.countyFIPS = "51780"

plt.clf()
for pm in [2, 4, 6, 8, 10, 12]:
    dp.pm25 = pm
    pred = result.predict(exog=dp)
    plt.plot(dp['date_shifted'], pred, label=pm)
plt.legend()
pdf.savefig()

dp.pm = 10
plt.clf()
for day in range(7):
    dp.dayofweek = day
    pred = result_gaussian.predict(exog=dp)
    plt.plot(dp['date_shifted'], pred, label=day)
plt.legend()
pdf.savefig()



pdf.close()


# print(df.daily_cases.quantile([.0, .25, .5, .75, 1.]))
# print(df.date_shifted.quantile([.0, .25, .5, .75, 1.]))
# print(df.pm25.quantile([.0, .25, .5, .75, 1.]))
# print(df.dayofweek.quantile([.0, .25, .5, .75, 1.]))
