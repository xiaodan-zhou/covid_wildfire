import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os


################################ load data #################################
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
df = df[df.daily_cases >= 0]



################################ build model #################################
## model v0
model_str1 = "daily_cases ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek) + C(countyFIPS)"
model = sm.GLM.from_formula(model_str1, data=df, family=sm.families.Poisson())
result = model.fit()
pred = result.predict(exog=df)
result.save(dir+"/results/GlobalPoissonModel.pickle")
# result = sm.load(dir+"/results/GlobalPoissonModel.pickle")

## model v1
# model_str1 = "daily_deaths ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek) + C(countyFIPS)"
# model = sm.GLM.from_formula(model_str1, data=df, family=sm.families.Poisson())
# result = model.fit()
# pred = result.predict(exog=df)
# result.save(dir+"/results/GlobalPoissonModel_v1.pickle")


################################ save model results #################################
file1 = open(dir+"/results/GlobalPoissonModel.txt", "w+")
file1.readline()
file1.write(result.summary().as_text())
file1.close()
# print(result.summary())


pdf = PdfPages(dir + "/results/GlobalPoissonModel.pdf")

df.reset_index(inplace=True)
# df.set_index("date_shifted_100case", inplace=True)


################################ visualize model results #################################
p = 21*7
dp = df.iloc[0:p, :].copy()
dp.date_shifted_100case = np.linspace(-30, 116, 147)
dp.dayofweek = np.tile(np.arange(7), 21)
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
