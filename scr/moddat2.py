import pandas as pd
import os

dir = os.getcwd()
df = pd.read_csv(dir + "/data/moddat2.csv")

# {0: "Mo", 1: "Tu", 2: "We", 3: "Th", 4: "Fr", 5: "Sa", 6: "Su"}
# 'countyFIPS', 'aerosols', 'pm25', 'State', 'deaths',
#        'cases', 'death_rate', 'case_rate', 'daily_cases', 'daily_deaths',
#        'daily_case_rate', 'daily_death_rate', 'date_shifted_100case',
#        'date_shifted_10kth_rate', 'dayofweek', 'dayofweek_str']

df['date'] = pd.to_datetime(df['date'])
df['countyFIPS'] = df['countyFIPS'].astype(str)
df.sort_values("date", inplace=True)
df.reset_index()
df.dropna(inplace=True)
df = df[df.daily_cases >= 0] ## todo: check this

# daycount = np.asarray(df.groupby("countyFIPS").agg({"date_shifted_1case": "count"})).ravel()
# plt.hist(daycount)