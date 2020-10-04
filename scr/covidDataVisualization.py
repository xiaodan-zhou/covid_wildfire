import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os

# dir = "/Users/mac/Desktop/PM2.5/wildfire"
dir = os.getcwd()
df = pd.read_csv(dir + "/data/moddat2.csv", sep=",")
df['date'] = pd.to_datetime(df.date)

# Index(['Unnamed: 0', 'countyFIPS', 'aerosols', 'pm25', 'State', 'deaths',
#        'cases', 'death_rate', 'case_rate', 'daily_cases', 'daily_deaths',
#        'daily_case_rate', 'daily_death_rate', 'date_shifted_100case',
#        'date_shifted_10kth_rate', 'dayofweek', 'dayofweek_str'],
#       dtype='object')

def TimeSeriesVisByCounty(df, var, title="", xlab="", ylab=""):
    plt.figure(figsize=(20, 8))
    for ifips in df.countyFIPS.unique():
        x = df[df.countyFIPS == ifips]
        plt.plot(x.index, x[var], c='grey', alpha=.3)
    plt.title(title, size=20)
    plt.xlabel(xlab)
    plt.ylabel(ylab)


df.set_index("date", inplace=True)

pdf = PdfPages(dir + "/output/covid_cumulative_count.pdf")
TimeSeriesVisByCounty(df, var="cases", title="cumulative cases by FIPS", xlab="date", ylab="case count")
pdf.savefig()
TimeSeriesVisByCounty(df, var="deaths", title="cumulative deaths by FIPS", xlab="date", ylab="death count")
pdf.savefig()
pdf.close()

pdf = PdfPages(dir + "/output/covid_cumulative_rate.pdf")
TimeSeriesVisByCounty(df, var="case_rate", title="case rate by FIPS", xlab="date", ylab="case rate")
plt.ylim((0, 0.08))
pdf.savefig()
TimeSeriesVisByCounty(df, var="death_rate", title="death rate by FIPS", xlab="date", ylab="death rate")
plt.ylim((0, 0.003))
pdf.savefig()
pdf.close()





df.reset_index(inplace=True)
df.set_index("date_shifted_100case", inplace=True)

pdf = PdfPages(dir + "/output/covid_cumulative_rate_shifted_by_case100.pdf")
TimeSeriesVisByCounty(df, var="case_rate", title="case rate by FIPS", xlab="date", ylab="case rate")
plt.ylim((0, 0.08))
pdf.savefig()
TimeSeriesVisByCounty(df, var="death_rate", title="death rate by FIPS", xlab="date", ylab="death rate")
plt.ylim((0, 0.003))
pdf.savefig()
pdf.close()





df.reset_index(inplace=True)
df.set_index("date_shifted_10kth_rate", inplace=True)

pdf = PdfPages(dir + "/output/covid_cumulative_rate_shifted_by_10kth_rate.pdf")
TimeSeriesVisByCounty(df, var="case_rate", title="case rate by FIPS", xlab="date", ylab="case rate")
plt.ylim((0, 0.08))
plt.xlim((-10, 200))
pdf.savefig()
TimeSeriesVisByCounty(df, var="death_rate", title="death rate by FIPS", xlab="date", ylab="death rate")
plt.ylim((0, 0.003))
plt.xlim((-10, 200))
pdf.savefig()
pdf.close()




df.reset_index(inplace=True)
df.set_index("date_shifted_1case", inplace=True)

pdf = PdfPages(dir + "/output/covid_cumulative_rate_shifted_by_case1.pdf")
TimeSeriesVisByCounty(df, var="case_rate", title="case rate by FIPS", xlab="date", ylab="case rate")
plt.ylim((0, 0.08))
plt.xlim((-10, 200))
pdf.savefig()
TimeSeriesVisByCounty(df, var="death_rate", title="death rate by FIPS", xlab="date", ylab="death rate")
plt.ylim((0, 0.003))
plt.xlim((-10, 200))
pdf.savefig()
pdf.close()






df.reset_index(inplace=True)
df.sort_values(["countyFIPS", "date"], inplace=True)

pdf = PdfPages(dir + "/output/covid_shifted_days_histogram.pdf")

plt.figure(figsize=(10,8))
hist_value = df["date"][df['date_shifted_100case'] == 0].value_counts()
hist_value.sort_index(inplace=True)
hist_value.plot(kind='bar')
plt.xlabel("date")
plt.ylabel("count")
plt.title("date when the county reach 100 covid cases")
pdf.savefig()

plt.figure(figsize=(10,8))
hist_value = df["date"][df['date_shifted_10kth_rate'] == 0].value_counts()
hist_value.sort_index(inplace=True)
hist_value.plot(kind='bar')
plt.xlabel("date")
plt.ylabel("count")
plt.title("date when the county reach 0.0001 case rates")
pdf.savefig()

pdf.close()






## do this on daily cases
# from statsmodels.graphics.tsaplots import plot_acf
# for ifips in random.sample(df.countyFIPS.unique(), 5):
#     plt.figure(figsize=(10, 8))
#     plot_acf(df[df.countyFIPS == icounty].cases)
#     plt.title("covid cumulative case autocorrelation at FIPS" + str(ifips), size=20)
#     plt.xlabel("date")
#     plt.ylabel("case rate")
#     pdf.savefig()
