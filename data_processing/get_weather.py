import ee
import datetime
import pandas as pd

ee.Initialize()

#Bounding box of California, Oregon, and Washington
bb = ee.Geometry.Polygon(
        [[[-125.87516701227898, 48.522490247276735],
          [-125.94736706059616, 31.9769810437932],
          [-115.09287487309616, 32.423213222563135],
          [-114.82920299809616, 34.40385323496726],
          [-120.14658581059616, 38.879597440110174],
          [-120.27842174809616, 42.165350355649885],
          [-117.42197643559616, 42.214190590619076],
          [-117.29014049809616, 48.866442645680806],
          [-124.45322643559616, 48.90978578645499]]])

#Get counties
cty = ee.FeatureCollection("TIGER/2018/Counties").filterBounds(bb)

#Utility Function
def getKeys(x, keys):
    #x is list of dict
    new = []
    for i in x:
        new.append(dict((k, i['properties'][k]) for k in keys))
    return(new)

# Get county populations
pop = ee.Image("CIESIN/GPWv411/GPW_Population_Count/gpw_v4_population_count_rev11_2020_30_sec")
res = pop.reduceRegions(cty, ee.Reducer.sum()).getInfo()
popres = pd.DataFrame(getKeys(res['features'], ['GEOID', 'sum']))

# Get daily population-weighted variables
base = datetime.datetime.strptime('2020-12-16', '%Y-%m-%d')
date_list = [base - datetime.timedelta(days=x) for x in range(277)] 

allres = pd.DataFrame({})
for date in date_list:
    start = datetime.datetime.strftime(date, '%Y%m%d')
    grd = ee.Image("IDAHO_EPSCOR/GRIDMET/" + start)
    
    res = pop.multiply(grd).reduceRegions(cty, ee.Reducer.sum()).getInfo()
    grdres = getKeys(res['features'], ['GEOID', 'tmmx', 'rmax', 'sph'])
    
    #Add pop and divide by total pop
    resdf = pd.DataFrame(grdres).merge(popres, on='GEOID')
    resdf['tmmx'] = resdf['tmmx']/resdf['sum']
    resdf['rmax'] = resdf['rmax']/resdf['sum']
    resdf['sph'] = resdf['sph']/resdf['sum']
    resdf = resdf.drop('sum', axis=1) #delete pop
    resdf['date'] = datetime.datetime.strftime(date, '%Y-%m-%d')
    
    allres = pd.concat([allres, resdf])
    print(start)

allres['tmmx'] = allres['tmmx'] - 273.15
allres.to_csv('~/covid_wildfire/data/new_weather.csv', index=False)

