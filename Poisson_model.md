### Global Poisson Model
#### script [script](/scr/GlobalPoissonModel.py)

v0
```
daily_cases ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek) + C(countyFIPS)
==================================================================================================
                                     coef    std err          z      P>|z|      [0.025      0.975]
--------------------------------------------------------------------------------------------------
pm25                               0.0051   6.37e-05     80.256      0.000       0.005       0.005
==================================================================================================
```

v1
```
daily_deaths ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek) + C(countyFIPS)
==================================================================================================
                                     coef    std err          z      P>|z|      [0.025      0.975]
--------------------------------------------------------------------------------------------------
pm25                               0.0031      0.001      5.356      0.000       0.002       0.004
==================================================================================================
```

v2
```
daily_cases ~ bs(date_shifted_100case, 8) + pm25 + C(dayofweek) + C(countyFIPS)
==================================================================================================
                                     coef    std err          z      P>|z|      [0.025      0.975]
--------------------------------------------------------------------------------------------------
pm25                               0.0049   6.38e-05     77.225      0.000       0.005       0.005
==================================================================================================
```

### Poisson Model by county
#### script [script](/scr/PoissonModelbyCounty.py)

v0 (more coefficient visualization [here](/results/PoissonModelbyCounty.pdf))
```
daily_cases ~ bs(date_shifted_100case, 5) + pm25 + C(dayofweek)
```
![](/images/image2.png)


v2 (more coefficient visualization [here](/results/PoissonModelbyCounty_v2.pdf))
```
daily_cases ~ bs(date_shifted_100case, 8) + pm25 + C(dayofweek)
```
![](/images/image3.png)
