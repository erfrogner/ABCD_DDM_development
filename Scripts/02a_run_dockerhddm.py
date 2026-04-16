#!/usr/bin/env python
# coding: utf-8

# HDDM on 4 TPs across TPs
# ERF 

import os
import pandas as pd
import hddm
import numpy as np
import sys
import kabuki
import arviz as az
from kabuki.analyze import gelman_rubin

i = np.int(sys.argv[1])
print('sample#')
print(i)

# Read data
dataname = '.../abcd_nback_grouped_data/nback_xtps_group_drop_6_1_%s.csv'%(i) 
data = pd.read_csv(dataname)

print('RUNNING HDDM')

# Run HDDM
m = hddm.HDDM(data,depends_on={'a':'BlockType','v':['BlockType','TargetType'],'z':'BlockType'},
              include=['a', 't', 'v', 'z','st'],p_outlier=0.05)

m_infdata = m.sample(2500, burn = 1500, chains = 4, 
                     loglike = True, ppc = False, 
                     return_infdata = True, save_name = '.../models/hddm_xtps_model_group_drop_6_1_'+str(i)) 

### CONVERGENCE AND STATS
summ = az.summary(m_infdata, round_to = 4)
summ.to_csv('.../stats/stats_drop_6_1_group_%s.csv'%(i))

### MODEL FIT (WAIC AND DIC)
waic_fit = az.waic(m_infdata, scale = "deviance")
dic_fit = m.dic
fit_dict = {
    "waic": waic_fit.elpd_waic,
    "waic_se": waic_fit.se,
    "waic_p": waic_fit.p_waic,
    "dic": dic_fit
}
fit_df = pd.DataFrame([fit_dict])
fit_df.to_csv('.../stats/model_xtps_fit_drop_6_1_group_%s.csv'%(i))

### TRACES
traces = m.get_traces()
traces.to_csv(".../traces/traces_xtps_drop_6_1_group_%s.csv"%(i))


