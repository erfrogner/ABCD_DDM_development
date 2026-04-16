#!/usr/bin/env python
# coding: utf-8

# Script for running parameter recovery on simulated data from PPC

import os
import pandas as pd
import hddm
import numpy as np
import sys
import kabuki
import arviz as az
from kabuki.analyze import gelman_rubin
from hddm.generate import gen_rand_data


i = np.int(sys.argv[1])
print('sample#')
print(i)


# Load model

m = hddm.load('.../models/hddm_xtps_model_group_drop_6_1_%s.db'%(i))
m_infdata = az.from_netcdf('.../models/hddm_xtps_model_group_drop_6_1_%s.nc'%(i))
traces = m.get_traces()

#####
##
##  PPC
##
####
print('STARTING PPC')

obs = m.data.reset_index(drop = True)
n_sims_per_trial = 10

def inv_logit(x):
    return np.exp(x) / (1.0 + np.exp(x))


def get_param_from_trace(trace_row, param, subj, block=None, target=None):

    if param == "a":
        name = f"a_subj({block}).{subj}"
        if name not in trace_row.index:
            raise KeyError(f"Missing column: {name}")
        return float(trace_row[name])

    if param == "z":
        name = f"z_subj_trans({block}).{subj}"
        if name not in trace_row.index:
            raise KeyError(f"Missing column: {name}")
        return inv_logit(float(trace_row[name]))

    if param == "t":
        name = f"t_subj.{subj}"
        if name not in trace_row.index:
            raise KeyError(f"Missing column: {name}")
        return float(trace_row[name])

    if param == "v":
        if target is None:
            raise ValueError("param='v' requires target")
        name = f"v_subj({block}.{target}).{subj}"
        if name not in trace_row.index:
            raise KeyError(f"Missing column: {name}")
        return float(trace_row[name])

    raise ValueError(f"Unknown param: {param}")



sim_rows = []

for obs_i, row in obs.iterrows():
    subj = int(row["subj_idx"])
    block = row["BlockType"]     
    target = row["TargetType"]   

    sampled_trace_rows = traces.sample(n=n_sims_per_trial, replace=True)

    for _, tr in sampled_trace_rows.iterrows():
        a = get_param_from_trace(tr, "a", block=block, subj=subj)
        t = get_param_from_trace(tr, "t", subj=subj)
        z = get_param_from_trace(tr, "z", block=block, subj=subj)
        v = get_param_from_trace(tr, "v", block=block, target=target, subj=subj)


        sim_df, _ = gen_rand_data({'v': v, 'a': a, 't': t, 'z': z},
                                  size=1, subjs=1)


        sim_rt = float(sim_df["rt"].iloc[0])
        sim_resp = int(sim_df["response"].iloc[0])

        sim_rows.append({
            "rt_sim": sim_rt,
            "response_sim": sim_resp,
            "orig_trial_idx": obs_i,
            "subj_idx": subj,
            "BlockType": block,
            "TargetType": target,
            "rt_obs": row["rt"],
            "response_obs": row["response"],
        })

ppc_manual = pd.DataFrame(sim_rows)

print('DONE WITH PPC')

print('STARTING PARAMETER RECOVERY')

# Simulated data
data = ppc_manual

# Change name of response and rt columns
data = data.rename(columns={'response_sim':'response', 'rt_sim':'rt'})

# Remove simulated trials with RT over 2 and under 0.2, like in observed data
data = data[data["rt"].between(0.2, 2.0)]


# Make a row number column to make sure it's arranged in the same order as before later on
data["row_nr"] = np.arange(len(data))

# Reduce to from ten per original trial to one
data_red = (
    data
    .groupby(["subj_idx", "orig_trial_idx"], as_index=False)
    .first()
    .sort_values("row_nr")
    .reset_index(drop=True)
)

print('RUNNING HDDM ON SIMULATED DATA')


m = hddm.HDDM(data_red,depends_on={'a':'BlockType','v':['BlockType','TargetType'],'z':'BlockType'},
              include=['a', 't', 'v', 'z','st'],p_outlier=0.05)

              
m_infdata = m.sample(2500, burn = 1500, chains = 4, 
                     loglike = False, ppc = False, 
                     return_infdata = True, save_name = '.../param_rec/hddm_xtps_model_parrec_'+str(i)) 

### CONVERGENCE AND STATS
summ = az.summary(m_infdata, round_to = 4)
summ.to_csv('.../param_rec/stats_group_%s.csv'%(i))

### TRACES
traces = m.get_traces()
traces.to_csv(".../param_rec/traces_group_%s.csv"%(i))

