#!/usr/bin/env python
# coding: utf-8

# PPC on HDDM models

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

m = hddm.load('.../models/hddm_xtps_model_group_drop_6_1_%s.db'%(i))
m_infdata = az.from_netcdf('.../models/hddm_xtps_model_group_drop_6_1_%s.nc'%(i))
traces = m.get_traces()

#######
### 
### PPC
###
print('STARTING PPC')

obs = m.data.reset_index(drop = True)
n_sims_per_trial = 10

def inv_logit(x):
    return np.exp(x) / (1.0 + np.exp(x))


def get_param_from_trace(trace_row, param, subj, block=None, target=None):
    """
    Column names:
      a: a_subj(<block>).<subj>
      z: z_subj_trans(<block>).<subj>   (stored on logit scale; inv_logit applied)
      t: t_subj.<subj>
      v: v_subj(<block>.<target>).<subj>
    """

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

        # gen_rand_data returns (data_df, params_dict)
        sim_df, _ = gen_rand_data({'v': v, 'a': a, 't': t, 'z': z},
                                  size=1, subjs=1)

        # sim_df is a 1-row dataframe
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

## write csv
ppc_manual.to_csv('.../ppc/ppc_xtps_drop_6_1_group_%s.csv'%(i))


print('DONE WITH PPC')

