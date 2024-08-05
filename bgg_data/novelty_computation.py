import pandas as pd
from sklearn.metrics import pairwise_distances
import numpy as np


def compute_dummies(data, config_index=2):
    configurations = pd.Series(data[:, config_index]).apply(
        lambda x: tuple(sorted(x))).unique()
    lut = {v: k for k, v in enumerate(configurations)}
    dummies = pd.Series(configurations).explode().str.get_dummies().groupby(level=0).sum()
    return dummies, lut


def compute_distance_matrix(dummies, metric='hamming'):
    dist_matrix = pd.DataFrame(pairwise_distances(
        dummies, metric=metric)*len(dummies.columns), index=dummies.index, columns=dummies.index)
    #dist_matrix = dist_matrix / dist_matrix.max().max()  # Normalize
    return dist_matrix


def min_max(df):
    return (df - df.min()) / (df.max() - df.min())


def compute_novelty(data, dist_matrix, lut, id_index=0, year_index=1, config_index=2, normalize=False):
    data_by_time = pd.DataFrame(data[:, [id_index, year_index, config_index]], columns=['id', 'year', 'config'])
    data_by_time = data_by_time.sort_values('year').reset_index(drop=True)
    data_by_time["configuration_id"] = data_by_time['config'].map(lambda x :lut[x])
    
    distances = dist_matrix.to_numpy()
    novelty = data_by_time
    for i in range(len(novelty)):
        focal = novelty.iloc[i]
        #previous = novelty[novelty.year.isin([focal.year-1,focal.year-2])]["configuration_id"] # other paper => prior 2 years
        previous = novelty.iloc[:(novelty.year==focal.year).argmax()]
        previous = previous[previous.id!=focal.id]["configuration_id"]
        if previous.empty:
            # If no previous configurations, assign maximum novelty
            novelty.loc[i, "novelty"] = pd.NA
        else:
            novelty_score = distances[previous.values, focal["configuration_id"]].mean()
            if(novelty_score <=0):
                print(previous.values, focal["configuration_id"])

            novelty.loc[i, "novelty"] = novelty_score

    if normalize:
        novelty["novelty"] = novelty[["novelty"]].apply(
            min_max).convert_dtypes()

    return novelty[["id","novelty"]]
