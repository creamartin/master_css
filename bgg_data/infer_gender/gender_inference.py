import glob
import pandas as pd
from collections import defaultdict
import os
import pickle

def load_names_data(path_pattern):
    """Load names data from files matching the given path pattern."""
    base_path = os.path.dirname(__file__)
    full_path_pattern = os.path.join(base_path, path_pattern)
    files = glob.glob(full_path_pattern)
    if not files:
        raise FileNotFoundError(f"No files found for pattern: {full_path_pattern}")
    data_frames = [pd.read_csv(filename, header=None, names=["name", "gender", "count"]) for filename in files]
    return pd.concat(data_frames, axis=0)

def process_names_data(df):
    """Process the names DataFrame to calculate the probability of being male."""
    names = df.groupby(["name", "gender"]).sum().reset_index()
    names = names.merge(names.groupby("name")["count"].sum(), on="name", suffixes=("", "_total"))
    names["prob"] = (names["count"] / names.count_total).round(2)
    names = names[names.gender == "M"].merge(names[names.gender == "F"], how="outer", on=["name", "count_total"], suffixes=("_m", "_f"))
    names.loc[names.count_m.isna(), "prob_m"] = 1 - names[names.count_m.isna()].prob_f
    names.count_f = names.count_f.fillna(0)
    names.count_m = names.count_m.fillna(0)
    names = names.drop(columns=["gender_m", "gender_f", "prob_f"])
    names = names.rename({"prob_m": "is.man"}, axis=1)
    return names

def create_gender_inference_dict(names):
    """Create a dictionary for gender inference."""
    infer_gender = {}
    names.name = names.name.str.lower()
    infer_gender.update(names[["name", "is.man"]].set_index("name").to_dict()["is.man"])
    return infer_gender

def load_or_create_gender_inference_dict(names_path_pattern, dict_path):
    """Load gender inference dictionary from a file, or create it if it doesn't exist."""
    if os.path.exists(dict_path):
        with open(dict_path, 'rb') as f:
            gender_inference_dict = pickle.load(f)
    else:
        names_df = load_names_data(names_path_pattern)
        processed_names = process_names_data(names_df)
        gender_inference_dict = create_gender_inference_dict(processed_names)
        with open(dict_path, 'wb') as f:
            pickle.dump(gender_inference_dict, f)
    return gender_inference_dict

# Singleton pattern for loading/creating the dictionary
base_path = os.path.dirname(__file__)
dict_path = os.path.join(base_path, "gender_inference_dict.pkl")
names_path_pattern = os.path.join(base_path, "names/yob*.txt")
gender_inference_dict = load_or_create_gender_inference_dict(names_path_pattern, dict_path)

def infer_gender(names_series):
    """Infer the gender score for a series of names using the pre-loaded dictionary."""
    def get_gender(name):
        return gender_inference_dict.get(name, pd.NA)
    
    first_names = names_series.str.split(pat=r"[^a-zA-Z]", regex=True)
    inferred_genders = first_names.explode().str.lower().map(get_gender).dropna().groupby(level=0).apply(list).map(lambda x: x[0])
    inferred_genders = inferred_genders.map(lambda x: x if x >= 0.9 or x <= 0.1 else pd.NA,na_action="ignore")
    inferred_genders = inferred_genders.map(lambda x: 1 if x > 0.5 else 0 if x <= 0.5 else -1,na_action="ignore")
    return inferred_genders