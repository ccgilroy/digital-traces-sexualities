import json
import pandas as pd

from utils import convert_gender_and_interested_in

data_json = []
with open('data/by_state_and_age_5yr.json') as f:
    for line in f:
        try:
            data_json.append(json.loads(line))
        except json.JSONDecodeError:
            # one line broke off mid-line when the ssh connection broke
            print(line)

cols = ['state', 'gender', 'interested_in', 'age_min', 'age_max', 'estimate', 'timestamp']
data = pd.DataFrame(data_json, columns=cols)
data = convert_gender_and_interested_in(data)

# don't group by age_max because NaNs get dropped
# take the estimate with the most recent timestamp using last()
data_cleaned = (data.groupby(['state', 'gender', 'interested_in', 'age_min'],
                             sort=False)
                    .last()
                    .reset_index())

data_cleaned.to_csv('data/by_state_and_age_5yr_from_json.csv', index=False)

def convert_json_to_csv(json_file, cols, grouping_cols=None):
    if grouping_cols is None:
        grouping_cols = cols[0:4]
    data_json = []
    with open(json_file) as f:
        for line in f:
            try:
                data_json.append(json.loads(line))
            except json.JSONDecodeError:
                print(line)

    data = pd.DataFrame(data_json, columns=cols)
    data = convert_gender_and_interested_in(data)

    data_cleaned = data.groupby(grouping_cols, sort=False).last().reset_index()
    return data_cleaned

us_cols = ['country', 'gender', 'interested_in', 'age_min', 'age_max', 'estimate', 'timestamp']
data_us_by_age_1yr = convert_json_to_csv('data/US_by_age_1yr.json', us_cols)
data_us_by_age_1yr.to_csv('data/US_by_age_1yr.csv', index=False)

us_cols2 = ['country', 'gender', 'relationship_status', 'age_min', 'age_max', 'estimate', 'timestamp']
data_us_rel_1yr = convert_json_to_csv('data/US_relationship_status_by_age_1yr.json', us_cols2)
data_us_rel_1yr.to_csv('data/US_relationship_status_by_age_1yr.csv', index=False)

us_cols3 = ['country', 'gender', 'interested_in', 'relationship_status', 'age_min', 'age_max', 'estimate', 'timestamp']
data_us_int_rel = convert_json_to_csv('data/US_interested_in_and_relationship_status_by_age_1yr.json', us_cols3, grouping_cols=us_cols3[0:5])
data_us_int_rel.to_csv("data/US_int_rel_1yr.csv", index=False)
