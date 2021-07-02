import datetime
import json
import os
import pandas as pd
import pickle
import time
import us
import yaml

from collections import namedtuple
from facebookads.api import FacebookAdsApi
from facebookads.adobjects.adaccount import AdAccount
from facebookads.adobjects.targetingsearch import TargetingSearch

def get_us_states(state_names):
    us_states = []
    for state in state_names:
        params = {
            'q': state,
            'type': 'adgeolocation',
            'location_types': ['region'],
        }
        r = TargetingSearch.search(params=params)
        us_states += r
        time.sleep(1)
    ## make sure all the states are in the US
    us_states = [s for s in us_states if s['country_code'] == 'US']

    us_states = list({s['key']: s for s in us_states}.values())
    us_states.sort(key=lambda x: x['name'])

    ## save output so that the request doesn't need to be made again
    with open('data/us_states.pkl', 'wb') as output:
        pickle.dump(us_states, output)
    return us_states

def convert_gender_and_interested_in(df):
    new_categories = {}
    ## for each column,
    ## unlist columns
    ## replace numbers with words
    if 'gender' in df.columns:
        df['gender'] = df['gender'].apply(lambda x: x[0] if len(x) > 0 else 3)
        new_categories['gender'] = {1: 'male', 2: 'female', 3: 'all'}
    if 'interested_in' in df.columns:
        df['interested_in'] = df['interested_in'].apply(lambda x: x[0])
        new_categories['interested_in'] = {1: 'men', 2: 'women', 3: 'men and women', 4: 'not specified'}
    if 'relationship_status' in df.columns:
        df['relationship_status'] = df['relationship_status'].apply(lambda x: x[0])
        new_categories['relationship_status'] = {
            1: 'single',
            2: 'in a relationship',
            3: 'married',
            4: 'engaged',
            6: 'not specified',
            7: 'in a civil union',
            8: 'in a domestic partnership',
            9: 'in an open relationship',
            10: 'it\'s complicated',
            11: 'separated',
            12: 'divorced',
            13: 'widowed',
        }

    df.replace(new_categories, inplace=True)

    return df

def generate_age_groups(overall_min, overall_max, group_size, unbounded_upper=True):
    AgeGroup = namedtuple('AgeGroup', ['min', 'max'])

    age_min = list(range(overall_min, overall_max, group_size))
    age_max = list(range(overall_min + group_size - 1, overall_max, group_size))

    age_groups = list(map(lambda x, y: AgeGroup(min=x, max=y), age_min, age_max))

    if unbounded_upper:
        age_groups.append(AgeGroup(min=overall_max, max=None))

    return age_groups
