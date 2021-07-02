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

from utils import generate_age_groups

def get_interested_in_and_relationship_status_by_age(gender, interested_in, relationship_status, age_min, age_max, country='US'):
    targeting_spec = {
        'geo_locations': {
            'countries': [country],
        },
        'genders': gender,
        'interested_in': interested_in,
        'relationship_statuses': relationship_status,
        'age_min': age_min,
        'age_max': age_max,
    }
    params = {
        'currency': 'USD',
        'optimize_for': 'NONE',
        'targeting_spec': targeting_spec,
    }
    reach_estimate = account.get_reach_estimate(params=params)
    return {
        'gender': gender,
        'interested_in': interested_in,
        'relationship_status': relationship_status,
        'country': country,
        'age_min': age_min,
        'age_max': age_max,
        'estimate': reach_estimate[0]['users'],
        'timestamp': str(datetime.datetime.utcnow()),
    }

if __name__ == '__main__':
    ## open yaml config file
    with open('config.yml', 'r') as ymlfile:
        cfg = yaml.load(ymlfile)

    ## bootstrap Facebook Ads API
    my_app_id = cfg['app_id']
    my_app_secret = cfg['app_secret']
    my_access_token = cfg['access_token']
    FacebookAdsApi.init(my_app_id, my_app_secret, my_access_token)

    ## create account object
    account = AdAccount(cfg['ad_account_id'])

    ## create data subdirectory if necessary
    if not os.path.exists('data'):
        os.mkdir('data')

    ages = generate_age_groups(18, 65, 1)
    gender = [[1], [2], []]
    interested_in = [[1], [2], [3], [4]]
    # 1 through 13 are valid relationship statuses, EXCEPT 5:
    # 1: single
    # 2: in_relationship
    # 3: married
    # 4: engaged
    # 6: not specified
    # 7: in a civil union
    # 8: in a domestic partnership
    # 9: In an open relationship
    # 10: It's complicated
    # 11: Separated
    # 12: Divorced
    # 13: Widowed
    relationship_status = [[i] for i in range(1, 14) if i is not 5]

    # expand list of arguments as dicts
    kwargs_list = [
        {
            'gender': g,
            'interested_in': i,
            'relationship_status': r,
            'age_min': age.min,
            'age_max': age.max,
        }
        for age in ages for g in gender for i in interested_in for r in relationship_status
    ]

    data_list = []
    error_list = []
    with open('data/US_interested_in_and_relationship_status_by_age_1yr.json', 'a') as f:
        for kwargs in kwargs_list:
            try:
                r = get_interested_in_and_relationship_status_by_age(**kwargs)
                json.dump(r, f)
                f.write('\n') # each json object separated by newline
                data_list.append(r)
                time.sleep(5)
            except Exception as e:
                print(e)
                error_info = kwargs.copy()
                error_info.update({'error': e})
                error_list.append(error_info)
                ## sleep longer, because an error may mean rate-limiting
                ## (error code 17)
                time.sleep(65)

    # pickle errors
    with open('data/ERRORS_US_interested_in_and_relationship_status_by_age_1yr.pkl', 'wb') as output:
        pickle.dump(error_list, output)
