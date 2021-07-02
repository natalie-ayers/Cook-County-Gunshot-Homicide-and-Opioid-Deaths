import requests
import json
import pandas as pd
import re


# Format addresses pulled from ME records; add hash key
#raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses.csv')
#raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses_2.csv')
#raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses_3.csv')
raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses_4.csv')
for row, index in zip(raw.values, raw.index):   
    #print(index)
    raw.loc[index, 'str_address'] = re.sub('\.', '', 
        re.sub(r'\s','+',raw.loc[index,'ADDRESS']))
    raw.loc[index, 'keystr'] = re.sub(r'\s','', 
        "".join([str(i) for i in row if not pd.isna(i)]))
raw.loc[:,'HASH_KEY'] = pd.util.hash_pandas_object(raw.loc[:,'keystr'])

# Build dynamic request
url = ("http://www.mapquestapi.com/geocoding/v1/address?key={}&street={}"
"&county={}&state={}&city={}&thumbMaps=false&maxResults=3")
apikey = "tRQRsDSxzHGAGzyVqlgA9ppFa1a2gYcj"

# Use MapQuest Geocoding API to pull locations for all addresses
full_locs = {}
for i in range(raw.shape[0]):
    request = url.format(apikey, raw.loc[i,'str_address'], 
                        raw.loc[i,'COUNTY'],raw.loc[i,'STATE'],
                        raw.loc[i,'CITY'])
    resp = json.loads(requests.get(request).text)
    full_locs[str(raw.loc[i,'HASH_KEY'])] = resp

# Load addresses with locations into json
with open("C:/Users/natra/Documents/ME_2021/GunViolence_ME/address_locs_4.json", "w") as f:
    json.dump(full_locs, f, indent=4)


"""
### Single, 5-box address request for testing ###
url = ("http://www.mapquestapi.com/geocoding/v1/address?key={}&street={}"
"&county={}&state={}&thumbMaps=false&maxResults=3")
apikey = "tRQRsDSxzHGAGzyVqlgA9ppFa1a2gYcj"
request = url.format(apikey, raw.loc[0,'str_address'], 
                    raw.loc[0,'COUNTY'],raw.loc[0,'STATE'])
test_5box = json.loads(requests.get(request).text)



### Single-line address requests - less accurate###
raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses.csv')
for row, index in zip(raw.values, raw.index):
    raw.loc[index, 'address'] = ", ".join([str(i) for i in row if not pd.isna(i)])
raw.loc[:,'address']
url = "http://www.mapquestapi.com/geocoding/v1/address?key={}&location={}$thumbMaps=false"
apikey = "tRQRsDSxzHGAGzyVqlgA9ppFa1a2gYcj"
request = url.format(apikey, raw.loc[0,'address'])
test = json.loads(requests.get(request).text)
"""


"""
References used:
https://developer.mapquest.com/documentation/geocoding-api/
https://stackoverflow.com/questions/31929066/reverse-geocoding-using-mapquest-api-and-python
"""




