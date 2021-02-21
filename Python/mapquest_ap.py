import requests
import json
import pandas as pd
import re

raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses.csv')
for row, index in zip(raw.values, raw.index):   
    raw.loc[index, 'str_address'] = re.sub('\.', '', re.sub(r'\s','+',raw.loc[index,'ADDRESS']))
    raw.loc[index, 'keystr'] = re.sub(r'\s','', "".join([str(i) for i in row if not pd.isna(i)]))
raw.loc[:,'HASH_KEY'] = pd.util.hash_pandas_object(raw.loc[:,'keystr'])

#url = ("http://www.mapquestapi.com/geocoding/v1/address?key={}&street={}"
#"&county={}&state={}&thumbMaps=false")
url = ("http://www.mapquestapi.com/geocoding/v1/address?key={}&street={}"
"&county={}&state={}&thumbMaps=false&maxResults=3")
apikey = "tRQRsDSxzHGAGzyVqlgA9ppFa1a2gYcj"
request = url.format(apikey, raw.loc[0,'str_address'], raw.loc[0,'COUNTY'],raw.loc[0,'STATE'])

test_5box = json.loads(requests.get(request).text)


url = ("http://www.mapquestapi.com/geocoding/v1/address?key={}&street={}"
"&county={}&state={}&thumbMaps=false&maxResults=3")
apikey = "tRQRsDSxzHGAGzyVqlgA9ppFa1a2gYcj"

full_locs = {}
for i in range(500,3254):
    request = url.format(apikey, raw.loc[i,'str_address'], raw.loc[i,'COUNTY'],raw.loc[i,'STATE'])
    resp = json.loads(requests.get(request).text)
    full_locs[str(raw.loc[i,'HASH_KEY'])] = resp

with open("address_locs.json","w") as f:
    json.dump(full_locs, f, indent=4)






### Single-line address requests - less accurate###
raw = pd.read_csv('C:/Users/natra/Documents/ME_2021/GunViolence_ME/python_addresses.csv')
for row, index in zip(raw.values, raw.index):
    raw.loc[index, 'address'] = ", ".join([str(i) for i in row if not pd.isna(i)])
raw.loc[:,'address']
url = "http://www.mapquestapi.com/geocoding/v1/address?key={}&location={}$thumbMaps=false"
apikey = "tRQRsDSxzHGAGzyVqlgA9ppFa1a2gYcj"
request = url.format(apikey, raw.loc[0,'address'])
test = json.loads(requests.get(request).text)
