import pandas as pd
from sodapy import Socrata
import geopandas as gpd
from shapely.geometry import shape


client = Socrata("data.cityofchicago.org", None)
results = client.get("y6yq-dbs2", limit=200)

chi_neighborhoods = pd.DataFrame(results)
chi_neighborhoods['geometry'] = chi_neighborhoods['the_geom'].apply(shape)
chi_neighborhoods = gpd.GeoDataFrame(chi_neighborhoods)
chi_neighborhoods = chi_neighborhoods.drop('the_geom', axis=1)
chi_neighborhoods.to_file('chi_neighborhoods.shp')
