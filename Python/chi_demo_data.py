import censusdata
import geopandas as gpd
import pandas as pd

# --- Code Modified from U Chicago Harris MSCAPP Machine Learning for Public Policy class taught by Nick Feamster --- # 

import censusdata
import geopandas as gpd

def load_multiyear_acs(year_start=2010, year_end_plus1=2020):
    """
    Load ACS 5-year demographic data for Cook County, IL
        by Census Tract
    """

    years = range(year_start, year_end_plus1)

    acs_blockgroup = gpd.GeoDataFrame(columns=['Total_Population', 'Population_Male', \
        'Population_Female', 'Population_White', 'Population_Black',\
        "Population_Native_American","Population_Asian","Population_Latino",\
        "Median_Income_All","Median_Income_White","Median_Income_Black",\
        "Median_Income_Native_American","Median_Income_Asian","geoid10",\
        "geometry","ACS_YEAR"])

    for year in years:

        # By Sex by Race
        # Note: IL FIPS = 17, Cook County FIPS = 031
        acs_tract = censusdata.download("acs5", year, censusdata.censusgeo(
            [("state", "17"), ("county", "031"), ("tract", "*")]), \
            ["B01003_001E","B01001_002E","B01001_026E","B01001H_001E","B01001B_001E",\
            "B01001C_001E","B01001D_001E","B01001I_001E", "B19013_001E","B19013H_001E", \
            "B19013B_001E","B19013C_001E","B19013D_001E","GEO_ID"])

        # Download Census block boundaries for Chicago 
        census_tract_gdf = gpd.read_file("https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON?$limit=9999999")

        # Extract 11-digit code from acs GEO_ID
        acs_tract["geoid10"] = acs_tract["GEO_ID"].map(lambda x: str(x)[-11:])

        # Merge ACS data with Census tract boundaries 
        tract_census = (gpd.GeoDataFrame(acs_tract.merge(census_tract_gdf, on="geoid10", how="inner"), 
                                    crs=4326))

        # Limit columns 
        tract_census_gdf = tract_census[["B01003_001E","B01001_002E","B01001_026E",\
            "B01001H_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001I_001E",\
            "B19013_001E","B19013H_001E","B19013B_001E","B19013C_001E","B19013D_001E",\
            "geoid10", "geometry"]].drop_duplicates()
        tract_census_gdf.rename(columns={"B01003_001E": "Total_Population", \
            "B01001_002E": "Population_Male","B01001_026E":"Population_Female", \
            "B01001H_001E": "Population_White", "B01001B_001E":'Population_Black', \
            "B01001C_001E": "Population_Native_American", "B01001D_001E":'Population_Asian', \
            "B01001I_001E":'Population_Latino',"B19013_001E":"Median_Income_All", \
            "B19013H_001E": "Median_Income_White", "B19013B_001E":"Median_Income_Black",\
            "B19013C_001E": "Median_Income_Native_American","B19013D_001E":"Median_Income_Asian"}, inplace=True)
        tract_census_gdf['ACS_YEAR'] = year

        acs_blockgroup = acs_blockgroup.append(tract_census_gdf, ignore_index=True)

    acs_blockgroup = gpd.GeoDataFrame(acs_blockgroup)

    acs_blockgroup.to_file('acs_blockgroup.geojson', driver='GeoJSON')
    
    return acs_blockgroup

neighborhoods = gpd.read_file('https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON')
neighborhoods = neighborhoods.loc[:,('pri_neigh','sec_neigh','geometry')]
neighborhoods['geometry'] = neighborhoods['geometry'].to_crs(4326)
neighborhoods['geometry'] = neighborhoods['geometry'].to_crs('EPSG:3857')

acs_blockgroup = gpd.read_file('acs_blockgroup.geojson')
acs_blockgroup['geom'] = acs_blockgroup['geometry']
acs_blockgroup['geometry'] = acs_blockgroup['geometry'].to_crs('EPSG:3857')
acs_blockgroup['geometry'] = acs_blockgroup['geometry'].to_crs(4326)
acs_blockgroup_neigh = gpd.sjoin(acs_blockgroup,neighborhoods,\
    op="intersects",how='left')

acs_blockgroup_neigh['geometry'] = acs_blockgroup_neigh['geometry'].to_crs('EPSG:3857')
acs_blockgroup_neigh['geometry'] = acs_blockgroup_neigh['geometry'].to_crs(4326)

acs_blockgroup_neigh.reset_index(inplace=True)
acs_blockgroup_neigh.to_crs =4326


join_areas = acs_blockgroup_neigh.join(areas).drop_duplicates()
areas = acs_blockgroup_neigh['geometry'].intersection(acs_blockgroup['geometry']).area
areas.name= 'Areas'
join_areas = acs_blockgroup_neigh.join(areas).drop_duplicates()
join_areas['geometry'] = join_areas['geometry'].to_crs(4326)
join_areas.reset_index(inplace=True)


join_max=join_areas.groupby(['pri_neigh','geoid10'])['Areas'].max()
join_max.reset_index(inplace=True)
join_max_idx =join_max.reset_index()
max_acs_areas = join_areas.merge(join_max_idx,on=['geoid10','ACS_YEAR','Areas'])


austin = neighborhoods[neighborhoods['pri_neigh']=='Austin']
acs_blockgroup_austin = gpd.sjoin(acs_blockgroup,austin,\
    op="intersects")

acs_blockgroup_austin['geometry'] = acs_blockgroup_austin['geometry'].to_crs('EPSG:3857')
acs_blockgroup['geometry'] = acs_blockgroup['geometry'].to_crs('EPSG:3857')

areas = acs_blockgroup_austin['geometry'].intersection(acs_blockgroup['geometry']).area
areas = areas[areas.notna()]


acs_blockgroup_neigh.to_crs('EPSG:3857')
acs_blockgroup_neigh['intersect_area'] = \
    acs_blockgroup_neigh.apply(lambda x: \
    acs_blockgroup_neigh['geometry'].intersection(\
    neighborhoods['geometry'].iloc[acs_blockgroup_neigh['index_right']]).area, \
    axis=1)
acs_blockgroup_neigh.plot()

acs_blockgroup_neigh['intersect_area'] = \
     acs_blockgroup_neigh.apply(lambda x: x.geom.intersection(x.geometry).area)

acs_blockgroup_neigh['intersect_area'] = \
     acs_blockgroup_neigh.apply(lambda x: x.intersection(x.geometry).area)