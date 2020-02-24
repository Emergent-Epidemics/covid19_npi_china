"""Utility functions for plotting human mobility data from Baidu Huiyan."""


import geopandas as gpd
import pandas as pd


def plot_choropleth_map(ax, data_geo, data_color, col_color,
                        col_merge='province', def_val=None,
                        c_missing='lightgrey', **kwargs):
    """
    Plot the choropleth map with a given data for coloring.

    Params
    ------
    ax (matplotlib.axes.Axes): Axes to plot.

    data_geo (pandas.DataFrame): Geographical data for the choropleth map.

    data_color (pandas.DataFrame): Dataset including the variable for coloring.

    col_color (str): Column name of the variable for coloring.

    col_merge (str): Column on which the two datasets are merged. Default to
                     'province'.

    def_val (float): Default value to replace NaNs if not set to None.

    c_missing (str): Color for missing data. Default to 'lightgrey'.

    """
    data = data_geo.merge(data_color, on=col_merge, how='left')

    if def_val is not None:
        # Replace NaNs to a chosen default value
        data.loc[data[col_color].isna(), col_color] = def_val
        # Plot the choropleth map for non-missing data
        data.plot(column=col_color, ax=ax, **kwargs)
    else:
        isnan = pd.isna(data[col_color])
        # Plot missing data as regions of a given color
        if isnan.any():
            data.loc[isnan, :].plot(color=c_missing, ax=ax)
        # Plot the choropleth map for non-missing data
        data.loc[~isnan, :].plot(column=col_color, ax=ax, **kwargs)


def read_geojson(path='../data/'):
    """Return geo-data loaded from the geojson on github/d3cn/data."""
    data = gpd.read_file(path + 'china-province.geojson')

    # Add province labels used in the line list
    cols = ('ADM1_ZH', 'province')
    table = pd.read_csv(path + 'region_translation.csv').loc[:, cols]

    table = table.sort_values(by='ADM1_ZH').reset_index()
    names = data['NAME'].sort_values().reset_index()
    labels = pd.concat([names, table], axis=1).loc[:, ('NAME', 'province')]
    return data.merge(labels, on='NAME')


def match_baidu(data):
    """Return geo-data with administrative regions matched to Baidu Huiyan."""
    baidu_exclusion = ['Taiwan', 'Hong Kong', 'Macao']
    mask = ~data['province'].isin(baidu_exclusion)
    return data.loc[mask, :]


def main():
    """Empty main function."""
    return


if __name__ == '__main__':
    main()
