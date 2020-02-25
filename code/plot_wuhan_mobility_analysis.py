"""Plot figures for the mobility analysis of reported cases."""


import numpy as np
import pandas as pd
from scipy.stats import linregress
from utilities import read_geojson, match_baidu, plot_choropleth_map

import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
from mpl_toolkits.axes_grid1 import make_axes_locatable


# Get a range of dates that satisfy the prespecified lag between the mobility
# data and the cases data
DATES_MOBILITY = pd.date_range(start='2020-01-01', end='2020-02-05')
DATES_CASES = pd.date_range(start='2020-01-07', end='2020-02-04')
LAG = pd.Timedelta('6 days')

START = max(min(DATES_MOBILITY), min(DATES_CASES - LAG))
END = min(max(DATES_MOBILITY), max(DATES_CASES - LAG))
DATES = pd.date_range(start=START, end=END)

DATE_RANGE_MOBILITY = DATES.strftime('%Y-%m-%d')
DATE_RANGE_CASES = (DATES + LAG).strftime('%Y-%m-%d')
DATE_BASE_MOBILITY = range(len(DATE_RANGE_MOBILITY))
DATE_BASE_CASES = range(len(DATE_RANGE_CASES))

# Dates for major ticks
DATE_LABELS_MOBILITY = [_str for i, _str in enumerate(DATES.strftime('%b %d'))
                        if i % 5 == 0]
DATE_TICKS_MOBILITY = [i for i in DATE_BASE_MOBILITY if i % 5 == 0]
DATE_LABELS_CASES = [_str
                     for i, _str in enumerate((DATES + LAG).strftime('%b %d'))
                     if i % 5 == 0]
DATE_TICKS_CASES = [i for i in DATE_BASE_CASES if i % 5 == 0]

# Dates to zoom in
DATE_ZOOM_CASES = ['2020-01-11', '2020-01-18', '2020-01-25',  '2020-02-01']
DATE_ZOOM_MOBILITY = ['2020-01-14', '2020-01-18', '2020-01-25',  '2020-01-29']

# Colormap for the choropleth maps
CM = plt.cm.OrRd
# Location of Wuhan
WUHAN_LATLON = [114.283333, 30.583332]


def add_annotation(ax, text, x_rel, y_rel, **kwargs):
    """Add text to a given position."""
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    x = xmin + x_rel * (xmax - xmin)
    y = ymin + y_rel * (ymax - ymin)
    ax.text(x, y, text, **kwargs)


def tweak_xaxis(ax, major_ticks, major_labels, minor):
    # Widen the x limits by 1
    minor_ticks = range(minor.start - 1, minor.stop)
    ax.set_xlim(minor_ticks.start, minor_ticks.stop)
    # Change the minor and major ticks and labels of the x axis
    ax.set_xticks(minor_ticks, minor=True)
    ax.set_xticks(major_ticks)
    ax.set_xticklabels(major_labels)


def main():
    """
    Generate a plot (Fig 1) to illustrate the mobility data from Baidu, and
    another plot (Fig 2) for the correlation between the mobility data and the
    reported cases.

    Notes
    -----
    Fig 1 (a): Time series of migration scale indices from Wuhan.
    Fig 1 (b): Snapshots of choropleth map of travels from Wuhan to provinces.

    Fig 2 (a): Snapshots of log-log regression between the cumulative cases
               and the percentage of travels from Wuhan 6 days before.
    Fig 2 (b): Time series of goodness of fit for a negative binomial model.

    """
    # Fig 1: Human mobility from Wuhan
    # Setup figure
    fig = plt.figure(figsize=(10, 4.5))
    gs = fig.add_gridspec(nrows=2, ncols=4, height_ratios=[1.25, 1])

    # Panel (a)
    # Load data
    tidy = pd.read_csv('../data/migration_scale_from_wuhan.csv')
    tidy['year'] = tidy['date'].apply(lambda _str: _str[:4])
    tidy.loc[:, 'date'] = tidy['date'].apply(lambda _str: _str[5:])
    data = tidy.pivot(index='date', columns='year', values='scale')
    date_range = DATE_RANGE_MOBILITY.map(lambda _str: _str[-5:])
    scaleindex = data.loc[data.index.isin(date_range), :]

    # Plot time series of migration scale index for 2019 and 2020
    ax = fig.add_subplot(gs[0, :])
    scaleindex.plot(ax=ax, legend=False,
                    linewidth=2, color=['#666666', '#f66136'],
                    marker='o', markeredgecolor='#333333', markeredgewidth=1.5)
    ax.legend(loc=2, fontsize=8, framealpha=0)

    # Add vertical lines indicating the start of quarantine and the lunar
    # new year
    quar = DATE_RANGE_MOBILITY.get_loc('2020-01-23')
    ax.axvline(x=quar, ls='dashdot', color='#a52a2a', lw=2, zorder=1)
    add_annotation(ax, 'Quarantine', 0.77, 0.2, ha='right', color='black')

    ny = DATE_RANGE_MOBILITY.get_loc('2020-01-25')
    ax.axvline(x=ny, ls='dotted', color='#a52a2a', lw=2, zorder=1)
    add_annotation(ax, 'Lunar New\nYear', 0.88, 0.8, ha='left', color='black')

    # Add axes labels and some aesthetic tweaks
    tweak_xaxis(ax, major_ticks=DATE_TICKS_MOBILITY,
                major_labels=DATE_LABELS_MOBILITY,
                minor=DATE_BASE_MOBILITY)
    ax.set_xlabel('')
    ax.set_ylabel(r'Migration scale index', fontsize=8, x=-0.15)
    ax.set_ylim(top=14.75)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.tick_params(axis='x', which='major', labelsize=8)
    ax.tick_params(axis='y', which='major', labelsize=7)

    # Panel (b)
    # Load data
    data = read_geojson()
    geo = match_baidu(data)

    data = pd.read_csv('../data/human_mobility_from_wuhan.csv')
    mobility = data.loc[data['date'].isin(DATE_RANGE_MOBILITY), :]
    mobility.loc[mobility['province'] == 'Hubei', 'scale'] = np.nan
    s_min, s_max = mobility['scale'].min(), mobility['scale'].max()

    for i, date in enumerate(DATE_ZOOM_MOBILITY):
        # Plot the choropleth map of migration for selected dates
        ax = fig.add_subplot(gs[1, i])
        divider = make_axes_locatable(ax)
        cax = divider.append_axes('left', size='3%', pad=0.1)
        legend = True if i == 0 else False

        mob = mobility.loc[mobility['date'] == date, :]
        plot_choropleth_map(ax, geo, mob, col_color='scale',
                            c_missing='gainsboro', cax=cax, cmap=CM,
                            norm=LogNorm(vmin=max(s_min, 5e-4), vmax=s_max),
                            legend=legend,
                            legend_kwds={'label': 'Migration (log scale)',
                                         'ticks': [0.001, 0.01, 0.1]})
        geo.boundary.plot(ax=ax, linewidth=0.1, color='#333333')

        # Plot an indicator of Wuhan
        ax.scatter(*WUHAN_LATLON, c='dodgerblue', s=15, edgecolor='white',
                   linewidth=1, zorder=4)

        # Add a colorbar, axes labels, and some aesthetic tweaks
        ax.set_axis_off()
        ax.set_title(f'{date}', fontsize=8, y=0.95)
        if i == 0:
            label_loc = (WUHAN_LATLON[0] + 12, WUHAN_LATLON[1] - 7)
            ax.plot(*zip(WUHAN_LATLON, label_loc), color='black', lw=1,
                    zorder=3)
            ax.text(label_loc[0]+2.5, label_loc[1]-1.5, 'Wuhan',
                    ha='center', va='top', fontsize=8)
            cax.set_yticklabels(['0.001', '0.01', '0.1'], fontsize=7)
            cax.yaxis.set_ticks_position('left')
            cax.yaxis.set_label_position('left')
            cax.set_ylabel('Migration (log scale)', fontsize=7.5)
        else:
            cax.set_axis_off()

    # Adjust spaces between subplots
    plt.subplots_adjust(wspace=0.15, hspace=0.3)

    # Fig 2: Correlation analysis (negative binomial model) between mobility
    # and cases data
    # Setup figure
    fig = plt.figure(figsize=(10, 4.5))
    gs = fig.add_gridspec(nrows=2, ncols=4, height_ratios=[1, 1.25])

    # Panel (a)
    # Load data
    data = pd.read_csv('../data/human_mobility_from_wuhan.csv')
    mobility = data.loc[data['date'].isin(DATE_RANGE_MOBILITY), :]
    mobility.loc[mobility['province'] == 'Huibei', 'percentage'] = np.nan
    provinces = mobility['province'].unique().tolist()

    data = pd.read_csv('../data/wuhan_cases.csv')
    data = data.rename(columns={data.columns[0]: 'date'})
    data = data.loc[:, ['date'] + provinces]
    melted = pd.melt(data, id_vars='date', value_vars=provinces,
                     var_name='province', value_name='cases')
    cases = melted.loc[melted['date'].isin(DATE_RANGE_CASES), :]
    cases.loc[cases['province'] == 'Hubei', 'cases'] = np.nan

    # Merge the cases and mobility datasets
    # with modified dates in mobility data by the prespecified lag
    dates_mobility = pd.to_datetime(mobility['date'])
    mobility_modified = mobility.copy()
    mobility_modified.loc[:, 'date'] = (dates_mobility + LAG).map(lambda d: d.strftime('%Y-%m-%d'))
    data = cases.merge(mobility_modified, on=['date', 'province'], how='left')

    for i, date in enumerate(DATE_ZOOM_CASES):
        # Correlation analysis between the logarithm of cumulative cases and
        # the logarithm of percentage of travels from Wuhan to the provinces
        subset = data.loc[data['date'] == date, :].dropna()
        s, y_0, rval, _, _ = linregress(np.log(subset['percentage']),
                                        np.log(subset['cases']))
        correlation = rval ** 2
        p_min = np.log(subset['percentage']).min()
        p_max = np.log(subset['percentage']).max()
        regline = ([p_min, p_max], [y_0 + s * p_min, y_0 + s * p_max])

        # Plot the scatter plot of log-cases and log-percentage
        ax = fig.add_subplot(gs[0, i])
        dat = data.loc[data['date'] == date, :].dropna()
        ax.scatter(np.log(dat['percentage']), np.log(dat['cases']), marker='o',
                   c='white', s=10, edgecolor='#333333', linewidth=1.05)

        # Add the regression line
        ax.plot(*regline, label=r'$R^2 = %.2f$' % correlation,
                linewidth=1.35, color='#f66136', linestyle=':', zorder=0)
        ax.legend(loc=2, fontsize=7, framealpha=0)

        # Add axes labels and some aesthetic tweaks
        ax.set_title(f'{date}', fontsize=8, y=1.05)
        ax.set_xlabel('Percent of travels\n(6-day prior, log-scale)', fontsize=8)
        if i == 0:
            ax.set_ylabel('Cumulative cases\n(log-scale)', fontsize=8, x=-0.1)
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.tick_params(axis='both', which='major', labelsize=7)

    # Panel (b)
    # Load data
    data = pd.read_csv('../data/roll_mean_nb_r2.csv', index_col='date')

    # Plot the time series of correlation in the analysis
    ax = fig.add_subplot(gs[1, :])
    ax.plot(DATE_RANGE_CASES, data.loc[DATE_RANGE_CASES, 'r2'].values,
            linewidth=2, color='#f66136', marker='o', markersize=6,
            markeredgecolor='#333333', markeredgewidth=1.5)

    # Add a vertical line indicating the start of quarantine
    quar = DATE_RANGE_CASES.get_loc('2020-01-23')
    ax.axvline(x=quar, ls='dashdot', color='#a52a2a', lw=2, zorder=1)
    add_annotation(ax, 'Quarantine', 0.67, 0.2, ha='right', color='black',
                   fontsize=9)

    # Add axes labels and some aesthetic tweaks
    tweak_xaxis(ax, major_ticks=DATE_TICKS_CASES,
                major_labels=DATE_LABELS_CASES,
                minor=DATE_BASE_CASES)
    ax.set_xlabel('')
    ax.set_ylabel(r'$R^2$ values', fontsize=8, x=-0.1)
    ax.set_ylim(data['r2'].min() - 0.05, 1.05)
    ax.tick_params(axis='x', which='major', labelsize=8)
    ax.tick_params(axis='y', which='major', labelsize=7)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    # Adjust spaces between subplots
    plt.subplots_adjust(wspace=0.15, hspace=0.5)

    # Show the two figures
    plt.show()

    return


if __name__ == '__main__':
    main()
