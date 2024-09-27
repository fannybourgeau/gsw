# Package gsw 

*Use GSW change intensity data to extract metrics for each city in GloUrb's sample1*


The gsw package gathers functions used to extract metrics from GSW change intensity data.

- **gws_extract_pixels()** extracts 1000 pixels of GSW change intensity per zone x reach polygon for a city
- **gws_summarise()** summarises results from the 1000 pixels randomly extracted for each zone x reach of a particular city

In **data-raw/gsw_collect.qmd** we show how these functions can be used iteratively to collect these metrics for the 300 cities of GloUrb's sample1.

In that same document, we show how results for each city are appended to tables in the **glourb database** :

- **gsw_pixels** (resulting from gws_extract_pixels())
- **gws_summary** (resulting from gws_summarise()) .

The functions to **display the results** (in [glourbapp](https://isig-apps.ens-lyon.fr/apps/glourb/glourbapp/) mainly) are:

- **gsw_summary()** which returns a table 
- **gsw_summary_plot()** which returns a plot
