## Basics
This repo provides tools and a default configuration for the ingestion, cleaning,
and preparation of data from the British Election Study 
([data found here](https://www.britishelectionstudy.com/data-objects/panel-study-data/)).

We seek to examine how acceptance and resistance to elite-level changes 
in political discourse regarding redistributive policies are affected by 
personal material interest, focusing on the general election in May 2015 
and the subsequent election of Jeremy Corbyn to leader of the Labour Party 
in September 2015.

## Everything Else
See the Documentation at the [project wiki](https://github.com/charraig/bes_policy_shift/wiki).

## Improvements to be Made
* Make configuration of the metadata columns (e.g. id, weights) programmatic so
that it can be changed easily through the `src/config/config.R` script.