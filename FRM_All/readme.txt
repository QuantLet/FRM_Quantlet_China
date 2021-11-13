To update channel:

1. Run preprocessing code for the channel in Input/channel, and do manual preprocessing. 
2. Add the cleaned data to Input/channel/xxxx-xxxx and change end date.
2. Run FRM_Quantlet.R after adjusting the update section in the header.

Upon first run for channel:

1. Recreate folder structure.
2. Ensure that input format is in accordance with existing channels.
3. If needed create Input/channel/channel_prep.R

If further interested in centrality measures:

1. Run FRM_Centrality_Analysis.R after adjusting the update section in the header.