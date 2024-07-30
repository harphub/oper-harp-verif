## How to interprete a panelification plot
The scores which are given in the definition file are calculated using harpSpatial.

The models are then ranked according to the scores.

The ranks are plotted in the panelification plots.

**Colour scheme of the ranks:**

- perfect scores (green)
- rank 1 (gold)
- rank 2 (silver)
- rank 3 (bronze)
- rank 4 and greater (white)
- FSS not skilful (< 0.5) (red)
- NA (black)

When 2 or more models have the same rank, the next model with a higher rank will have a rank that is following up after the "missing" ranks.
e.g. 3 model have rank 2. 3 and 4 are the missing ranks and the next rankded model will have rank 5.

**Information displayed in the plot**

  In the first panel the observation field is displayed, there is one panel for each of the verified models.
  
  observations:
  
  obs title: name + valid observation time

  models:
  
  * model title:
     * left: model name, initialisation time + lead time, (average FSS rank)
     * right: average rank of basic scores (this is the average of all non-fss scores
    that are passed in the definiton file) and ranking of the models according to this average rank

  * top box:
    * ranks of FSS (using thresholds)
    * ranks of FSS (using percentiles)
    * basic scores - displaying the actual values, (rank according to the value)


## How ranks are calculated
tbd

  

## Good to know
windows: 

The actual boxes have window size: 2*n+1,
with n being the amount of grid points

