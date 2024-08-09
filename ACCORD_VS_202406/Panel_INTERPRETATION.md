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
* Basic scores
  
  scores are ordered and the ranks given accordingly
  the ordering is done depending on the nature of the score,

  * mae, rmse: smallest is best
  * Rpearson: highest is best
  * bias: smallest absolute value is best

  average: simply average of each score, ranking as above.

* FSS

  *  ordering of all models for each window/threshold (highest best) is done and ranks given accordingly
  *  NA are given a seperate rank, as well as FSS > skilful FSS or when FSS is 1
 
  average of FSS ranks: sum of the inverted FSS ranks done -> the bigger the better.
  This is to give models that perform in low thresholds/ great window sizes very good a little less weight.

  TODO: instead of the above approch, weight the FSS values according to thresholds / window sizes
  (this seems like a better aproach. Smaller window sizes and higher thresholds are more difficult to forecast and should therefore 
  given more credit)
  

## Good to know
window size is configurable from the 'definitions_X_data.R' (X=tp,sat), e.g.:
window_sizes   = c(10, 20, 30, 40, 60, 80, 100, 120, 140, 160, 180, 200)

The actual boxes have window size: 2*n+1,
with n being the amount of grid points (configurable in the variable window_sizes)

