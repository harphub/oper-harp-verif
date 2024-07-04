
# quick plot
# # GeoSphere plotting scheme
# prec_breaks = c(0, 0.1, 0.2, 0.5, 1., 5., 10., 15,
#            20., 25., 30., 35., 40., 45., 50., 100)
# prec_palette = c("#CFCCC8","#00FE96","#00FEC8","#00FEFE",  ## "#FFFFFE"
#             "#00C8FE","#0096FE","#0032FE","#3200FE",
#             "#6400FE","#9600FE","#C800FE","#FA00FE",
#             "#C800C8", "#960096","#FF0000")

# DMI plotting scheme
# breaks = c(0, 0., 1, 3, 5, 7, 10., 15.,
#            20, 25, 30, 40, 50, 60, 70, 80, 100)
# palette = c("#e5ebec", "#c7e5fb","#8cc7f2", "#45a6eb", "#1c73b2",
#                  "#4d991b", "#71ce9c", "#b4db72", "#f5f305",
#                  "#f6d125", "#f6a625", "#f54125", "#ae092f",
#                  "#d59de5", "#9c04c6", "#23052b")
library(RColorBrewer)

breaks = seq(210, 280, 10)
palette   = brewer.pal(length(breaks)-1, "Greys")


#### position of score boxes ###
# ## FSS #
# ## c(x0, x1, y0, y1) in decimals
ll_fss_box   <- c(0.0, 0.24, 0.56, 0.95)

# ## basic scores #
# ## c(x0, x1, y0, y1) in decimals
ll_score_box <- c(0.0, 0.24, 0.31, 0.51) # below fss box
# ll_score_box <- c(0.25, 0.47, 0.75, 0.95)  # right to fss box

