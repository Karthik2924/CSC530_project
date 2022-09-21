# show an example of the topography of the solution space

motif = "ACGTACGTACGTACGTACGTACGT"

set.seed(100)

length = 250

seq1 = paste0(sample(c("A", "C", "G", "T"), size = length, replace = TRUE), 
              collapse = "")

seq2 = paste0(sample(c("A", "C", "G", "T"), size = length, replace = TRUE), 
              collapse = "")

substr(seq1, floor(length/2), (floor(length/2)+nchar(motif))) <- motif
substr(seq2, floor(length/2), (floor(length/2)+nchar(motif))) <- motif

# motifs are implanted
# now score all combinations of starts 
# max = nchar(seq1) - nchar(motif) + 1
L = nchar(seq1) - nchar(motif) + 1

# install.packages("DescTools")
library(DescTools)

distFun <- function(a, b) {
  return(DescTools::StrDist(a, b, method = "hamming")[1])
}

dists <- sapply(1:L, function(y) sapply(1:L, function(x)
  distFun(substr(seq1, y, y + nchar(motif) - 1),
          substr(seq2, x, x + nchar(motif) - 1))))

dists_long <- data.frame("dist" = c(dists))
# add columns for the seq1 and seq2 starting positions
dists_long$seq1_pos <- rep(1:L, each = L)
dists_long$seq2_pos <- rep(1:L, L)
dists_long <- dists_long[order(dists_long$seq1_pos, dists_long$seq2_pos), ]

x <- 1:L
y <- 1:L

# # this plot is ok
# rgl::persp3d(x = x,
#              y = y,
#              z = nchar(motif) - dists)

library(plot3D)

# this one is better
plot3D::persp3D(x = x,
                y = y,
                z = (nchar(motif) - dists),
                d = 1.5,
                zlab = "score",
                xlab = "starting position in seq1",
                ylab = "starting position in seq2",
                expand = 0.13,
                resfac = 0.5,
                rasterImage = TRUE,
                border = "black",
                lwd = 0.1,
                phi = 40, theta = 160)
                # shade = 0.2,
                # border = "black", box = TRUE

plot3D::persp3D(x = x,
                y = y,
                z = (nchar(motif) - dists),
                d = 1.5,
                zlab = "score",
                xlab = "starting position in seq1",
                ylab = "starting position in seq2",
                expand = 0.13,
                resfac = 0.75,
                rasterImage = TRUE,
                border = "black",
                lwd = 0.1,
                phi = 90, theta = 160)


# M <- plot3D::mesh(x, y)
# 
# plot3D::persp3D(x = x,
#                 y = y,
#                 z = nchar(motif) - dists,
#                 expand = 0.13,
#                 resfac = 0.5,
#                 rasterImage = TRUE, phi = 30, theta = 160)
# 
# 
# plot3D::persp3D(x = x,
#                 y = y,
#                 z = nchar(motif) - dists,
#                 expand = 0.25,
#                 rasterImage = TRUE, phi = 90)
# 
# plot3D::surf3D(x = M$x,
#                y = M$y,
#                z =  nchar(motif) - dists,
#                colvar =  nchar(motif) - dists,
#                phi = 20,
#                resfac = 1/20,
#                rasterImage = TRUE)
# 
# # install.packages("deldir")
# library(deldir)
# 
# # install.packages("GA")
# library(GA)
# # lets another way
# # https://stackoverflow.com/questions/48852014/how-to-achieve-smoothed-surfaces-from-points-in-grouped-data
# surf <- with(dists_long, deldir(seq1_pos, seq2_pos, 1 - dist)) 
# 
# dists_ <- nchar(motif) - dists
# 
# # https://luca-scr.github.io/GA/reference/persp3D.html
# GA::persp3D(x = x, y = y, z = dists_, expand = .33,
#             d = 30,
#             phi = 15, theta = 30, # nlevels = 8, #shade = 0,
#             col.palette = spectral.colors
#             )
# 
# GA::persp3D(x = x, y = y, z = dists_, expand = 0.5,
#             phi = 90, theta = 30, nlevels = 14, shade = 1.5,
#             col.palette = spectral.colors)
