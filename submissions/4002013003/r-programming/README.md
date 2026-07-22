# Programming with R — K-means Clustering from Scratch

**Student ID:** 4002013003
**File:** [`km.R`](./km.R) · sample output: [`example_plot.png`](./example_plot.png)

## Goal
A mobile-services company wants to open `k` branch offices across a city.
Given the coordinates (latitude, longitude) of `n` customer areas, choose the
office locations that minimise the total distance to customers — i.e. run the
**k-means** algorithm, implemented by hand with base R only (no `kmeans()`, no
third-party packages).

## The `km(x, k)` function

**Input**
* `x` — an `n x 2` matrix: column 1 = latitude, column 2 = longitude
* `k` — number of offices (clusters)

**Algorithm**
1. Randomly pick `k` of the points as the starting centres (`sample`), stored in
   a `k x 2` `centers` matrix.
2. Define a helper `dis(point, centers)` returning the Euclidean distance
   `sqrt((x1 - xc)^2 + (x2 - yc)^2)` from one point to every centre.
3. Assign each point to its nearest centre → the `clust` vector (values `1..k`).
4. Recompute each centre as the **mean latitude / mean longitude** of the points
   assigned to it.
5. Repeat steps 2–4 until the assignment `clust` no longer changes (compared with
   the previous `clust0`). The number of passes is counted in `itr`.

**Output** — a list:
| Field | Meaning |
| :-- | :-- |
| `centers` | `k x 2` optimal office coordinates |
| `size` | number of customer points each office covers |
| `clust` | nearest office (`1..k`) for every point |
| `clustdata` | list of the point coordinates grouped per office |
| `itr` | iterations until convergence |

It also draws the **"office location"** plot: each cluster in its own colour,
the office centres drawn as larger dots, a segment from every point to its
office, `latitude` on the x-axis and `longitude` on the y-axis, with
`N=` and `k=` shown as the subtitle.

## Sample run (verified with R 4.3.2)
Using the example data from the assignment sheet:
```r
x <- cbind(matrix(rnorm(50, mean = 33.8000, sd = 0.05), ncol = 1),
           matrix(rnorm(50, mean = 46.4333, sd = 0.05), ncol = 1))
km(x, 3)
```
A representative run converged in **6 iterations** with cluster sizes summing to
all 50 points. The resulting figure is saved in `example_plot.png`.

> Note: `rnorm` in the demo is not seeded (as on the assignment sheet), so the
> exact centres, sizes and iteration count vary from run to run — the algorithm
> and the shape of the plot stay the same.

## How to run
```r
Rscript km.R
```
