#' Check files
#'
#' @description Internal utility function used to check that file names passed
#' as argument are valid.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param name.pattern character A vector of accepted file name patterns, used
#'   as pattern in a call to [`grepl()`] If `NULL` the test is skipped, if
#'   match fails, a warning is issued.
#'
#' @details Accepts a `character` vector or a list of `character` vectors,
#' returning always a `character` vector. The character strings are assumed to
#' be paths to files. If the files pointed at cannot be accessed, and error is
#' triggered. If the files exist, but one or more do not match the expected
#' `name.pattern` a warning is triggered.
#'
#' @return A `character` vector with one or more paths to files as members.
#'
#' @export
#'
check_files <- function(files,
                        name.pattern = NULL) {
  if (is.list(files)) {
    files <- unlist(files)
  }
  if (!is.character(files)) {
    stop("The argument passed to 'files' must be file names as character strings")
  }
  missing.files <- !file.exists(files)
  if (any(missing.files)) {
    stop("Cannot access ", sum(missing.files), " of the files: ",
         paste(files[missing.files], collapse = ", "), sep = "")
  }
  if (!is.null(name.pattern) && !all(grepl(name.pattern, basename(files)))) {
    warning("Unexpected file name(s): ",
            grep(name.pattern, basename(files), value = TRUE, invert = TRUE),
            "Call to the wrong function?")
  }
  files
}

#' Nearest 2D grid point
#'
#' @description Internal utility function used to find the nearest grid point
#'   in a 2D coordinate system based on Euclidean distance.
#'
#' @param x,y numeric vectors of equal length representing 2D coordinate pairs..
#' @param x.target,y.target numeric of length one giving the coordinates of the
#'   nearest neighbor search.
#' @param x.step,y.step numeric of length one equal or larger than the largest
#'   coordinate steps between grid points in `x` and `y`
#'
#' @details The values of `x.step` and `y.step` are used to constrain the
#'   computation of distances to the neighborhood of the target. This is
#'   to improve performance.
#'
#' @return A numeric index, assuming that `x` and `y` are coordinates
#'   variables of data stored in long form in the case of `nearest_point()`
#'   or the values of `x` and `y` at this grid point in the case of
#'   `nearest_pcoord()`.
#'
#' @export
#'
nearest_point <- function(x,
                          y,
                          x.target,
                          y.target,
                          x.step = Inf,
                          y.step = Inf) {
  # prefer largest if same distance
  x.target <- x.target * (1 + 1e-10)
  y.target <- y.target * (1 + 1e-10)
  # constrain x search
  if (is.infinite(x.step)) {
    x.range <- range(x)
  } else {
    x.range <- x.target + c(-1, +1) * x.step
  }
  x.idx <- which(x >= x.range[1] & x <= x.range[2])
  # constrain y search
  if (is.infinite(y.step)) {
    y.range <- range(y)
  } else {
    y.range <- y.target + c(-1, +1) * y.step
  }
  y.idx <- which(y >= y.range[1] & y <= y.range[2])
  xy.idx <- intersect(x.idx, y.idx)
  # compute distances
  distances <- sqrt((x[xy.idx]- x.target)^2 + (y[xy.idx] - y.target)^2)
  nearest.idx <- which((distances - min(distances)) < 1e-15)
  xy.idx[nearest.idx]
}

#' @rdname nearest_point
#'
#' @export
#'
nearest_pcoord <- function(x,
                           y,
                           x.target,
                           y.target,
                           x.step = Inf,
                           y.step = Inf) {
  idx <- nearest_point(x = x,
                       y = y,
                       x.target = x.target,
                       y.target = y.target,
                       x.step = x.step,
                       y.step = y.step)
  data.frame(x = x[idx], y = y[idx])
}

#' @rdname nearest_point
#'
#' @export
#'
dist2target <- function(x,
                        y,
                        x.target,
                        y.target,
                        x.step = Inf,
                        y.step = Inf) {
  # constrain x search
  x.range <- x.target + c(-1, +1) * x.step
  x <- x[x >= x.range[1] & x <= x.range[2]]
  # constrain y search
  y.range <- y.target + c(-1, +1) * y.step
  y <- y[y >= y.range[1] & y <= y.range[2]]
  # compute distances
  sqrt((x- x.target)^2 + (y- y.target)^2)
}

#' Nearest 2D grid region
#'
#' @description Internal utility functions used to find indexes to rectangular
#'   grid regions based on Euclidean distance.
#'
#' @param x,y numeric vectors of equal length representing 2D coordinate pairs..
#' @param x.target,y.target numeric of length two giving the coordinates of the
#'   corners of the target region.
#' @param x.step,y.step numeric of length one equal or larger than the largest
#'   coordinate steps between grid points in `x` and `y`.
#' @param envelope character One of "nearest", "outer" or "inner".
#'
#' @details The values of `x.step` and `y.step` are used to constrain the
#'   computation of distances to the neighbourhood of the target. This is
#'   to improve performance. The default `envelope = "outer"` returns the
#'   coordinates of the smallest enclosing region.
#'
#' @note For `"inner"` and `"outer"` envelopes the region is shrunk or expanded
#'   by half a step. If the `step` is `Inf`, the largest steps found in `x` and
#'   `y` replace `Inf`.
#'
#' @return A vector of indices, assuming that `x` and `y` are coordinates
#'   variables of data stored in long form.
#'
#' @export
#'
nearest_corners <- function(x,
                            y,
                            x.target,
                            y.target,
                            x.step = Inf,
                            y.step = Inf,
                            envelope = "nearest") {
  if (envelope != "nearest") {
    if (x.step < Inf) {
      x.target.delta <- x.step / 2
    } else {
      x.target.delta <- max(diff(x)) / 2
    }
    if (y.step < Inf) {
      y.target.delta <- y.step / 2
    } else {
      y.target.delta <- max(diff(y)) / 2
    }
    if (envelope == "inner") {
      x.target.delta <- -x.target.delta
      y.target.delta <- -y.target.delta
    }
  } else {
    x.target.delta <- 0
    y.target.delta <- 0
  }
  # if shrinking would result in empty region return nearest point
  lf.x.target <- min(x.target - x.target.delta)
  ur.x.target <- max(x.target + x.target.delta)
  if (ur.x.target < lf.x.target) {
    lf.x.target <- ur.x.target <- (lf.x.target + ur.x.target) /2
  }
  lf.y.target <- min(y.target - y.target.delta)
  ur.y.target <- max(y.target + y.target.delta)
  if (ur.y.target < lf.y.target) {
    lf.y.target <- ur.y.target <- (lf.y.target + ur.y.target) /2
  }

  lower.left <-
    nearest_point(x = x,
                  y = y,
                  x.target = lf.x.target,
                  y.target = lf.y.target,
                  x.step = x.step,
                  y.step = y.step)

  upper.right <-
    nearest_point(x = x,
                  y = y,
                  x.target = ur.x.target,
                  y.target = ur.y.target,
                  x.step = x.step,
                  y.step = y.step)
  data.frame(lower.left = lower.left, upper.right = upper.right)
}

#' @rdname nearest_corners
#'
#' @export
#'
nearest_region <- function(x,
                           y,
                           x.target,
                           y.target,
                           x.step = Inf,
                           y.step = Inf,
                           envelope = "nearest") {
  corners <- nearest_corners(x = x,
                             y = y,
                             x.target = x.target,
                             y.target = y.target,
                             x.step = x.step,
                             y.step = y.step,
                             envelope = envelope)

  unique(which(x >= x[corners[["lower.left"]] ] & x <= x[corners[["upper.right"]] ] &
          y >= y[corners[["lower.left"]] ] & y <= y[corners[["upper.right"]] ]))
}

#' @rdname nearest_corners
#'
#' @export
#'
nearest_rcoord <- function(x,
                           y,
                           x.target,
                           y.target,
                           x.step = Inf,
                           y.step = Inf,
                           envelope = "nearest") {
  selector <- nearest_region(x = x,
                             y = y,
                             x.target = x.target,
                             y.target = y.target,
                             x.step = x.step,
                             y.step = y.step,
                             envelope = envelope)

   data.frame(x = x[selector], y = y[selector])

}

#' @rdname nearest_corners
#'
#' @export
#'
nearest_ccoord <- function(x,
                           y,
                           x.target,
                           y.target,
                           x.step = Inf,
                           y.step = Inf,
                           envelope = "nearest") {
  selector <- nearest_corners(x = x,
                             y = y,
                             x.target = x.target,
                             y.target = y.target,
                             x.step = x.step,
                             y.step = y.step,
                             envelope = envelope)

  data.frame(x = x[selector], y = y[selector], row.names = names(selector))

}

