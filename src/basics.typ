
#let is-arr(a) = (type(a) == array)
#let is-flt(a) = (type(a) == float)
#let is-int(a) = (type(a) == int)
#let is-str(a) = (type(a) == str)

/// Return the shape of an array.
///
/// ```example
/// #nt.shape(nt.eye(3))
/// #nt.shape(((1, 3),))
/// #nt.shape(nt.c(1, 3))
/// #nt.shape((1, 2, 3,))
/// #nt.shape(5)
/// ```
///
/// -> array
#let shape(a) = if is-arr(a) { (a.len(),) + shape(a.at(0)) } else { () }

#let dim = shape

/// Shapes an array with its data unchanged.
///
/// ```example
/// #nt.reshape(range(6), (3, 2))
///
/// #nt.reshape(((0, 1, 2), (3, 4, 5)), 6)
/// ```
///
/// -> array
#let reshape(arr, new-shape, exact: true) = {
  arr = arr.flatten()
  while not is-int(new-shape) and new-shape.len() != 1 {
    arr = arr.chunks(new-shape.pop(), exact: exact)
  }
  if not exact { return arr }

  let last-size = if is-arr(new-shape) { new-shape.at(0) } else { new-shape }
  return arr.slice(0, last-size)
}

/// Checks if is a 1D row array.
///
/// ```example
/// #nt.is-vec((1, 2, 3, 4, 5))
/// #nt.is-vec(((1, 5), (1, 4)))
/// ```
///
/// -> bool
#let is-vec(a) = (shape(a).len() == 1)

/// Alias of `is-vec`.
#let is-1d-arr = is-vec

#let is-mat(a) = (shape(a).len() > 1)

/// Checks if is a 1D array.
///
/// ```example
/// #nt.is-1d(nt.eye(3))
/// #nt.is-1d(((1, 3),))
/// #nt.is-1d(nt.c(1, 3))
/// #nt.is-1d((1, 2, 3,))
/// #nt.is-1d(5)
/// ```
///
/// -> bool
#let is-1d(arr) = (shape(arr).filter(x => x > 1).len() <= 1)

/// Check if all values are `true` or `1`.
///
/// ```example
/// #nt.all(((true, true), (true, true)))
/// #nt.all((false, true, false))
/// #nt.all(1)
/// ```
///
/// -> bool
#let all(a) = (a == true or a == 1 or is-arr(a) and a.all(x => all(x)))

/// Returns true if any value in a array / matrix is true or 1
///
/// -> bool
#let any(x) = (x == true or x == 1 or is-arr(x) and x.any(any))

/// Applies unary function to an array
///
/// -> any
#let _apply(a, func) = if is-arr(a) { a.map(v => _apply(v, func)) } else { func(a) }

#let _broadcast(a, b, func) = {
  let is-scalar(a) = shape(a).len() == 0 or shape(a).at(0) == 1
  let as-scalar(a) = if is-arr(a) { a.at(0) } else { a }

  let sa = shape(a)
  let sb = shape(b)

  if sa == () and sb == () {
    return func(as-scalar(a), as-scalar(b))
  } else if is-scalar(a) {
    return b.map(bi => _broadcast(as-scalar(a), bi, func))
  } else if is-scalar(b) {
    return a.map(ai => _broadcast(ai, as-scalar(b), func))
  } else if sa.at(0) == sb.at(0) {
    return a.zip(b).map(((ai, bi)) => _broadcast(ai, bi, func))
  }
}

#let op(..args, func) = {
  if args.pos().len() == 1 {
    _apply(..args, func)
  } else {
    args.pos().reduce((acc, x) => _broadcast(acc, x, func))
  }
}

/// Check if is NaN.
///
/// Returns True where `x` is NaN, false otherwise.
/// Returns a scalar if `x` is a scalar.
///
/// ```example
/// #nt.is-nan(float.nan)
/// ```
///
/// -> array | bool
#let is-nan(a) = op(a, float.is-nan)

/// Alias of `is-nan`.
#let isna = is-nan

/// Internal equality operator.
///
/// -> bool
#let _eq(i, j, equal-nan) = (i == j or all(is-nan((i, j))) and equal-nan)

/// Checks for equality element wise.
///
/// -> bool
#let eq(u, v, equal-nan: false) = op(u, v, ((ui, vi) => _eq(ui, vi, equal-nan)))

/// Check if all values are equal
/// -> bool
#let all-eq(u, v) = all(eq(u, v))
