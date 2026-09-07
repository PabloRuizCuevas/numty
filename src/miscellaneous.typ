#import "basics.typ": is-arr, is-mat
#import "matrices.typ": col, row

/// Create a equispaced numbers between a range
#let linspace(start, stop, num) = {
  // mimics numpy linspace
  let step = (stop - start) / (num - 1)
  range(0, num).map(v => start + v * step)
}

#let logspace(start, stop, num, base: 10) = {
  // mimics numpy logspace
  let step = (stop - start) / (num - 1)
  range(0, num).map(v => calc.pow(base, (start + v * step)))
}

/// Create a equispaced numbers between a range in a logarithmic scale
#let geomspace(start, stop, num) = {
  // mimics numpy geomspace
  let step = calc.pow(stop / start, 1 / (num - 1))
  range(0, num).map(v => start * calc.pow(step, v))
}

/// Internal function for printing mathematical expressions.
/// Formerly called `_p`.
///
/// -> content
#let _repr(expr) = {
  if is-mat(expr) {
    $mat(..expr)$
  } else if is-arr(expr) {
    $mat(..col(..expr))$
  } else {
    $expr$
  }
}

/// Prints mathematical expresions.
/// Experimental API, subject to change without deprecation.
///
/// -> content
#let print(..m, block: true) = math.equation(m.pos().map(_repr).join(), block: block)

/// Inline alias of print.
/// Experimental API, subject to change without deprecation.
///
/// -> content
#let p = print.with(block: false)
