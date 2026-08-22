#import "basics.typ": shape
#import "operations.typ": abs, div, div-euclid, mult, pow, sub

// == vector ==

/// Transposes matrix or vector.
///
/// ```example
/// #nt.transpose(((1, 5), (1, 4)))
/// ```
///
/// -> array
#let transpose(m) = array.zip(..m)

/// Alias of `transpose`.
#let t = transpose

/// Creates row vector.
///
/// ```example
/// #nt.row(1, 1,)
/// #nt.shape(nt.row(1, 1,))
/// ```
///
/// -> array
#let row(..v) = (v.pos(),)

/// Alias of `row`.
#let r = row

/// Creates column vector.
///
/// ```example
/// #nt.col(1, 1,)
/// #nt.shape(nt.col(1, 1,))
/// ```
///
/// -> array
#let col(..v) = transpose(row(..v))

/// Alias of `col`.
#let c = col

/// Dot product of two vectors
///
/// -> float
#let dot(a, b) = mult(a, b).sum()

/// normalize a vector, defaults to L2 normalization
#let normalize(v, p: 2.0) = div(v, calc.norm(p: p, ..v))

#let roll(v, n) = v.slice(-n) + v.slice(0, -n)

// == matrix ==

#let vstack(..rows) = rows.pos().reduce((a, b) => a + b)
#let hstack(..cols) = transpose(vstack(..cols.map(transpose)))

/// Trace of a matrix.
///
/// ```example
/// #nt.trace(((1, 2), (3, 4)))
/// #nt.trace(())
/// ```
///
/// -> float
#let trace(m) = range(m.len()).map(i => m.at(i).at(i)).sum(default: 0)

/// Matrix multiplication
///
/// -> array
#let matmul(a, b) = a.map(a_row => transpose(b).map(b_col => dot(a_row, b_col)))

/// Matrix determinant
///
/// -> float
#let det(m) = {
  let n = m.len()
  // assert(n > 0, "cannot take determinant of empty matrix!")

  if shape(m) == (2, 2) {
    let ((a, b), (c, d)) = m
    return a * d - b * c
  }

  /// using https://en.wikipedia.org/wiki/Bareiss_algorithm

  // let pivot(x, y, z, w, p) = div-euclid(sub(mul(x, z), mul(w, y)), p)

  let sign = 1
  let (prev, curr) = (1, 1)

  for i in range(n) {
    for j in range(i, n) {
      if m.at(j).at(i) != 0 and j != i {
        (m.at(i), m.at(j)) = (m.at(j), m.at(i))
        sign = -sign
        break
      }
    }

    if m.at(i).at(i) == 0 { return 0 }

    (prev, curr) = (curr, m.at(i).at(i))
    m = for (j, row) in m.enumerate() {
      (
        for (x, y) in row.zip(m.at(i)) {
          (div-euclid(sub(mult(curr, x), mult(row.at(i), y)), prev),)
        },
      )
    }
  }
  mult(sign, curr)
}

/// Returns matrix with ones on the k-th diagonal. Mimics `numpy.eye`.
///
/// ```example
/// #nt.eye(2)
///
/// #nt.eye(3, k: 1)
/// ```
///
/// -> array
#let eye(
  /// Number of rows.
  ///
  /// -> int
  n,
  /// Index of the diagonal to fill with ones. 0 refers to the main diagonal,
  /// a positive value refers to an upper diagonal, and a negative value refers
  /// to a lower diagonal.
  ///
  /// -> int
  k: 0,
) = range(n).map(i => range(n).map(j => if (j == i + k) { 1 } else { 0 }))

/// Create a (square) identity matrix with dimensions $n times n$
///
/// -> array
#let identity = eye.with(k: 0)

/// Calculates the inverse matrix of any size. May require manual rounding to avoid floating point errors. \
/// *Note*: On uninversible matrix ($"det"("matrix") = 0$) returns undefined values, does not error out.
///
/// - matrix (matrix): The matrix to inverse.
/// -> matrix
#let inverse(matrix) = {
  let size = shape(matrix)
  assert(size.len() == 2 and size.first() == size.last(), message: "matrix must be square to perform inversion!")

  let n = size.first()
  let retmat = eye(n)

  for j in range(n) {
    for i in range(j, n) {
      if matrix.at(i).at(j) == 0 { continue }

      // 1. Properly swap rows
      (matrix.at(i), matrix.at(j)) = (matrix.at(j), matrix.at(i))
      (retmat.at(i), retmat.at(j)) = (retmat.at(j), retmat.at(i))

      // Normalize the pivot row
      let p = matrix.at(j).at(j)
      matrix.at(j) = div(matrix.at(j), p)
      retmat.at(j) = div(retmat.at(j), p)

      // Eliminate the column in other rows
      for k in range(n) {
        if k == j { continue }

        p = -matrix.at(k).at(j)
        for l in range(n) {
          matrix.at(k).at(l) += p * matrix.at(j).at(l)
          retmat.at(k).at(l) += p * retmat.at(j).at(l)
        }
      }

      // 2. CRITICAL: Stop searching for a pivot in this column!
      break
    }
  }

  return retmat
}

#let cross-3(a, b) = {
  let (ax, ay, az) = a
  let (bx, by, bz) = b

  return (
    ay * bz - az * by,
    az * bx - ax * bz,
    ax * by - ay * bx,
  )
}

#let _cross-7-triples = range(7).map(i => (i, calc.rem(i + 1, 7), calc.rem(i + 3, 7)))

#let cross-7(a, b) = {
  let result = (0,) * 7

  for (i, j, k) in _cross-7-triples {
    result.at(i) += a.at(j) * b.at(k) - a.at(k) * b.at(j)
    result.at(j) += a.at(k) * b.at(i) - a.at(i) * b.at(k)
    result.at(k) += a.at(i) * b.at(j) - a.at(j) * b.at(i)
  }

  result
}

/// Cross product of two 3D or 7D vectors.
///
/// ```example
/// #nt.cross((1, 2, 3), (4, 5, 6))
/// ```
///
/// -> array
#let cross(a, b) = if shape(a) == (3,) and shape(b) == (3,) {
  cross-3(a, b)
} else if shape(a) == (7,) and shape(b) == (7,) {
  cross-7(a, b)
} else {
  panic("cross product is only defined for 3D or 7D vectors")
}
