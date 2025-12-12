// == types ==

#let arrarr(a,b) = (type(a) == array and type(b) == array)
#let arrflt(a,b) = (type(a) == array and type(b) != array)
#let fltarr(a,b) = (type(a) != array and type(b) == array)
#let fltflt(a,b) = (type(a) != array and type(b) != array)
#let is-arr(a) = (type(a) == array)
#let is-mat(m)  = (is-arr(m) and is-arr(m.at(0)))
#let matmat(m,n) = is-mat(m) and is-mat(n)
#let matflt(m,n) = is-mat(m) and type(n) != array
#let fltmat(m,n) = is-mat(n) and type(m) != array

/// Checks if is string
/// -> bool
#let is-str(s) = (type(a) == str)

/// Checks if is a 1d array
/// -> bool
#let is-1d-arr(arr) ={
  if is-arr(arr){if is-arr(arr.at(0)) {false} else {true}} else {false}
}

/// Checks if is a 1d array array or matrix
/// -> bool
#let is-1d(arr) ={
  if is-arr(arr){ // arrays or mats
    if is-arr(arr.at(0)) and arr.at(0).len() > 1 { 
      if arr.len() == 1 {true} else {false}  // row mat else full mat
    } 
    else {true} // col mat
  } 
  else {false} // no array
}

// == reshapers ==

/// Creates row vector
#let r(..v) = {
  (v.pos(),)
}

/// Creates column vector
#let c(..v) = {
  v.pos().map(r => (r,),) 
}

/// Transposes matrx and vectors
#let transpose(m) = {
    // Get dimensions of the matrix
    let rows = m.len()
    let cols = m.at(0).len()
    range(0, cols).map(c => range(0, rows).map(r => m.at(r).at(c)))
  }

/// Alias of transpose
#let t(m) = transpose(m) 

// == boolean ==

/// Check if is na
#let isna(v) = {
  if is-arr(v){
    v.map(i => if (type(i)==float){i.is-nan()} else {false})
  }
  else{
    if (type(v)==float){v.is-nan()} else {false} 
  }
}

/// Check if all values are true / 1 
/// -> bool
#let all(v) ={
  if is-arr(v){
    v.flatten().all(a => a == true or a ==1)
  }
  else{
    v == true or v == 1
  }
}

/// Generic application of operator to a, b
/// where a b can be matrices vectors or numbers of any shape
/// and fun is typically a operator between them
#let op(a,b, fun) ={
  // generic operator with broacasting
  if matmat(a,b) {
    a.zip(b).map(((a,b)) => op(a,b, fun))
  }
  else if matflt(a,b){ // supports n-dim matrices
    a.map(v=> op(v,b, fun))
  }
  else if fltmat(a,b){
    b.map(v=> op(a,v, fun))
  }
  else if arrarr(a,b) {
    a.zip(b).map(((i,j)) => fun(i,j))
  }
  else if arrflt(a,b) {
    a.map(a => fun(a,b))
  }
  else if fltarr(a,b) {
    b.map(i => fun(a,i))
  }
  else {
    fun(a,b)
  }
}

/// internall equality operator
#let _eq(i,j, equal-nan) ={
  i==j or (all(isna((i,j))) and equal-nan)
}

/// Check for equality
#let eq(u,v, equal-nan: false) = {
  // Checks for equality element wise
  // eq((1,2,3), (1,2,3)) = (true, true, true)
  // eq((1,2,3), 1) = (true, false, false)
  let _eqf(i,j)={_eq(i,j, equal-nan)}
  op(u,v, _eqf)
}

/// Returns true if any value in a array / matrix is true or 1
/// -> bool
#let any(v) ={
  // check if any item is true after iterating everything
  if is-arr(v){
    v.flatten().any(a => a == true or a ==1)
  }
  else{
    v == true or v == 1
  }
}

/// Check if all values are equal
/// -> bool
#let all-eq(u,v) = all(eq(u,v))

/// Applies  function to an array
#let apply(a, fun) = {
  // vectorize
  // consider returning a function of a instead?
  if is-arr(a){ //recursion exploted for n-dim
    a.map(v=>apply(v, fun))
  }
  else{
    fun(a)
  }
} 

/// Absolute value of a number/array/matrix
#let abs(a)= apply(a, calc.abs)

// == Operators ==

#let _add(a,b)=(a + b)
#let _sub(a,b)=(a - b)
#let _mul(a,b)=(a * b)
#let _div(a,b)= if (b!=0) {a/b} else {float.nan}

/// Addition of a number/array/matrix with broadcasting
#let add(u,v) = op(u,v, _add)

/// Substraction of a number/array/matrix with broadcasting
#let sub(u, v) = op(u,v, _sub)

/// Multiplication of a number/array/matrix with broadcasting
#let mult(u, v) = op(u,v, _mul)

/// Division of a number/array/matrix with broadcasting
#let div(u, v) = op(u,v, _div)

/// Exponenciation of a number/array/matrix with broadcasting
#let pow(u, v) = op(u,v, calc.pow)

// == vectorial ==

/// normalization of a vector
#let normalize(a, l:2) = { 
  // normalize a vector, defaults to L2 normalization
  let aux = pow(pow(abs(a),l).sum(),1/l)
  a.map(b => b/aux)
} 

// dot product

/// Dot product of two vectors
#let dot(a,b) = mult(a,b).sum()

// == Algebra, trigonometry ==

/// Sin function of a number/array/matrix
#let sin(a) = apply(a,calc.sin)

/// Cos function of a number/array/matrix
#let cos(a) = apply(a,calc.cos)

/// Tan function of a number/array/matrix
#let tan(a) = apply(a,calc.tan)

/// Log function of a number/array/matrix
#let log(a) = apply(a, j => if (j>0) {calc.log(j)} else {float.nan} )

// matrix

/// Matrix multiplication
#let matmul(a,b) = {
  let bt = transpose(b)
  a.map(a_row => bt.map(b_col => dot(a_row,b_col)))
}



/// Matrix determinant
#let det(m) = {
  let n = m.len()
  if n == 0 {
    panic("cannot take determinant of empty matrix!")
  }

  if m.len() == 2 and m.at(0).len() == 2 {
    return m.at(0).at(0) * m.at(1).at(1) - m.at(1).at(0) * m.at(0).at(1)
  }

  /// using https://en.wikipedia.org/wiki/Bareiss_algorithm

  let sign = 1
  let prev = 1

  for i in range(n - 1) {
    if m.at(i).at(i) == 0 { // swap with another row having nonzero i's elem
      let swap-found = false
      for j in range(i + 1, n) {
        if m.at(j).at(i) != 0 {
          (m.at(i), m.at(j)) = (m.at(j), m.at(i))
          sign = -sign
          swap-found = true
          break
        }
      }
      if not swap-found {
        return 0 // entire column is zero, det must be zero
      }
    }
    for j in range(i + 1, n) {
      for k in range(i + 1, n) {
        m.at(j).at(k) = (m.at(i).at(i) * m.at(j).at(k) - m.at(j).at(i) * m.at(i).at(k)) / prev
      }
    }
    prev = m.at(i).at(i)
  }

  sign * m.at(n - 1).at(n - 1)
}

/// Trace of a matrix
#let trace(m) ={
  m.enumerate().map( ((i,_ )) => m.at(i).at(i)).sum()
}

// others:

/// Create a equispaced numbers between a range
#let linspace = (start, stop, num) => {
  // mimics numpy linspace
  let step = (stop - start) / (num - 1)
  range(0, num).map(v => start + v * step)
}

#let logspace = (start, stop, num, base: 10) => {
  // mimics numpy logspace
  let step = (stop - start) / (num - 1)
  range(0, num).map(v => calc.pow(base, (start + v * step)))
}

/// Create a equispaced numbers between a range in a logarithmic scale
#let geomspace = (start, stop, num) => {
  // mimics numpy geomspace
  let step = calc.pow( stop / start, 1 / (num - 1))
  range(0, num).map(v => start * calc.pow(step,v))
}

#let to-str(a) = {
  if (type(a) == bool){
    if(a){
      "value1"
    } 
    else {
      "value2"
    } 
  } 
  else{
    str(a)
  }
}


#let _p(m) = {
  if is-mat(m) {
   "mat(" + m.map(v => v.map(j=>to-str(j)).join(",")).join(";")+ ")"
  }
  else if is-arr(m){
    "vec(" + m.map(v => str(v)).join(",")+ ")"
  }
  else if is-arr(m){
    is-str(m)
  }
  else{
   str(m)
  }
}

// print mathematical expresions
#let print(..m) = {
  let scope = (value1: "true", value2: "false")
  eval("$ " + m.pos().map(r => _p(r)).join(" ") + " $", scope: scope)
}

// alis of print
#let p(..m) = {
  let scope = (value1: "true", value2: "false")
  eval("$" + m.pos().map(r => _p(r)).join(" ") + "$", scope: scope)
}


