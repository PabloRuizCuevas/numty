#import "basics.typ": op

#let add(..args) = op(..args, (a, b) => a + b)
#let sub(..args) = op(..args, (a, b) => a - b)
#let mult(..args) = op(..args, (a, b) => a * b)
#let div(..args) = op(..args, (a, b) => if (b != 0) { a / b } else { float.nan })

// == std.calc ==

#let abs(value) = op(value, calc.abs)
#let pow(base, exponent) = op(
  if type(base) == decimal and type(exponent) != int { float(base) } else { base },
  exponent,
  calc.pow,
)
#let exp(value) = op(value, calc.exp)
#let sqrt(value) = op(value, value => if value >= 0 { calc.sqrt(value) } else { float.nan })

// `root` requires `odd`

#let sin(angle) = op(angle, calc.sin)
#let cos(angle) = op(angle, calc.cos)
#let tan(angle) = op(angle, calc.tan)
#let asin(value) = op(value, calc.asin)
#let acos(value) = op(value, calc.acos)
#let atan(value) = op(value, calc.atan)
#let atan2(x, y) = op(x, y, calc.atan2)

#let sinh(value) = op(value, calc.sinh)
#let cosh(value) = op(value, calc.cosh)
#let tanh(value) = op(value, calc.tanh)
#let asinh(value) = op(value, calc.asinh)
#let acosh(value) = op(value, calc.acosh)
#let atanh(value) = op(value, v => if -1 < v and v < 1 { calc.atanh(v) } else { float.nan })

#let log(value, base: 10) = op(value, j => if (j > 0) and (base > 0) { calc.log(j, base: base) } else { float.nan })
#let ln(value) = op(value, v => if (v > 0) { calc.ln(v) } else { float.nan })
#let erf(value) = op(value, calc.erf)

#let fact(number) = op(number, n => if n >= 0 { calc.fact(n) } else { -1 })
#let perm(base, numbers) = op(base, numbers, (b, n) => if b >= 0 and n >= 0 { calc.perm(b, n) } else { -1 })
#let binom(n, k) = op(n, k, (n, k) => if n >= 0 and k >= 0 { calc.binom(n, k) } else { -1 })

#let gcd(..args) = op(..args, calc.gcd)
#let lcm(..args) = op(..args, calc.lcm)

#let floor(value) = op(value, calc.floor)
#let ceil(value) = op(value, calc.ceil)
#let trunc(value) = op(value, calc.trunc)
#let fract(value) = op(value, calc.fract)
#let round(a, digits: 0) = op(a, calc.round.with(digits: digits))
#let clamp(value, min, max) = op(value, v => calc.clamp(v, min, max))

#let min(..args) = op(..args, calc.min)
#let max(..args) = op(..args, calc.max)

#let even(n) = op(n, calc.even)
#let odd(n) = op(n, calc.odd)

#let quo(dividend, divisor) = op(dividend, divisor, calc.quo)
#let rem(dividend, divisor) = op(dividend, divisor, calc.rem)
#let div-euclid(u, v) = op(u, v, calc.div-euclid)
#let rem-euclid(u, v) = op(u, v, calc.rem-euclid)

// redefinition under requirements

#let root(radicant, index) = op(
  radicant,
  index,
  (r, i) => if r >= 0 or odd(i) { calc.root(r, i) } else { float.nan }
)

// == convenience functions ==

/// Binary logarithm.
#let lb(value) = log(value, base: 2)
