# numty
Numeric Typst

Mathematical functions to operate vectors / arrays in typst:

```typ
#let a = (1,2,3)
#let b = 2

#vmult(a,b)  => (2,4,6)
#vsum(a,a)  => (2,4,6)
#vsum(2,a)  => (3,4,5)
#vdot(a,a)  => 11
```

Supported functions

vpow -> exponentation

vmult -> multiplication

vdiv -> division

vsum -> sumation

vnorm -> normalization of a vector

vdot -> dot product
