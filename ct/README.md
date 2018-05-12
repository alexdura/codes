# Codes
Generators for linear codes and Reed-Solomon codes
This project also includes some nice extensions to the types defined in the **numeric-prelude** Haskell package: the field with p elements and the field with p^n elements

## Modules
### PrimeField
* type constructor: PrimeField.T p
* value constructor: PrimeField.e :: Integer -> (PrimeField.T p)
* known as Fp

### GaloisField
* type constructor: GaloisField.T p n
* value constructor: GaloisField.fromPolynomial p
* known as GF(p^n)
* represented as Fp[X]/P(X) where P(X) is some irreducible polynomial of degree n; this polynomial is chosen such that all its coefficients are maximal with respect to the lexicographic order in Fp
* **TODO** look for faster methods of generating irreducible polynomials of a given degree - perhaps hard for the general case, but there may be some speed-up if the degree is of a given form

### LinearCode
* LinearCode.check and LinearCode.generator - build the check and generator matrices for a linear [n, k, d] code over some prime field Fp
* the functions may fail if there is no such code with the given requirements
* **TODO** look for faster ways of generating the check matrix

### RSCode
* RSCode.check and RSCode.generator - build the check and generator matrices for a Reed-Solomon code [k, p^n] over of GF(p^n); k must be less than p^n
