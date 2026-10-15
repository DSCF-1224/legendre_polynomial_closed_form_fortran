# legendre_polynomial_closed_form_fortran

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![Actions Status](https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/actions)
[![Documentation](https://img.shields.io/badge/docs-ford-blue)](https://dscf-1224.github.io/legendre_polynomial_closed_form_fortran/)

Legendre polynomial evaluation for Fortran, via the closed-form expression.

This library evaluates the Legendre polynomials $P_n(x)$ directly from their closed-form
coefficients. Each coefficient is an exact integer over a common denominator of $2^n$, computed
via Python's [`math.comb`][math-comb] when the `.fypp` source is expanded to Fortran by
[Fypp][fypp]. This happens once, at generation time; the published [`default`][branch-default]/
[`with_real128`][branch-with-real128] branches contain only the generated `.f90`, so consumers
never need Python themselves. Evaluation itself uses Horner's method.

The first derivatives $P_n'(x)$ are evaluated in the same way, from the closed form obtained by
differentiating that expression term by term (see [Derivatives](#derivatives)).

[math-comb]: https://docs.python.org/3/library/math.html#math.comb
[fypp]: https://github.com/aradi/fypp
[branch-default]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/tree/default
[branch-with-real128]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/tree/with_real128

## Supported degrees

This library requires that every integer numerator involved be exactly representable, without
loss of precision, in the mantissa of the real kind it's evaluated in. The supported degree
range for each kind follows from that requirement:

| Real kind  | Degree range | Notes                                        |
| ---------- | ------------ | --------------------------------------------- |
| `real32`   | 0 – 13       |                                                |
| `real64`   | 0 – 27       |                                                |
| `real128`  | 0 – 30       | only in the `with_real128` branch (see below) |

`real128` support — and with it, degrees 28–30 — is available only in the `with_real128`
branch. `real32` and `real64` have the same degree range in either branch. Each `realXX`
range also applies to `complex(realXX)`: the coefficients are shared, so the degree limit
depends only on the kind, not on whether the argument is real or complex.

The same requirement applies to the integer numerators of $P_n'(x)$, so the supported degrees
of the derivative differ from the ones above:

| Real kind  | Degree range | Notes                                        |
| ---------- | ------------ | --------------------------------------------- |
| `real32`   | 0 – 12       |                                                |
| `real64`   | 0 – 25       |                                                |
| `real128`  | 0 – 28       | only in the `with_real128` branch (see above) |

## Usage

Each degree has its own generic interface, resolved at compile time:

```fortran
use legendre_polynomial_closed_form_fortran, only: p_05

real(real64) :: x, y

x = 0.5_real64
y = p_05(x)   ! P_5(0.5)
```

For a degree chosen at run time, `p_n(degree, x)` selects the matching `p_NN` via a `select
case` on `degree`. If `degree` is negative, or exceeds the maximum degree available for `x`'s
kind, it returns a signaling NaN rather than failing to compile or run:

```fortran
use legendre_polynomial_closed_form_fortran, only: p_n

real(real64) :: x, y
integer :: degree

degree = 5
x = 0.5_real64
y = p_n(degree, x)   ! same value as p_05(x)
```

Both `p_NN` and `p_n` also accept `complex(realXX)`, resolved via the same generic interfaces:

```fortran
use legendre_polynomial_closed_form_fortran, only: p_05

complex(real64) :: z, w

z = cmplx(0.3_real64, 0.4_real64, real64)
w = p_05(z)   ! P_5(0.3 + 0.4i)
```

## Derivatives

The derivative has the same interface structure as the polynomial itself. `p_NN_prime`
evaluates $P_n'(x)$ for the degree `NN`, and `p_n_prime(degree, x)` selects the matching
`p_NN_prime` for a degree chosen at run time:

```fortran
use legendre_polynomial_closed_form_fortran, only: p_05_prime, p_n_prime

real(real64) :: x, y
integer :: degree

degree = 5
x = 0.5_real64
y = p_05_prime(x)         ! P_5'(0.5)
y = p_n_prime(degree, x)  ! same value as p_05_prime(x)
```

If `degree` is negative, or exceeds the maximum degree available for `x`'s kind (see
[Supported degrees](#supported-degrees)), `p_n_prime` returns a signaling NaN, as `p_n` does.
`complex(realXX)` arguments are accepted in the same way.

## Precision

Evaluation at $x = -1$, $0$, and $1$ is exact — no rounding error — for every supported degree
and real kind. Away from these points, cancellation among the coefficients grows with degree,
and the resulting relative error grows accordingly. For `complex` arguments, each multiplication
involves more floating-point operations than the `real` case, so precision may degrade somewhat
faster with degree.

The derivative is also exact at $x = -1$, $0$, and $1$ for every supported degree and kind, for
both real and complex arguments: $P_n'(\pm 1) = (\pm 1)^{n+1} n (n + 1) / 2$, and $P_n'(0)$ is
zero for even $n$. These values are checked by the tests in [`test/`][main-test].

Away from these points, the tests compare `p_n_prime` with reference values generated by
[`reference/scipy_prime.fypp`][scipy-prime-fypp], which evaluates
$P_n'(x) = n (P_{n-1}(x) - x P_n(x)) / (1 - x^2)$ using only
[`scipy.special.eval_legendre`][scipy-eval-legendre]. The tests do not check the accuracy of the
derivative for complex arguments away from these points; the example below compares it with a
reference.

## Installation

Add this as a dependency in your `fpm.toml`, pointing at either the `default` branch (`real32`
and `real64` only) or the `with_real128` branch (adds `real128`):

```toml
[dependencies]
legendre_polynomial_closed_form_fortran = { git = "https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran", branch = "default" }
```

Pin to a specific release instead of a branch by using its tag (e.g. `tag = "v0.3.0+default"`).

## Example

`example/demo_p_n_fortran.f90` evaluates `p_n(degree, x)` for every supported degree and real
kind over $x \in [-1, 1]$, compares each value against [SciPy][scipy]'s
[`scipy.special.eval_legendre`][scipy-eval-legendre], and writes the results and the error to
`.dat` files; `example/demo_p_n_real.gpl` plots both as SVGs with [gnuplot][gnuplot]. This is
for developing and verifying the library itself, so it isn't included in the
[`default`][branch-default]/[`with_real128`][branch-with-real128] branches — see it, and the
`.fypp` sources it's generated from, on [`main`][main-example].

`example/demo_p_n_prime_fortran.f90` does the same for `p_n_prime(degree, x)`, and
`example/demo_p_n_prime_real.gpl` plots its results. The real-valued reference values come
from [`example/scipy_real_prime.fypp`][scipy-real-prime-fypp], which uses the same identity as
above (at $x = \pm 1$, where it is indeterminate, the known value of $P_n'(\pm 1)$ is used
instead).

### A note on the complex-valued reference comparison

[`example/`][main-example] compares this library's complex-valued evaluation against a
reference value generated by [`example/mpmath_complex.fypp`][mpmath-complex-fypp] using
[mpmath][mpmath]. This reference previously used `scipy.special.eval_legendre`, which
showed large discrepancies at higher degrees for complex arguments; this repository had
not determined at the time whether those discrepancies reflected this library's own
precision or the reference value's. For context: [`scipy.special.eval_legendre`][scipy-eval-legendre]
evaluates its result via the Gauss hypergeometric function ${}_2F_1(-n,\ n+1;\ 1;\ (1-x)/2)$
rather than a three-term recurrence (see [`orthogonal_eval.pxd`][scipy-orthogonal-eval-pxd]).

The complex-valued reference of the derivative is generated by
[`example/mpmath_complex_prime.fypp`][mpmath-complex-prime-fypp], which differentiates
`mpmath.legendre` with `mpmath.diff` and therefore does not depend on the identity above.

The mpmath-based reference's working precision is not fixed: generation doubles it,
starting from 64 bits, until the generated output text stops changing at 39 significant
digits, then uses that converged output. This is an empirical convergence check, not a
proven error bound — mpmath here uses ordinary (non-interval) multiple-precision
arithmetic, so a stable result across a doubling of precision is evidence that rounding
error is no longer visible at the requested output precision, not a certified bound.

[mpmath]: https://mpmath.org/
[mpmath-complex-fypp]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/blob/main/example/mpmath_complex.fypp
[mpmath-complex-prime-fypp]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/blob/main/example/mpmath_complex_prime.fypp
[scipy]: https://scipy.org/
[scipy-prime-fypp]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/blob/main/reference/scipy_prime.fypp
[scipy-real-prime-fypp]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/blob/main/example/scipy_real_prime.fypp
[scipy-eval-legendre]: https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.eval_legendre.html
[scipy-orthogonal-eval-pxd]: https://github.com/scipy/scipy/blob/v1.18.1/scipy/special/orthogonal_eval.pxd
[gnuplot]: http://www.gnuplot.info/
[main-example]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/tree/main/example
[main-test]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/tree/main/test

## License

MIT
