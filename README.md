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

## Precision

Evaluation at $x = -1$, $0$, and $1$ is exact — no rounding error — for every supported degree
and real kind. Away from these points, cancellation among the coefficients grows with degree,
and the resulting relative error grows accordingly. For `complex` arguments, each multiplication
involves more floating-point operations than the `real` case, so precision may degrade somewhat
faster with degree.

## Installation

Add this as a dependency in your `fpm.toml`, pointing at either the `default` branch (`real32`
and `real64` only) or the `with_real128` branch (adds `real128`):

```toml
[dependencies]
legendre_polynomial_closed_form_fortran = { git = "https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran", branch = "default" }
```

Pin to a specific release instead of a branch by using its tag (e.g. `tag = "v0.1.3+default"`).

## Example

`example/demo_p_n_fortran.f90` evaluates `p_n(degree, x)` for every supported degree and real
kind over $x \in [-1, 1]$, compares each value against [SciPy][scipy]'s
[`scipy.special.eval_legendre`][scipy-eval-legendre], and writes the results and the error to
`.dat` files; `example/demo_p_n_gnuplot.gpl` plots both as SVGs with [gnuplot][gnuplot]. This is
for developing and verifying the library itself, so it isn't included in the
[`default`][branch-default]/[`with_real128`][branch-with-real128] branches — see it, and the
`.fypp` sources it's generated from, on [`main`][main-example].

[scipy]: https://scipy.org/
[scipy-eval-legendre]: https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.eval_legendre.html
[gnuplot]: http://www.gnuplot.info/
[main-example]: https://github.com/DSCF-1224/legendre_polynomial_closed_form_fortran/tree/main/example

## License

MIT
