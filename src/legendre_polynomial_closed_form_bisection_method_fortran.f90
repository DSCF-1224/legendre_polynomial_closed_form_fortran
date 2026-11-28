module legendre_polynomial_closed_form_bisection_method_fortran
  !! @note
  !! - \( P_{n} (x) \) is Legendre polynomial of degree \( n \)
  !! - This module does NOT support `real128`
  !! @endnote

  use, intrinsic :: iso_fortran_env, only: int32
  use, intrinsic :: iso_fortran_env, only: real32
  use, intrinsic :: iso_fortran_env, only: real64

  use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
  use, intrinsic :: ieee_arithmetic, only: ieee_value

  use, non_intrinsic :: legendre_polynomial_closed_form_fortran, only: &!
    p_01, &!
    p_02, &!
    p_03, &!
    p_04, &!
    p_05, &!
    p_06, &!
    p_07, &!
    p_08, &!
    p_09, &!
    p_10, &!
    p_11, &!
    p_12, &!
    p_13, &!
    p_14, &!
    p_15, &!
    p_16, &!
    p_17, &!
    p_18, &!
    p_19, &!
    p_20, &!
    p_21, &!
    p_22, &!
    p_23, &!
    p_24, &!
    p_25, &!
    p_26, &!
    p_27



  implicit none



  private

  public :: update_interval_by_bisection_method



  !> version: experimental
  !! One step of the bisection method for a zero of \( P_{n} (x) \)
  !! \( ( n = 1 , 2, \dots , N ) \), where
  !! $$
  !! N =
  !! \begin{cases}
  !! 13 & \text{for real32} \\
  !! 27 & \text{for real64}
  !! \end{cases}
  !! $$
  !!
  !! The ends of the interval are given by `x_a` and `x_b`, and the values of
  !! \( P_{n} (x) \) at these ends by `p_n_a` and `p_n_b`.
  !! These four arguments are updated in place.
  !! The order of `x_a` and `x_b` is not used.
  !! The midpoint `(x_a + x_b) * 0.5` is evaluated, and
  !! - if \( P_{n} \) is exactly zero there, both ends become the midpoint;
  !! - if its sign is the same as that of `p_n_a`, `x_a` becomes the midpoint;
  !! - otherwise, `x_b` becomes the midpoint.
  !!
  !! `p_n_a` and `p_n_b` are updated together with the ends.
  !! Therefore \( P_{n} \) is evaluated once per step.
  !!
  !! If `degree` is not supported, the four arguments become signaling NaNs.
  !!
  !! @warning
  !! The signs of `p_n_a` and `p_n_b` must be different,
  !! and these must be the values of \( P_{n} \) at `x_a` and `x_b`.
  !! This is not checked.
  !! @endwarning
  interface update_interval_by_bisection_method
    module procedure update_interval_by_bisection_method_real32
    module procedure update_interval_by_bisection_method_real64
  end interface update_interval_by_bisection_method



  contains


  elemental subroutine update_interval_by_bisection_method_real32(degree, x_a, x_b, p_n_a, p_n_b)
    !! version: experimental
    !! One step of the bisection method for a zero of
    !! \( P_{n} (x) \ ( n = 1 , 2, \dots , 13 ) \) for `real(real32)`

    integer(int32), intent(in) :: degree

    real(real32), intent(inout) :: x_a, x_b

    real(real32), intent(inout) :: p_n_a, p_n_b


    real(real32) :: x_middle, p_n_middle


    x_middle = (x_a + x_b) * 0.5_real32

    select case (degree)

      case (  1_int32 ); p_n_middle = p_01(x_middle)
      case (  2_int32 ); p_n_middle = p_02(x_middle)
      case (  3_int32 ); p_n_middle = p_03(x_middle)
      case (  4_int32 ); p_n_middle = p_04(x_middle)
      case (  5_int32 ); p_n_middle = p_05(x_middle)
      case (  6_int32 ); p_n_middle = p_06(x_middle)
      case (  7_int32 ); p_n_middle = p_07(x_middle)
      case (  8_int32 ); p_n_middle = p_08(x_middle)
      case (  9_int32 ); p_n_middle = p_09(x_middle)
      case ( 10_int32 ); p_n_middle = p_10(x_middle)
      case ( 11_int32 ); p_n_middle = p_11(x_middle)
      case ( 12_int32 ); p_n_middle = p_12(x_middle)
      case ( 13_int32 ); p_n_middle = p_13(x_middle)

      case default

        x_a   = ieee_value(x_a, ieee_signaling_nan)
        x_b   = ieee_value(x_b, ieee_signaling_nan)
        p_n_a = ieee_value(p_n_a, ieee_signaling_nan)
        p_n_b = ieee_value(p_n_b, ieee_signaling_nan)

        return

    end select

    if ( abs(p_n_middle) <= 0.0_real32 ) then

      x_a   = x_middle
      x_b   = x_middle
      p_n_a = p_n_middle
      p_n_b = p_n_middle

    else if ( (p_n_middle < 0.0_real32) .eqv. (p_n_a < 0.0_real32) ) then

      x_a   = x_middle
      p_n_a = p_n_middle

    else

      x_b   = x_middle
      p_n_b = p_n_middle

    end if

  end subroutine update_interval_by_bisection_method_real32


  elemental subroutine update_interval_by_bisection_method_real64(degree, x_a, x_b, p_n_a, p_n_b)
    !! version: experimental
    !! One step of the bisection method for a zero of
    !! \( P_{n} (x) \ ( n = 1 , 2, \dots , 27 ) \) for `real(real64)`

    integer(int32), intent(in) :: degree

    real(real64), intent(inout) :: x_a, x_b

    real(real64), intent(inout) :: p_n_a, p_n_b


    real(real64) :: x_middle, p_n_middle


    x_middle = (x_a + x_b) * 0.5_real64

    select case (degree)

      case (  1_int32 ); p_n_middle = p_01(x_middle)
      case (  2_int32 ); p_n_middle = p_02(x_middle)
      case (  3_int32 ); p_n_middle = p_03(x_middle)
      case (  4_int32 ); p_n_middle = p_04(x_middle)
      case (  5_int32 ); p_n_middle = p_05(x_middle)
      case (  6_int32 ); p_n_middle = p_06(x_middle)
      case (  7_int32 ); p_n_middle = p_07(x_middle)
      case (  8_int32 ); p_n_middle = p_08(x_middle)
      case (  9_int32 ); p_n_middle = p_09(x_middle)
      case ( 10_int32 ); p_n_middle = p_10(x_middle)
      case ( 11_int32 ); p_n_middle = p_11(x_middle)
      case ( 12_int32 ); p_n_middle = p_12(x_middle)
      case ( 13_int32 ); p_n_middle = p_13(x_middle)
      case ( 14_int32 ); p_n_middle = p_14(x_middle)
      case ( 15_int32 ); p_n_middle = p_15(x_middle)
      case ( 16_int32 ); p_n_middle = p_16(x_middle)
      case ( 17_int32 ); p_n_middle = p_17(x_middle)
      case ( 18_int32 ); p_n_middle = p_18(x_middle)
      case ( 19_int32 ); p_n_middle = p_19(x_middle)
      case ( 20_int32 ); p_n_middle = p_20(x_middle)
      case ( 21_int32 ); p_n_middle = p_21(x_middle)
      case ( 22_int32 ); p_n_middle = p_22(x_middle)
      case ( 23_int32 ); p_n_middle = p_23(x_middle)
      case ( 24_int32 ); p_n_middle = p_24(x_middle)
      case ( 25_int32 ); p_n_middle = p_25(x_middle)
      case ( 26_int32 ); p_n_middle = p_26(x_middle)
      case ( 27_int32 ); p_n_middle = p_27(x_middle)

      case default

        x_a   = ieee_value(x_a, ieee_signaling_nan)
        x_b   = ieee_value(x_b, ieee_signaling_nan)
        p_n_a = ieee_value(p_n_a, ieee_signaling_nan)
        p_n_b = ieee_value(p_n_b, ieee_signaling_nan)

        return

    end select

    if ( abs(p_n_middle) <= 0.0_real64 ) then

      x_a   = x_middle
      x_b   = x_middle
      p_n_a = p_n_middle
      p_n_b = p_n_middle

    else if ( (p_n_middle < 0.0_real64) .eqv. (p_n_a < 0.0_real64) ) then

      x_a   = x_middle
      p_n_a = p_n_middle

    else

      x_b   = x_middle
      p_n_b = p_n_middle

    end if

  end subroutine update_interval_by_bisection_method_real64

end module legendre_polynomial_closed_form_bisection_method_fortran
