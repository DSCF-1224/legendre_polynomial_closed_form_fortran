module legendre_polynomial_closed_form_newton_method_fortran
  !! @note
  !! - \( P_{n} (x) \) is Legendre polynomial of degree \( n \)
  !! - This module supports `real128`
  !! @endnote

  use, intrinsic :: iso_fortran_env, only: int32
  use, intrinsic :: iso_fortran_env, only: real32
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: real128

  use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
  use, intrinsic :: ieee_arithmetic, only: ieee_value

  use, non_intrinsic :: legendre_polynomial_closed_form_fortran, only: &!
    p_01, p_01_prime, &!
    p_02, p_02_prime, &!
    p_03, p_03_prime, &!
    p_04, p_04_prime, &!
    p_05, p_05_prime, &!
    p_06, p_06_prime, &!
    p_07, p_07_prime, &!
    p_08, p_08_prime, &!
    p_09, p_09_prime, &!
    p_10, p_10_prime, &!
    p_11, p_11_prime, &!
    p_12, p_12_prime, &!
    p_13, p_13_prime, &!
    p_14, p_14_prime, &!
    p_15, p_15_prime, &!
    p_16, p_16_prime, &!
    p_17, p_17_prime, &!
    p_18, p_18_prime, &!
    p_19, p_19_prime, &!
    p_20, p_20_prime, &!
    p_21, p_21_prime, &!
    p_22, p_22_prime, &!
    p_23, p_23_prime, &!
    p_24, p_24_prime, &!
    p_25, p_25_prime, &!
    p_26, p_26_prime, &!
    p_27, p_27_prime, &!
    p_28, p_28_prime



  implicit none



  private

  public :: update_zero_by_newton_method



  !> version: experimental
  !! $$
  !! x - P_{n}(x) / P_{n}'(x) \ ( n = 1 , 2, \dots , N ),
  !! \ N =
  !! \begin{cases}
  !! 12 & \text{for real32} \\
  !! 25 & \text{for real64} \\
  !! 28 & \text{for real128}
  !! \end{cases}
  !! $$
  interface update_zero_by_newton_method
    module procedure update_zero_by_newton_method_real32
    module procedure update_zero_by_newton_method_real64
    module procedure update_zero_by_newton_method_real128
  end interface update_zero_by_newton_method



  contains


  real(real32) elemental function update_zero_by_newton_method_real32(degree, x) result(updated_x)
    !! version: experimental
    !! \( x - P_{n}(x) / P_{n}'(x) \ ( n = 1 , 2, \dots , 12 ) \) for `real(real32)`

    integer(int32), intent(in) :: degree

    real(real32), intent(in) :: x


    select case (degree)

      case (  1_int32 ); updated_x = x - p_01(x) / p_01_prime(x)
      case (  2_int32 ); updated_x = x - p_02(x) / p_02_prime(x)
      case (  3_int32 ); updated_x = x - p_03(x) / p_03_prime(x)
      case (  4_int32 ); updated_x = x - p_04(x) / p_04_prime(x)
      case (  5_int32 ); updated_x = x - p_05(x) / p_05_prime(x)
      case (  6_int32 ); updated_x = x - p_06(x) / p_06_prime(x)
      case (  7_int32 ); updated_x = x - p_07(x) / p_07_prime(x)
      case (  8_int32 ); updated_x = x - p_08(x) / p_08_prime(x)
      case (  9_int32 ); updated_x = x - p_09(x) / p_09_prime(x)
      case ( 10_int32 ); updated_x = x - p_10(x) / p_10_prime(x)
      case ( 11_int32 ); updated_x = x - p_11(x) / p_11_prime(x)
      case ( 12_int32 ); updated_x = x - p_12(x) / p_12_prime(x)

      case default; updated_x = ieee_value(x, ieee_signaling_nan)

    end select

  end function update_zero_by_newton_method_real32


  real(real64) elemental function update_zero_by_newton_method_real64(degree, x) result(updated_x)
    !! version: experimental
    !! \( x - P_{n}(x) / P_{n}'(x) \ ( n = 1 , 2, \dots , 25 ) \) for `real(real64)`

    integer(int32), intent(in) :: degree

    real(real64), intent(in) :: x


    select case (degree)

      case (  1_int32 ); updated_x = x - p_01(x) / p_01_prime(x)
      case (  2_int32 ); updated_x = x - p_02(x) / p_02_prime(x)
      case (  3_int32 ); updated_x = x - p_03(x) / p_03_prime(x)
      case (  4_int32 ); updated_x = x - p_04(x) / p_04_prime(x)
      case (  5_int32 ); updated_x = x - p_05(x) / p_05_prime(x)
      case (  6_int32 ); updated_x = x - p_06(x) / p_06_prime(x)
      case (  7_int32 ); updated_x = x - p_07(x) / p_07_prime(x)
      case (  8_int32 ); updated_x = x - p_08(x) / p_08_prime(x)
      case (  9_int32 ); updated_x = x - p_09(x) / p_09_prime(x)
      case ( 10_int32 ); updated_x = x - p_10(x) / p_10_prime(x)
      case ( 11_int32 ); updated_x = x - p_11(x) / p_11_prime(x)
      case ( 12_int32 ); updated_x = x - p_12(x) / p_12_prime(x)
      case ( 13_int32 ); updated_x = x - p_13(x) / p_13_prime(x)
      case ( 14_int32 ); updated_x = x - p_14(x) / p_14_prime(x)
      case ( 15_int32 ); updated_x = x - p_15(x) / p_15_prime(x)
      case ( 16_int32 ); updated_x = x - p_16(x) / p_16_prime(x)
      case ( 17_int32 ); updated_x = x - p_17(x) / p_17_prime(x)
      case ( 18_int32 ); updated_x = x - p_18(x) / p_18_prime(x)
      case ( 19_int32 ); updated_x = x - p_19(x) / p_19_prime(x)
      case ( 20_int32 ); updated_x = x - p_20(x) / p_20_prime(x)
      case ( 21_int32 ); updated_x = x - p_21(x) / p_21_prime(x)
      case ( 22_int32 ); updated_x = x - p_22(x) / p_22_prime(x)
      case ( 23_int32 ); updated_x = x - p_23(x) / p_23_prime(x)
      case ( 24_int32 ); updated_x = x - p_24(x) / p_24_prime(x)
      case ( 25_int32 ); updated_x = x - p_25(x) / p_25_prime(x)

      case default; updated_x = ieee_value(x, ieee_signaling_nan)

    end select

  end function update_zero_by_newton_method_real64


  real(real128) elemental function update_zero_by_newton_method_real128(degree, x) result(updated_x)
    !! version: experimental
    !! \( x - P_{n}(x) / P_{n}'(x) \ ( n = 1 , 2, \dots , 28 ) \) for `real(real128)`

    integer(int32), intent(in) :: degree

    real(real128), intent(in) :: x


    select case (degree)

      case (  1_int32 ); updated_x = x - p_01(x) / p_01_prime(x)
      case (  2_int32 ); updated_x = x - p_02(x) / p_02_prime(x)
      case (  3_int32 ); updated_x = x - p_03(x) / p_03_prime(x)
      case (  4_int32 ); updated_x = x - p_04(x) / p_04_prime(x)
      case (  5_int32 ); updated_x = x - p_05(x) / p_05_prime(x)
      case (  6_int32 ); updated_x = x - p_06(x) / p_06_prime(x)
      case (  7_int32 ); updated_x = x - p_07(x) / p_07_prime(x)
      case (  8_int32 ); updated_x = x - p_08(x) / p_08_prime(x)
      case (  9_int32 ); updated_x = x - p_09(x) / p_09_prime(x)
      case ( 10_int32 ); updated_x = x - p_10(x) / p_10_prime(x)
      case ( 11_int32 ); updated_x = x - p_11(x) / p_11_prime(x)
      case ( 12_int32 ); updated_x = x - p_12(x) / p_12_prime(x)
      case ( 13_int32 ); updated_x = x - p_13(x) / p_13_prime(x)
      case ( 14_int32 ); updated_x = x - p_14(x) / p_14_prime(x)
      case ( 15_int32 ); updated_x = x - p_15(x) / p_15_prime(x)
      case ( 16_int32 ); updated_x = x - p_16(x) / p_16_prime(x)
      case ( 17_int32 ); updated_x = x - p_17(x) / p_17_prime(x)
      case ( 18_int32 ); updated_x = x - p_18(x) / p_18_prime(x)
      case ( 19_int32 ); updated_x = x - p_19(x) / p_19_prime(x)
      case ( 20_int32 ); updated_x = x - p_20(x) / p_20_prime(x)
      case ( 21_int32 ); updated_x = x - p_21(x) / p_21_prime(x)
      case ( 22_int32 ); updated_x = x - p_22(x) / p_22_prime(x)
      case ( 23_int32 ); updated_x = x - p_23(x) / p_23_prime(x)
      case ( 24_int32 ); updated_x = x - p_24(x) / p_24_prime(x)
      case ( 25_int32 ); updated_x = x - p_25(x) / p_25_prime(x)
      case ( 26_int32 ); updated_x = x - p_26(x) / p_26_prime(x)
      case ( 27_int32 ); updated_x = x - p_27(x) / p_27_prime(x)
      case ( 28_int32 ); updated_x = x - p_28(x) / p_28_prime(x)

      case default; updated_x = ieee_value(x, ieee_signaling_nan)

    end select

  end function update_zero_by_newton_method_real128

end module legendre_polynomial_closed_form_newton_method_fortran
