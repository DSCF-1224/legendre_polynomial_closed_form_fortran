module legendre_polynomial_closed_form_fortran
  !! Legendre polynomial evaluation for Fortran, via the closed-form expression.
  !! @note
  !! - \( P_n (x) \) is Legendre polynomial of degree \( n \)
  !! - This module supports `real128`
  !! @endnote

  use, intrinsic :: iso_fortran_env, only: int32
  use, intrinsic :: iso_fortran_env, only: int64
  use, intrinsic :: iso_fortran_env, only: real32
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: iso_fortran_env, only: real128

  use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
  use, intrinsic :: ieee_arithmetic, only: ieee_value



  implicit none



  private

  public :: p_00
  public :: p_01
  public :: p_02
  public :: p_03
  public :: p_04
  public :: p_05
  public :: p_06
  public :: p_07
  public :: p_08
  public :: p_09
  public :: p_10
  public :: p_11
  public :: p_12
  public :: p_13
  public :: p_14
  public :: p_15
  public :: p_16
  public :: p_17
  public :: p_18
  public :: p_19
  public :: p_20
  public :: p_21
  public :: p_22
  public :: p_23
  public :: p_24
  public :: p_25
  public :: p_26
  public :: p_27
  public :: p_28
  public :: p_29
  public :: p_30
  public :: p_n



  integer(int32), parameter :: denominator_deg02 =          4_int32 !! for \( P_{ 2} (x) \)
  integer(int32), parameter :: denominator_deg03 =          8_int32 !! for \( P_{ 3} (x) \)
  integer(int32), parameter :: denominator_deg04 =         16_int32 !! for \( P_{ 4} (x) \)
  integer(int32), parameter :: denominator_deg05 =         32_int32 !! for \( P_{ 5} (x) \)
  integer(int32), parameter :: denominator_deg06 =         64_int32 !! for \( P_{ 6} (x) \)
  integer(int32), parameter :: denominator_deg07 =        128_int32 !! for \( P_{ 7} (x) \)
  integer(int32), parameter :: denominator_deg08 =        256_int32 !! for \( P_{ 8} (x) \)
  integer(int32), parameter :: denominator_deg09 =        512_int32 !! for \( P_{ 9} (x) \)
  integer(int32), parameter :: denominator_deg10 =       1024_int32 !! for \( P_{10} (x) \)
  integer(int32), parameter :: denominator_deg11 =       2048_int32 !! for \( P_{11} (x) \)
  integer(int32), parameter :: denominator_deg12 =       4096_int32 !! for \( P_{12} (x) \)
  integer(int32), parameter :: denominator_deg13 =       8192_int32 !! for \( P_{13} (x) \)
  integer(int32), parameter :: denominator_deg14 =      16384_int32 !! for \( P_{14} (x) \)
  integer(int32), parameter :: denominator_deg15 =      32768_int32 !! for \( P_{15} (x) \)
  integer(int32), parameter :: denominator_deg16 =      65536_int32 !! for \( P_{16} (x) \)
  integer(int32), parameter :: denominator_deg17 =     131072_int32 !! for \( P_{17} (x) \)
  integer(int32), parameter :: denominator_deg18 =     262144_int32 !! for \( P_{18} (x) \)
  integer(int32), parameter :: denominator_deg19 =     524288_int32 !! for \( P_{19} (x) \)
  integer(int32), parameter :: denominator_deg20 =    1048576_int32 !! for \( P_{20} (x) \)
  integer(int32), parameter :: denominator_deg21 =    2097152_int32 !! for \( P_{21} (x) \)
  integer(int32), parameter :: denominator_deg22 =    4194304_int32 !! for \( P_{22} (x) \)
  integer(int32), parameter :: denominator_deg23 =    8388608_int32 !! for \( P_{23} (x) \)
  integer(int32), parameter :: denominator_deg24 =   16777216_int32 !! for \( P_{24} (x) \)
  integer(int32), parameter :: denominator_deg25 =   33554432_int32 !! for \( P_{25} (x) \)
  integer(int32), parameter :: denominator_deg26 =   67108864_int32 !! for \( P_{26} (x) \)
  integer(int32), parameter :: denominator_deg27 =  134217728_int32 !! for \( P_{27} (x) \)
  integer(int32), parameter :: denominator_deg28 =  268435456_int32 !! for \( P_{28} (x) \)
  integer(int32), parameter :: denominator_deg29 =  536870912_int32 !! for \( P_{29} (x) \)
  integer(int32), parameter :: denominator_deg30 = 1073741824_int32 !! for \( P_{30} (x) \)



  integer(int32), parameter :: numerator_deg02_pow02 =                    6_int32 !! for \( x^{2} \) of \( P_{2} (x) \)
  integer(int32), parameter :: numerator_deg02_pow00 =                   -2_int32 !! for \( x^{0} \) of \( P_{2} (x) \)

  integer(int32), parameter :: numerator_deg03_pow03 =                   20_int32 !! for \( x^{3} \) of \( P_{3} (x) \)
  integer(int32), parameter :: numerator_deg03_pow01 =                  -12_int32 !! for \( x^{1} \) of \( P_{3} (x) \)

  integer(int32), parameter :: numerator_deg04_pow04 =                   70_int32 !! for \( x^{4} \) of \( P_{4} (x) \)
  integer(int32), parameter :: numerator_deg04_pow02 =                  -60_int32 !! for \( x^{2} \) of \( P_{4} (x) \)
  integer(int32), parameter :: numerator_deg04_pow00 =                    6_int32 !! for \( x^{0} \) of \( P_{4} (x) \)

  integer(int32), parameter :: numerator_deg05_pow05 =                  252_int32 !! for \( x^{5} \) of \( P_{5} (x) \)
  integer(int32), parameter :: numerator_deg05_pow03 =                 -280_int32 !! for \( x^{3} \) of \( P_{5} (x) \)
  integer(int32), parameter :: numerator_deg05_pow01 =                   60_int32 !! for \( x^{1} \) of \( P_{5} (x) \)

  integer(int32), parameter :: numerator_deg06_pow06 =                  924_int32 !! for \( x^{6} \) of \( P_{6} (x) \)
  integer(int32), parameter :: numerator_deg06_pow04 =                -1260_int32 !! for \( x^{4} \) of \( P_{6} (x) \)
  integer(int32), parameter :: numerator_deg06_pow02 =                  420_int32 !! for \( x^{2} \) of \( P_{6} (x) \)
  integer(int32), parameter :: numerator_deg06_pow00 =                  -20_int32 !! for \( x^{0} \) of \( P_{6} (x) \)

  integer(int32), parameter :: numerator_deg07_pow07 =                 3432_int32 !! for \( x^{7} \) of \( P_{7} (x) \)
  integer(int32), parameter :: numerator_deg07_pow05 =                -5544_int32 !! for \( x^{5} \) of \( P_{7} (x) \)
  integer(int32), parameter :: numerator_deg07_pow03 =                 2520_int32 !! for \( x^{3} \) of \( P_{7} (x) \)
  integer(int32), parameter :: numerator_deg07_pow01 =                 -280_int32 !! for \( x^{1} \) of \( P_{7} (x) \)

  integer(int32), parameter :: numerator_deg08_pow08 =                12870_int32 !! for \( x^{8} \) of \( P_{8} (x) \)
  integer(int32), parameter :: numerator_deg08_pow06 =               -24024_int32 !! for \( x^{6} \) of \( P_{8} (x) \)
  integer(int32), parameter :: numerator_deg08_pow04 =                13860_int32 !! for \( x^{4} \) of \( P_{8} (x) \)
  integer(int32), parameter :: numerator_deg08_pow02 =                -2520_int32 !! for \( x^{2} \) of \( P_{8} (x) \)
  integer(int32), parameter :: numerator_deg08_pow00 =                   70_int32 !! for \( x^{0} \) of \( P_{8} (x) \)

  integer(int32), parameter :: numerator_deg09_pow09 =                48620_int32 !! for \( x^{9} \) of \( P_{9} (x) \)
  integer(int32), parameter :: numerator_deg09_pow07 =              -102960_int32 !! for \( x^{7} \) of \( P_{9} (x) \)
  integer(int32), parameter :: numerator_deg09_pow05 =                72072_int32 !! for \( x^{5} \) of \( P_{9} (x) \)
  integer(int32), parameter :: numerator_deg09_pow03 =               -18480_int32 !! for \( x^{3} \) of \( P_{9} (x) \)
  integer(int32), parameter :: numerator_deg09_pow01 =                 1260_int32 !! for \( x^{1} \) of \( P_{9} (x) \)

  integer(int32), parameter :: numerator_deg10_pow10 =               184756_int32 !! for \( x^{10} \) of \( P_{10} (x) \)
  integer(int32), parameter :: numerator_deg10_pow08 =              -437580_int32 !! for \( x^{8} \) of \( P_{10} (x) \)
  integer(int32), parameter :: numerator_deg10_pow06 =               360360_int32 !! for \( x^{6} \) of \( P_{10} (x) \)
  integer(int32), parameter :: numerator_deg10_pow04 =              -120120_int32 !! for \( x^{4} \) of \( P_{10} (x) \)
  integer(int32), parameter :: numerator_deg10_pow02 =                13860_int32 !! for \( x^{2} \) of \( P_{10} (x) \)
  integer(int32), parameter :: numerator_deg10_pow00 =                 -252_int32 !! for \( x^{0} \) of \( P_{10} (x) \)

  integer(int32), parameter :: numerator_deg11_pow11 =               705432_int32 !! for \( x^{11} \) of \( P_{11} (x) \)
  integer(int32), parameter :: numerator_deg11_pow09 =             -1847560_int32 !! for \( x^{9} \) of \( P_{11} (x) \)
  integer(int32), parameter :: numerator_deg11_pow07 =              1750320_int32 !! for \( x^{7} \) of \( P_{11} (x) \)
  integer(int32), parameter :: numerator_deg11_pow05 =              -720720_int32 !! for \( x^{5} \) of \( P_{11} (x) \)
  integer(int32), parameter :: numerator_deg11_pow03 =               120120_int32 !! for \( x^{3} \) of \( P_{11} (x) \)
  integer(int32), parameter :: numerator_deg11_pow01 =                -5544_int32 !! for \( x^{1} \) of \( P_{11} (x) \)

  integer(int32), parameter :: numerator_deg12_pow12 =              2704156_int32 !! for \( x^{12} \) of \( P_{12} (x) \)
  integer(int32), parameter :: numerator_deg12_pow10 =             -7759752_int32 !! for \( x^{10} \) of \( P_{12} (x) \)
  integer(int32), parameter :: numerator_deg12_pow08 =              8314020_int32 !! for \( x^{8} \) of \( P_{12} (x) \)
  integer(int32), parameter :: numerator_deg12_pow06 =             -4084080_int32 !! for \( x^{6} \) of \( P_{12} (x) \)
  integer(int32), parameter :: numerator_deg12_pow04 =               900900_int32 !! for \( x^{4} \) of \( P_{12} (x) \)
  integer(int32), parameter :: numerator_deg12_pow02 =               -72072_int32 !! for \( x^{2} \) of \( P_{12} (x) \)
  integer(int32), parameter :: numerator_deg12_pow00 =                  924_int32 !! for \( x^{0} \) of \( P_{12} (x) \)

  integer(int32), parameter :: numerator_deg13_pow13 =             10400600_int32 !! for \( x^{13} \) of \( P_{13} (x) \)
  integer(int32), parameter :: numerator_deg13_pow11 =            -32449872_int32 !! for \( x^{11} \) of \( P_{13} (x) \)
  integer(int32), parameter :: numerator_deg13_pow09 =             38798760_int32 !! for \( x^{9} \) of \( P_{13} (x) \)
  integer(int32), parameter :: numerator_deg13_pow07 =            -22170720_int32 !! for \( x^{7} \) of \( P_{13} (x) \)
  integer(int32), parameter :: numerator_deg13_pow05 =              6126120_int32 !! for \( x^{5} \) of \( P_{13} (x) \)
  integer(int32), parameter :: numerator_deg13_pow03 =              -720720_int32 !! for \( x^{3} \) of \( P_{13} (x) \)
  integer(int32), parameter :: numerator_deg13_pow01 =                24024_int32 !! for \( x^{1} \) of \( P_{13} (x) \)

  integer(int32), parameter :: numerator_deg14_pow14 =             40116600_int32 !! for \( x^{14} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow12 =           -135207800_int32 !! for \( x^{12} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow10 =            178474296_int32 !! for \( x^{10} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow08 =           -116396280_int32 !! for \( x^{8} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow06 =             38798760_int32 !! for \( x^{6} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow04 =             -6126120_int32 !! for \( x^{4} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow02 =               360360_int32 !! for \( x^{2} \) of \( P_{14} (x) \)
  integer(int32), parameter :: numerator_deg14_pow00 =                -3432_int32 !! for \( x^{0} \) of \( P_{14} (x) \)

  integer(int32), parameter :: numerator_deg15_pow15 =            155117520_int32 !! for \( x^{15} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow13 =           -561632400_int32 !! for \( x^{13} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow11 =            811246800_int32 !! for \( x^{11} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow09 =           -594914320_int32 !! for \( x^{9} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow07 =            232792560_int32 !! for \( x^{7} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow05 =            -46558512_int32 !! for \( x^{5} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow03 =              4084080_int32 !! for \( x^{3} \) of \( P_{15} (x) \)
  integer(int32), parameter :: numerator_deg15_pow01 =              -102960_int32 !! for \( x^{1} \) of \( P_{15} (x) \)

  integer(int64), parameter :: numerator_deg16_pow16 =            601080390_int64 !! for \( x^{16} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow14 =          -2326762800_int64 !! for \( x^{14} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow12 =           3650610600_int64 !! for \( x^{12} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow10 =          -2974571600_int64 !! for \( x^{10} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow08 =           1338557220_int64 !! for \( x^{8} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow06 =           -325909584_int64 !! for \( x^{6} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow04 =             38798760_int64 !! for \( x^{4} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow02 =             -1750320_int64 !! for \( x^{2} \) of \( P_{16} (x) \)
  integer(int64), parameter :: numerator_deg16_pow00 =                12870_int64 !! for \( x^{0} \) of \( P_{16} (x) \)

  integer(int64), parameter :: numerator_deg17_pow17 =           2333606220_int64 !! for \( x^{17} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow15 =          -9617286240_int64 !! for \( x^{15} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow13 =          16287339600_int64 !! for \( x^{13} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow11 =         -14602442400_int64 !! for \( x^{11} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow09 =           7436429000_int64 !! for \( x^{9} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow07 =          -2141691552_int64 !! for \( x^{7} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow05 =            325909584_int64 !! for \( x^{5} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow03 =            -22170720_int64 !! for \( x^{3} \) of \( P_{17} (x) \)
  integer(int64), parameter :: numerator_deg17_pow01 =               437580_int64 !! for \( x^{1} \) of \( P_{17} (x) \)

  integer(int64), parameter :: numerator_deg18_pow18 =           9075135300_int64 !! for \( x^{18} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow16 =         -39671305740_int64 !! for \( x^{16} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow14 =          72129646800_int64 !! for \( x^{14} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow12 =         -70578471600_int64 !! for \( x^{12} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow10 =          40156716600_int64 !! for \( x^{10} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow08 =         -13385572200_int64 !! for \( x^{8} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow06 =           2498640144_int64 !! for \( x^{6} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow04 =           -232792560_int64 !! for \( x^{4} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow02 =              8314020_int64 !! for \( x^{2} \) of \( P_{18} (x) \)
  integer(int64), parameter :: numerator_deg18_pow00 =               -48620_int64 !! for \( x^{0} \) of \( P_{18} (x) \)

  integer(int64), parameter :: numerator_deg19_pow19 =          35345263800_int64 !! for \( x^{19} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow17 =        -163352435400_int64 !! for \( x^{17} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow15 =         317370445920_int64 !! for \( x^{15} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow13 =        -336605018400_int64 !! for \( x^{13} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow11 =         211735414800_int64 !! for \( x^{11} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow09 =         -80313433200_int64 !! for \( x^{9} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow07 =          17847429600_int64 !! for \( x^{7} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow05 =          -2141691552_int64 !! for \( x^{5} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow03 =            116396280_int64 !! for \( x^{3} \) of \( P_{19} (x) \)
  integer(int64), parameter :: numerator_deg19_pow01 =             -1847560_int64 !! for \( x^{1} \) of \( P_{19} (x) \)

  integer(int64), parameter :: numerator_deg20_pow20 =         137846528820_int64 !! for \( x^{20} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow18 =        -671560012200_int64 !! for \( x^{18} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow16 =        1388495700900_int64 !! for \( x^{16} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow14 =       -1586852229600_int64 !! for \( x^{14} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow12 =        1093966309800_int64 !! for \( x^{12} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow10 =        -465817912560_int64 !! for \( x^{10} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow08 =         120470149800_int64 !! for \( x^{8} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow06 =         -17847429600_int64 !! for \( x^{6} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow04 =           1338557220_int64 !! for \( x^{4} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow02 =            -38798760_int64 !! for \( x^{2} \) of \( P_{20} (x) \)
  integer(int64), parameter :: numerator_deg20_pow00 =               184756_int64 !! for \( x^{0} \) of \( P_{20} (x) \)

  integer(int64), parameter :: numerator_deg21_pow21 =         538257874440_int64 !! for \( x^{21} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow19 =       -2756930576400_int64 !! for \( x^{19} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow17 =        6044040109800_int64 !! for \( x^{17} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow15 =       -7405310404800_int64 !! for \( x^{15} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow13 =        5553982803600_int64 !! for \( x^{13} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow11 =       -2625519143520_int64 !! for \( x^{11} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow09 =         776363187600_int64 !! for \( x^{9} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow07 =        -137680171200_int64 !! for \( x^{7} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow05 =          13385572200_int64 !! for \( x^{5} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow03 =           -594914320_int64 !! for \( x^{3} \) of \( P_{21} (x) \)
  integer(int64), parameter :: numerator_deg21_pow01 =              7759752_int64 !! for \( x^{1} \) of \( P_{21} (x) \)

  integer(int64), parameter :: numerator_deg22_pow22 =        2104098963720_int64 !! for \( x^{22} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow20 =      -11303415363240_int64 !! for \( x^{20} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow18 =       26190840475800_int64 !! for \( x^{18} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow16 =      -34249560622200_int64 !! for \( x^{16} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow14 =       27769914018000_int64 !! for \( x^{14} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow12 =      -14440355289360_int64 !! for \( x^{12} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow10 =        4813451763120_int64 !! for \( x^{10} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow08 =        -998181241200_int64 !! for \( x^{8} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow06 =         120470149800_int64 !! for \( x^{6} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow04 =          -7436429000_int64 !! for \( x^{4} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow02 =            178474296_int64 !! for \( x^{2} \) of \( P_{22} (x) \)
  integer(int64), parameter :: numerator_deg22_pow00 =              -705432_int64 !! for \( x^{0} \) of \( P_{22} (x) \)

  integer(int64), parameter :: numerator_deg23_pow23 =        8233430727600_int64 !! for \( x^{23} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow21 =      -46290177201840_int64 !! for \( x^{21} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow19 =      113034153632400_int64 !! for \( x^{19} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow17 =     -157145042854800_int64 !! for \( x^{17} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow15 =      136998242488800_int64 !! for \( x^{15} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow13 =      -77755759250400_int64 !! for \( x^{13} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow11 =       28880710578720_int64 !! for \( x^{11} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow09 =       -6876359661600_int64 !! for \( x^{9} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow07 =         998181241200_int64 !! for \( x^{7} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow05 =         -80313433200_int64 !! for \( x^{5} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow03 =           2974571600_int64 !! for \( x^{3} \) of \( P_{23} (x) \)
  integer(int64), parameter :: numerator_deg23_pow01 =            -32449872_int64 !! for \( x^{1} \) of \( P_{23} (x) \)

  integer(int64), parameter :: numerator_deg24_pow24 =       32247603683100_int64 !! for \( x^{24} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow22 =     -189368906734800_int64 !! for \( x^{22} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow20 =      486046860619320_int64 !! for \( x^{20} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow18 =     -715882973005200_int64 !! for \( x^{18} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow16 =      667866432132900_int64 !! for \( x^{16} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow14 =     -410994727466400_int64 !! for \( x^{14} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow12 =      168470811709200_int64 !! for \( x^{12} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow10 =      -45383973766560_int64 !! for \( x^{10} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow08 =        7735904619300_int64 !! for \( x^{8} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow06 =        -776363187600_int64 !! for \( x^{6} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow04 =          40156716600_int64 !! for \( x^{4} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow02 =           -811246800_int64 !! for \( x^{2} \) of \( P_{24} (x) \)
  integer(int64), parameter :: numerator_deg24_pow00 =              2704156_int64 !! for \( x^{0} \) of \( P_{24} (x) \)

  integer(int64), parameter :: numerator_deg25_pow25 =      126410606437752_int64 !! for \( x^{25} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow23 =     -773942488394400_int64 !! for \( x^{23} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow21 =     2083057974082800_int64 !! for \( x^{21} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow19 =    -3240312404128800_int64 !! for \( x^{19} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow17 =     3221473378523400_int64 !! for \( x^{17} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow15 =    -2137172582825280_int64 !! for \( x^{15} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow13 =      958987697421600_int64 !! for \( x^{13} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow11 =     -288807105787200_int64 !! for \( x^{11} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow09 =       56729967208200_int64 !! for \( x^{9} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow07 =       -6876359661600_int64 !! for \( x^{7} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow05 =         465817912560_int64 !! for \( x^{5} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow03 =         -14602442400_int64 !! for \( x^{3} \) of \( P_{25} (x) \)
  integer(int64), parameter :: numerator_deg25_pow01 =            135207800_int64 !! for \( x^{1} \) of \( P_{25} (x) \)

  integer(int64), parameter :: numerator_deg26_pow26 =      495918532948104_int64 !! for \( x^{26} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow24 =    -3160265160943800_int64 !! for \( x^{24} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow22 =     8900338616535600_int64 !! for \( x^{22} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow20 =   -14581405818579600_int64 !! for \( x^{20} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow18 =    15391483919611800_int64 !! for \( x^{18} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow16 =   -10953009486979560_int64 !! for \( x^{16} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow14 =     5342931457063200_int64 !! for \( x^{14} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow12 =    -1780977152354400_int64 !! for \( x^{12} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow10 =      397109770457400_int64 !! for \( x^{10} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow08 =      -56729967208200_int64 !! for \( x^{8} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow06 =        4813451763120_int64 !! for \( x^{6} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow04 =        -211735414800_int64 !! for \( x^{4} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow02 =           3650610600_int64 !! for \( x^{2} \) of \( P_{26} (x) \)
  integer(int64), parameter :: numerator_deg26_pow00 =            -10400600_int64 !! for \( x^{0} \) of \( P_{26} (x) \)

  integer(int64), parameter :: numerator_deg27_pow27 =     1946939425648112_int64 !! for \( x^{27} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow25 =   -12893881856650704_int64 !! for \( x^{25} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow23 =    37923181931325600_int64 !! for \( x^{23} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow21 =   -65269149854594400_int64 !! for \( x^{21} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow19 =    72907029092898000_int64 !! for \( x^{19} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow17 =   -55409342110602480_int64 !! for \( x^{17} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow15 =    29208025298612160_int64 !! for \( x^{15} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow13 =   -10685862914126400_int64 !! for \( x^{13} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow11 =     2671465728531600_int64 !! for \( x^{11} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow09 =     -441233078286000_int64 !! for \( x^{9} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow07 =       45383973766560_int64 !! for \( x^{7} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow05 =       -2625519143520_int64 !! for \( x^{5} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow03 =          70578471600_int64 !! for \( x^{3} \) of \( P_{27} (x) \)
  integer(int64), parameter :: numerator_deg27_pow01 =           -561632400_int64 !! for \( x^{1} \) of \( P_{27} (x) \)

  integer(int64), parameter :: numerator_deg28_pow28 =     7648690600760440_int64 !! for \( x^{28} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow26 =   -52567364492499024_int64 !! for \( x^{26} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow24 =   161173523208133800_int64 !! for \( x^{24} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow22 =  -290744394806829600_int64 !! for \( x^{22} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow20 =   342663036736620600_int64 !! for \( x^{20} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow18 =  -277046710553012400_int64 !! for \( x^{18} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow16 =   156993135980040360_int64 !! for \( x^{16} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow14 =   -62588625639883200_int64 !! for \( x^{14} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow12 =    17364527235455400_int64 !! for \( x^{12} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow10 =    -3265124779316400_int64 !! for \( x^{10} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow08 =      397109770457400_int64 !! for \( x^{8} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow06 =      -28880710578720_int64 !! for \( x^{6} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow04 =        1093966309800_int64 !! for \( x^{4} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow02 =         -16287339600_int64 !! for \( x^{2} \) of \( P_{28} (x) \)
  integer(int64), parameter :: numerator_deg28_pow00 =             40116600_int64 !! for \( x^{0} \) of \( P_{28} (x) \)

  integer(int64), parameter :: numerator_deg29_pow29 =    30067266499541040_int64 !! for \( x^{29} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow27 =  -214163336821292320_int64 !! for \( x^{27} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow25 =   683375738402487312_int64 !! for \( x^{25} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow23 = -1289388185665070400_int64 !! for \( x^{23} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow21 =  1599094171437562800_int64 !! for \( x^{21} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow19 = -1370652146946482400_int64 !! for \( x^{19} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow17 =   831140131659037200_int64 !! for \( x^{17} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow15 =  -358841453668663680_int64 !! for \( x^{15} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow13 =   109530094869795600_int64 !! for \( x^{13} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow11 =   -23152702980607200_int64 !! for \( x^{11} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow09 =     3265124779316400_int64 !! for \( x^{9} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow07 =     -288807105787200_int64 !! for \( x^{7} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow05 =       14440355289360_int64 !! for \( x^{5} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow03 =        -336605018400_int64 !! for \( x^{3} \) of \( P_{29} (x) \)
  integer(int64), parameter :: numerator_deg29_pow01 =           2326762800_int64 !! for \( x^{1} \) of \( P_{29} (x) \)

  integer(int64), parameter :: numerator_deg30_pow30 =   118264581564861424_int64 !! for \( x^{30} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow28 =  -871950728486690160_int64 !! for \( x^{28} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow26 =  2891205047087446320_int64 !! for \( x^{26} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow24 = -5694797820020727600_int64 !! for \( x^{24} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow22 =  7413982067574154800_int64 !! for \( x^{22} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow20 = -6716195520037763760_int64 !! for \( x^{20} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow18 =  4340398465330527600_int64 !! for \( x^{18} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow16 = -2018483176886233200_int64 !! for \( x^{16} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow14 =   672827725628744400_int64 !! for \( x^{14} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow12 =  -158210137034149200_int64 !! for \( x^{12} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow10 =    25467973278667920_int64 !! for \( x^{10} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow08 =    -2671465728531600_int64 !! for \( x^{8} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow06 =      168470811709200_int64 !! for \( x^{6} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow04 =       -5553982803600_int64 !! for \( x^{4} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow02 =          72129646800_int64 !! for \( x^{2} \) of \( P_{30} (x) \)
  integer(int64), parameter :: numerator_deg30_pow00 =           -155117520_int64 !! for \( x^{0} \) of \( P_{30} (x) \)



  !> version: experimental
  !! \( P_{0} (x) \) for `real32`, `real64` and `real128`
  interface p_00
    module procedure p_00_real32
    module procedure p_00_real64
    module procedure p_00_real128
  end interface p_00


  !> version: experimental
  !! \( P_{1} (x) \) for `real32`, `real64` and `real128`
  interface p_01
    module procedure p_01_real32
    module procedure p_01_real64
    module procedure p_01_real128
  end interface p_01


  !> version: experimental
  !! \( P_{2} (x) \) for `real32`, `real64` and `real128`
  interface p_02
    module procedure p_02_real32
    module procedure p_02_real64
    module procedure p_02_real128
  end interface p_02


  !> version: experimental
  !! \( P_{3} (x) \) for `real32`, `real64` and `real128`
  interface p_03
    module procedure p_03_real32
    module procedure p_03_real64
    module procedure p_03_real128
  end interface p_03


  !> version: experimental
  !! \( P_{4} (x) \) for `real32`, `real64` and `real128`
  interface p_04
    module procedure p_04_real32
    module procedure p_04_real64
    module procedure p_04_real128
  end interface p_04


  !> version: experimental
  !! \( P_{5} (x) \) for `real32`, `real64` and `real128`
  interface p_05
    module procedure p_05_real32
    module procedure p_05_real64
    module procedure p_05_real128
  end interface p_05


  !> version: experimental
  !! \( P_{6} (x) \) for `real32`, `real64` and `real128`
  interface p_06
    module procedure p_06_real32
    module procedure p_06_real64
    module procedure p_06_real128
  end interface p_06


  !> version: experimental
  !! \( P_{7} (x) \) for `real32`, `real64` and `real128`
  interface p_07
    module procedure p_07_real32
    module procedure p_07_real64
    module procedure p_07_real128
  end interface p_07


  !> version: experimental
  !! \( P_{8} (x) \) for `real32`, `real64` and `real128`
  interface p_08
    module procedure p_08_real32
    module procedure p_08_real64
    module procedure p_08_real128
  end interface p_08


  !> version: experimental
  !! \( P_{9} (x) \) for `real32`, `real64` and `real128`
  interface p_09
    module procedure p_09_real32
    module procedure p_09_real64
    module procedure p_09_real128
  end interface p_09


  !> version: experimental
  !! \( P_{10} (x) \) for `real32`, `real64` and `real128`
  interface p_10
    module procedure p_10_real32
    module procedure p_10_real64
    module procedure p_10_real128
  end interface p_10


  !> version: experimental
  !! \( P_{11} (x) \) for `real32`, `real64` and `real128`
  interface p_11
    module procedure p_11_real32
    module procedure p_11_real64
    module procedure p_11_real128
  end interface p_11


  !> version: experimental
  !! \( P_{12} (x) \) for `real32`, `real64` and `real128`
  interface p_12
    module procedure p_12_real32
    module procedure p_12_real64
    module procedure p_12_real128
  end interface p_12


  !> version: experimental
  !! \( P_{13} (x) \) for `real32`, `real64` and `real128`
  interface p_13
    module procedure p_13_real32
    module procedure p_13_real64
    module procedure p_13_real128
  end interface p_13


  !> version: experimental
  !! \( P_{14} (x) \) for `real64` and `real128`
  interface p_14
    module procedure p_14_real64
    module procedure p_14_real128
  end interface p_14


  !> version: experimental
  !! \( P_{15} (x) \) for `real64` and `real128`
  interface p_15
    module procedure p_15_real64
    module procedure p_15_real128
  end interface p_15


  !> version: experimental
  !! \( P_{16} (x) \) for `real64` and `real128`
  interface p_16
    module procedure p_16_real64
    module procedure p_16_real128
  end interface p_16


  !> version: experimental
  !! \( P_{17} (x) \) for `real64` and `real128`
  interface p_17
    module procedure p_17_real64
    module procedure p_17_real128
  end interface p_17


  !> version: experimental
  !! \( P_{18} (x) \) for `real64` and `real128`
  interface p_18
    module procedure p_18_real64
    module procedure p_18_real128
  end interface p_18


  !> version: experimental
  !! \( P_{19} (x) \) for `real64` and `real128`
  interface p_19
    module procedure p_19_real64
    module procedure p_19_real128
  end interface p_19


  !> version: experimental
  !! \( P_{20} (x) \) for `real64` and `real128`
  interface p_20
    module procedure p_20_real64
    module procedure p_20_real128
  end interface p_20


  !> version: experimental
  !! \( P_{21} (x) \) for `real64` and `real128`
  interface p_21
    module procedure p_21_real64
    module procedure p_21_real128
  end interface p_21


  !> version: experimental
  !! \( P_{22} (x) \) for `real64` and `real128`
  interface p_22
    module procedure p_22_real64
    module procedure p_22_real128
  end interface p_22


  !> version: experimental
  !! \( P_{23} (x) \) for `real64` and `real128`
  interface p_23
    module procedure p_23_real64
    module procedure p_23_real128
  end interface p_23


  !> version: experimental
  !! \( P_{24} (x) \) for `real64` and `real128`
  interface p_24
    module procedure p_24_real64
    module procedure p_24_real128
  end interface p_24


  !> version: experimental
  !! \( P_{25} (x) \) for `real64` and `real128`
  interface p_25
    module procedure p_25_real64
    module procedure p_25_real128
  end interface p_25


  !> version: experimental
  !! \( P_{26} (x) \) for `real64` and `real128`
  interface p_26
    module procedure p_26_real64
    module procedure p_26_real128
  end interface p_26


  !> version: experimental
  !! \( P_{27} (x) \) for `real64` and `real128`
  interface p_27
    module procedure p_27_real64
    module procedure p_27_real128
  end interface p_27


  !> version: experimental
  !! \( P_{28} (x) \) for `real128` only
  interface p_28
    module procedure p_28_real128
  end interface p_28


  !> version: experimental
  !! \( P_{29} (x) \) for `real128` only
  interface p_29
    module procedure p_29_real128
  end interface p_29


  !> version: experimental
  !! \( P_{30} (x) \) for `real128` only
  interface p_30
    module procedure p_30_real128
  end interface p_30


  !> version: experimental
  !! $$
  !! P_n (x) \ ( n = 0 , 1, \dots , N ),\ 
  !! N =
  !! \begin{cases}
  !! 13 & \text{for real32} \\
  !! 27 & \text{for real64} \\
  !! 30 & \text{for real128}
  !! \end{cases}
  !! $$
  interface p_n
    module procedure p_n_real32
    module procedure p_n_real64
    module procedure p_n_real128
  end interface p_n


  contains




  real(real32) elemental function p_00_real32(x)
    !! version: experimental
    !! \( P_{0} (x) \) for `real32`

    real(real32), intent(in) :: x

    p_00_real32 = real(1, kind=kind(x))

  end function p_00_real32


  real(real64) elemental function p_00_real64(x)
    !! version: experimental
    !! \( P_{0} (x) \) for `real64`

    real(real64), intent(in) :: x

    p_00_real64 = real(1, kind=kind(x))

  end function p_00_real64


  real(real128) elemental function p_00_real128(x)
    !! version: experimental
    !! \( P_{0} (x) \) for `real128`

    real(real128), intent(in) :: x

    p_00_real128 = real(1, kind=kind(x))

  end function p_00_real128




  real(real32) elemental function p_01_real32(x)
    !! version: experimental
    !! \( P_{1} (x) \) for `real32`

    real(real32), intent(in) :: x

    p_01_real32 = x

  end function p_01_real32


  real(real64) elemental function p_01_real64(x)
    !! version: experimental
    !! \( P_{1} (x) \) for `real64`

    real(real64), intent(in) :: x

    p_01_real64 = x

  end function p_01_real64


  real(real128) elemental function p_01_real128(x)
    !! version: experimental
    !! \( P_{1} (x) \) for `real128`

    real(real128), intent(in) :: x

    p_01_real128 = x

  end function p_01_real128




  real(real32) elemental function p_02_real32(x)
    !! version: experimental
    !! \( P_{2} (x) \) for `real32`

    real(real32), intent(in) :: x

    p_02_real32 = (numerator_deg02_pow02 * x * x + numerator_deg02_pow00) / denominator_deg02

  end function p_02_real32


  real(real64) elemental function p_02_real64(x)
    !! version: experimental
    !! \( P_{2} (x) \) for `real64`

    real(real64), intent(in) :: x

    p_02_real64 = (numerator_deg02_pow02 * x * x + numerator_deg02_pow00) / denominator_deg02

  end function p_02_real64


  real(real128) elemental function p_02_real128(x)
    !! version: experimental
    !! \( P_{2} (x) \) for `real128`

    real(real128), intent(in) :: x

    p_02_real128 = (numerator_deg02_pow02 * x * x + numerator_deg02_pow00) / denominator_deg02

  end function p_02_real128




  real(real32) elemental function p_03_real32(x)
    !! version: experimental
    !! \( P_{3} (x) \) for `real32`

    real(real32), intent(in) :: x

    p_03_real32 = (numerator_deg03_pow03 * x * x + numerator_deg03_pow01) * x / denominator_deg03

  end function p_03_real32


  real(real64) elemental function p_03_real64(x)
    !! version: experimental
    !! \( P_{3} (x) \) for `real64`

    real(real64), intent(in) :: x

    p_03_real64 = (numerator_deg03_pow03 * x * x + numerator_deg03_pow01) * x / denominator_deg03

  end function p_03_real64


  real(real128) elemental function p_03_real128(x)
    !! version: experimental
    !! \( P_{3} (x) \) for `real128`

    real(real128), intent(in) :: x

    p_03_real128 = (numerator_deg03_pow03 * x * x + numerator_deg03_pow01) * x / denominator_deg03

  end function p_03_real128




  real(real32) elemental function p_04_real32(x)
    !! version: experimental
    !! \( P_{4} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_04_real32 = numerator_deg04_pow02 + x2 * numerator_deg04_pow04
    p_04_real32 = numerator_deg04_pow00 + x2 * p_04_real32

    p_04_real32 = p_04_real32 / denominator_deg04

  end function p_04_real32


  real(real64) elemental function p_04_real64(x)
    !! version: experimental
    !! \( P_{4} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_04_real64 = numerator_deg04_pow02 + x2 * numerator_deg04_pow04
    p_04_real64 = numerator_deg04_pow00 + x2 * p_04_real64

    p_04_real64 = p_04_real64 / denominator_deg04

  end function p_04_real64


  real(real128) elemental function p_04_real128(x)
    !! version: experimental
    !! \( P_{4} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_04_real128 = numerator_deg04_pow02 + x2 * numerator_deg04_pow04
    p_04_real128 = numerator_deg04_pow00 + x2 * p_04_real128

    p_04_real128 = p_04_real128 / denominator_deg04

  end function p_04_real128




  real(real32) elemental function p_05_real32(x)
    !! version: experimental
    !! \( P_{5} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_05_real32 = numerator_deg05_pow03 + x2 * numerator_deg05_pow05
    p_05_real32 = numerator_deg05_pow01 + x2 * p_05_real32

    p_05_real32 = p_05_real32 * x / denominator_deg05

  end function p_05_real32


  real(real64) elemental function p_05_real64(x)
    !! version: experimental
    !! \( P_{5} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_05_real64 = numerator_deg05_pow03 + x2 * numerator_deg05_pow05
    p_05_real64 = numerator_deg05_pow01 + x2 * p_05_real64

    p_05_real64 = p_05_real64 * x / denominator_deg05

  end function p_05_real64


  real(real128) elemental function p_05_real128(x)
    !! version: experimental
    !! \( P_{5} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_05_real128 = numerator_deg05_pow03 + x2 * numerator_deg05_pow05
    p_05_real128 = numerator_deg05_pow01 + x2 * p_05_real128

    p_05_real128 = p_05_real128 * x / denominator_deg05

  end function p_05_real128




  real(real32) elemental function p_06_real32(x)
    !! version: experimental
    !! \( P_{6} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_06_real32 = numerator_deg06_pow04 + x2 * numerator_deg06_pow06
    p_06_real32 = numerator_deg06_pow02 + x2 * p_06_real32
    p_06_real32 = numerator_deg06_pow00 + x2 * p_06_real32

    p_06_real32 = p_06_real32 / denominator_deg06

  end function p_06_real32


  real(real64) elemental function p_06_real64(x)
    !! version: experimental
    !! \( P_{6} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_06_real64 = numerator_deg06_pow04 + x2 * numerator_deg06_pow06
    p_06_real64 = numerator_deg06_pow02 + x2 * p_06_real64
    p_06_real64 = numerator_deg06_pow00 + x2 * p_06_real64

    p_06_real64 = p_06_real64 / denominator_deg06

  end function p_06_real64


  real(real128) elemental function p_06_real128(x)
    !! version: experimental
    !! \( P_{6} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_06_real128 = numerator_deg06_pow04 + x2 * numerator_deg06_pow06
    p_06_real128 = numerator_deg06_pow02 + x2 * p_06_real128
    p_06_real128 = numerator_deg06_pow00 + x2 * p_06_real128

    p_06_real128 = p_06_real128 / denominator_deg06

  end function p_06_real128




  real(real32) elemental function p_07_real32(x)
    !! version: experimental
    !! \( P_{7} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_07_real32 = numerator_deg07_pow05 + x2 * numerator_deg07_pow07
    p_07_real32 = numerator_deg07_pow03 + x2 * p_07_real32
    p_07_real32 = numerator_deg07_pow01 + x2 * p_07_real32

    p_07_real32 = p_07_real32 * x / denominator_deg07

  end function p_07_real32


  real(real64) elemental function p_07_real64(x)
    !! version: experimental
    !! \( P_{7} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_07_real64 = numerator_deg07_pow05 + x2 * numerator_deg07_pow07
    p_07_real64 = numerator_deg07_pow03 + x2 * p_07_real64
    p_07_real64 = numerator_deg07_pow01 + x2 * p_07_real64

    p_07_real64 = p_07_real64 * x / denominator_deg07

  end function p_07_real64


  real(real128) elemental function p_07_real128(x)
    !! version: experimental
    !! \( P_{7} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_07_real128 = numerator_deg07_pow05 + x2 * numerator_deg07_pow07
    p_07_real128 = numerator_deg07_pow03 + x2 * p_07_real128
    p_07_real128 = numerator_deg07_pow01 + x2 * p_07_real128

    p_07_real128 = p_07_real128 * x / denominator_deg07

  end function p_07_real128




  real(real32) elemental function p_08_real32(x)
    !! version: experimental
    !! \( P_{8} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_08_real32 = numerator_deg08_pow06 + x2 * numerator_deg08_pow08
    p_08_real32 = numerator_deg08_pow04 + x2 * p_08_real32
    p_08_real32 = numerator_deg08_pow02 + x2 * p_08_real32
    p_08_real32 = numerator_deg08_pow00 + x2 * p_08_real32

    p_08_real32 = p_08_real32 / denominator_deg08

  end function p_08_real32


  real(real64) elemental function p_08_real64(x)
    !! version: experimental
    !! \( P_{8} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_08_real64 = numerator_deg08_pow06 + x2 * numerator_deg08_pow08
    p_08_real64 = numerator_deg08_pow04 + x2 * p_08_real64
    p_08_real64 = numerator_deg08_pow02 + x2 * p_08_real64
    p_08_real64 = numerator_deg08_pow00 + x2 * p_08_real64

    p_08_real64 = p_08_real64 / denominator_deg08

  end function p_08_real64


  real(real128) elemental function p_08_real128(x)
    !! version: experimental
    !! \( P_{8} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_08_real128 = numerator_deg08_pow06 + x2 * numerator_deg08_pow08
    p_08_real128 = numerator_deg08_pow04 + x2 * p_08_real128
    p_08_real128 = numerator_deg08_pow02 + x2 * p_08_real128
    p_08_real128 = numerator_deg08_pow00 + x2 * p_08_real128

    p_08_real128 = p_08_real128 / denominator_deg08

  end function p_08_real128




  real(real32) elemental function p_09_real32(x)
    !! version: experimental
    !! \( P_{9} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_09_real32 = numerator_deg09_pow07 + x2 * numerator_deg09_pow09
    p_09_real32 = numerator_deg09_pow05 + x2 * p_09_real32
    p_09_real32 = numerator_deg09_pow03 + x2 * p_09_real32
    p_09_real32 = numerator_deg09_pow01 + x2 * p_09_real32

    p_09_real32 = p_09_real32 * x / denominator_deg09

  end function p_09_real32


  real(real64) elemental function p_09_real64(x)
    !! version: experimental
    !! \( P_{9} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_09_real64 = numerator_deg09_pow07 + x2 * numerator_deg09_pow09
    p_09_real64 = numerator_deg09_pow05 + x2 * p_09_real64
    p_09_real64 = numerator_deg09_pow03 + x2 * p_09_real64
    p_09_real64 = numerator_deg09_pow01 + x2 * p_09_real64

    p_09_real64 = p_09_real64 * x / denominator_deg09

  end function p_09_real64


  real(real128) elemental function p_09_real128(x)
    !! version: experimental
    !! \( P_{9} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_09_real128 = numerator_deg09_pow07 + x2 * numerator_deg09_pow09
    p_09_real128 = numerator_deg09_pow05 + x2 * p_09_real128
    p_09_real128 = numerator_deg09_pow03 + x2 * p_09_real128
    p_09_real128 = numerator_deg09_pow01 + x2 * p_09_real128

    p_09_real128 = p_09_real128 * x / denominator_deg09

  end function p_09_real128




  real(real32) elemental function p_10_real32(x)
    !! version: experimental
    !! \( P_{10} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_10_real32 = numerator_deg10_pow08 + x2 * numerator_deg10_pow10
    p_10_real32 = numerator_deg10_pow06 + x2 * p_10_real32
    p_10_real32 = numerator_deg10_pow04 + x2 * p_10_real32
    p_10_real32 = numerator_deg10_pow02 + x2 * p_10_real32
    p_10_real32 = numerator_deg10_pow00 + x2 * p_10_real32

    p_10_real32 = p_10_real32 / denominator_deg10

  end function p_10_real32


  real(real64) elemental function p_10_real64(x)
    !! version: experimental
    !! \( P_{10} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_10_real64 = numerator_deg10_pow08 + x2 * numerator_deg10_pow10
    p_10_real64 = numerator_deg10_pow06 + x2 * p_10_real64
    p_10_real64 = numerator_deg10_pow04 + x2 * p_10_real64
    p_10_real64 = numerator_deg10_pow02 + x2 * p_10_real64
    p_10_real64 = numerator_deg10_pow00 + x2 * p_10_real64

    p_10_real64 = p_10_real64 / denominator_deg10

  end function p_10_real64


  real(real128) elemental function p_10_real128(x)
    !! version: experimental
    !! \( P_{10} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_10_real128 = numerator_deg10_pow08 + x2 * numerator_deg10_pow10
    p_10_real128 = numerator_deg10_pow06 + x2 * p_10_real128
    p_10_real128 = numerator_deg10_pow04 + x2 * p_10_real128
    p_10_real128 = numerator_deg10_pow02 + x2 * p_10_real128
    p_10_real128 = numerator_deg10_pow00 + x2 * p_10_real128

    p_10_real128 = p_10_real128 / denominator_deg10

  end function p_10_real128




  real(real32) elemental function p_11_real32(x)
    !! version: experimental
    !! \( P_{11} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_11_real32 = numerator_deg11_pow09 + x2 * numerator_deg11_pow11
    p_11_real32 = numerator_deg11_pow07 + x2 * p_11_real32
    p_11_real32 = numerator_deg11_pow05 + x2 * p_11_real32
    p_11_real32 = numerator_deg11_pow03 + x2 * p_11_real32
    p_11_real32 = numerator_deg11_pow01 + x2 * p_11_real32

    p_11_real32 = p_11_real32 * x / denominator_deg11

  end function p_11_real32


  real(real64) elemental function p_11_real64(x)
    !! version: experimental
    !! \( P_{11} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_11_real64 = numerator_deg11_pow09 + x2 * numerator_deg11_pow11
    p_11_real64 = numerator_deg11_pow07 + x2 * p_11_real64
    p_11_real64 = numerator_deg11_pow05 + x2 * p_11_real64
    p_11_real64 = numerator_deg11_pow03 + x2 * p_11_real64
    p_11_real64 = numerator_deg11_pow01 + x2 * p_11_real64

    p_11_real64 = p_11_real64 * x / denominator_deg11

  end function p_11_real64


  real(real128) elemental function p_11_real128(x)
    !! version: experimental
    !! \( P_{11} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_11_real128 = numerator_deg11_pow09 + x2 * numerator_deg11_pow11
    p_11_real128 = numerator_deg11_pow07 + x2 * p_11_real128
    p_11_real128 = numerator_deg11_pow05 + x2 * p_11_real128
    p_11_real128 = numerator_deg11_pow03 + x2 * p_11_real128
    p_11_real128 = numerator_deg11_pow01 + x2 * p_11_real128

    p_11_real128 = p_11_real128 * x / denominator_deg11

  end function p_11_real128




  real(real32) elemental function p_12_real32(x)
    !! version: experimental
    !! \( P_{12} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_12_real32 = numerator_deg12_pow10 + x2 * numerator_deg12_pow12
    p_12_real32 = numerator_deg12_pow08 + x2 * p_12_real32
    p_12_real32 = numerator_deg12_pow06 + x2 * p_12_real32
    p_12_real32 = numerator_deg12_pow04 + x2 * p_12_real32
    p_12_real32 = numerator_deg12_pow02 + x2 * p_12_real32
    p_12_real32 = numerator_deg12_pow00 + x2 * p_12_real32

    p_12_real32 = p_12_real32 / denominator_deg12

  end function p_12_real32


  real(real64) elemental function p_12_real64(x)
    !! version: experimental
    !! \( P_{12} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_12_real64 = numerator_deg12_pow10 + x2 * numerator_deg12_pow12
    p_12_real64 = numerator_deg12_pow08 + x2 * p_12_real64
    p_12_real64 = numerator_deg12_pow06 + x2 * p_12_real64
    p_12_real64 = numerator_deg12_pow04 + x2 * p_12_real64
    p_12_real64 = numerator_deg12_pow02 + x2 * p_12_real64
    p_12_real64 = numerator_deg12_pow00 + x2 * p_12_real64

    p_12_real64 = p_12_real64 / denominator_deg12

  end function p_12_real64


  real(real128) elemental function p_12_real128(x)
    !! version: experimental
    !! \( P_{12} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_12_real128 = numerator_deg12_pow10 + x2 * numerator_deg12_pow12
    p_12_real128 = numerator_deg12_pow08 + x2 * p_12_real128
    p_12_real128 = numerator_deg12_pow06 + x2 * p_12_real128
    p_12_real128 = numerator_deg12_pow04 + x2 * p_12_real128
    p_12_real128 = numerator_deg12_pow02 + x2 * p_12_real128
    p_12_real128 = numerator_deg12_pow00 + x2 * p_12_real128

    p_12_real128 = p_12_real128 / denominator_deg12

  end function p_12_real128




  real(real32) elemental function p_13_real32(x)
    !! version: experimental
    !! \( P_{13} (x) \) for `real32`

    real(real32), intent(in) :: x


    real(real32) :: x2


    x2 = x * x

    p_13_real32 = numerator_deg13_pow11 + x2 * numerator_deg13_pow13
    p_13_real32 = numerator_deg13_pow09 + x2 * p_13_real32
    p_13_real32 = numerator_deg13_pow07 + x2 * p_13_real32
    p_13_real32 = numerator_deg13_pow05 + x2 * p_13_real32
    p_13_real32 = numerator_deg13_pow03 + x2 * p_13_real32
    p_13_real32 = numerator_deg13_pow01 + x2 * p_13_real32

    p_13_real32 = p_13_real32 * x / denominator_deg13

  end function p_13_real32


  real(real64) elemental function p_13_real64(x)
    !! version: experimental
    !! \( P_{13} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_13_real64 = numerator_deg13_pow11 + x2 * numerator_deg13_pow13
    p_13_real64 = numerator_deg13_pow09 + x2 * p_13_real64
    p_13_real64 = numerator_deg13_pow07 + x2 * p_13_real64
    p_13_real64 = numerator_deg13_pow05 + x2 * p_13_real64
    p_13_real64 = numerator_deg13_pow03 + x2 * p_13_real64
    p_13_real64 = numerator_deg13_pow01 + x2 * p_13_real64

    p_13_real64 = p_13_real64 * x / denominator_deg13

  end function p_13_real64


  real(real128) elemental function p_13_real128(x)
    !! version: experimental
    !! \( P_{13} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_13_real128 = numerator_deg13_pow11 + x2 * numerator_deg13_pow13
    p_13_real128 = numerator_deg13_pow09 + x2 * p_13_real128
    p_13_real128 = numerator_deg13_pow07 + x2 * p_13_real128
    p_13_real128 = numerator_deg13_pow05 + x2 * p_13_real128
    p_13_real128 = numerator_deg13_pow03 + x2 * p_13_real128
    p_13_real128 = numerator_deg13_pow01 + x2 * p_13_real128

    p_13_real128 = p_13_real128 * x / denominator_deg13

  end function p_13_real128






  real(real64) elemental function p_14_real64(x)
    !! version: experimental
    !! \( P_{14} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_14_real64 = numerator_deg14_pow12 + x2 * numerator_deg14_pow14
    p_14_real64 = numerator_deg14_pow10 + x2 * p_14_real64
    p_14_real64 = numerator_deg14_pow08 + x2 * p_14_real64
    p_14_real64 = numerator_deg14_pow06 + x2 * p_14_real64
    p_14_real64 = numerator_deg14_pow04 + x2 * p_14_real64
    p_14_real64 = numerator_deg14_pow02 + x2 * p_14_real64
    p_14_real64 = numerator_deg14_pow00 + x2 * p_14_real64

    p_14_real64 = p_14_real64 / denominator_deg14

  end function p_14_real64


  real(real128) elemental function p_14_real128(x)
    !! version: experimental
    !! \( P_{14} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_14_real128 = numerator_deg14_pow12 + x2 * numerator_deg14_pow14
    p_14_real128 = numerator_deg14_pow10 + x2 * p_14_real128
    p_14_real128 = numerator_deg14_pow08 + x2 * p_14_real128
    p_14_real128 = numerator_deg14_pow06 + x2 * p_14_real128
    p_14_real128 = numerator_deg14_pow04 + x2 * p_14_real128
    p_14_real128 = numerator_deg14_pow02 + x2 * p_14_real128
    p_14_real128 = numerator_deg14_pow00 + x2 * p_14_real128

    p_14_real128 = p_14_real128 / denominator_deg14

  end function p_14_real128






  real(real64) elemental function p_15_real64(x)
    !! version: experimental
    !! \( P_{15} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_15_real64 = numerator_deg15_pow13 + x2 * numerator_deg15_pow15
    p_15_real64 = numerator_deg15_pow11 + x2 * p_15_real64
    p_15_real64 = numerator_deg15_pow09 + x2 * p_15_real64
    p_15_real64 = numerator_deg15_pow07 + x2 * p_15_real64
    p_15_real64 = numerator_deg15_pow05 + x2 * p_15_real64
    p_15_real64 = numerator_deg15_pow03 + x2 * p_15_real64
    p_15_real64 = numerator_deg15_pow01 + x2 * p_15_real64

    p_15_real64 = p_15_real64 * x / denominator_deg15

  end function p_15_real64


  real(real128) elemental function p_15_real128(x)
    !! version: experimental
    !! \( P_{15} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_15_real128 = numerator_deg15_pow13 + x2 * numerator_deg15_pow15
    p_15_real128 = numerator_deg15_pow11 + x2 * p_15_real128
    p_15_real128 = numerator_deg15_pow09 + x2 * p_15_real128
    p_15_real128 = numerator_deg15_pow07 + x2 * p_15_real128
    p_15_real128 = numerator_deg15_pow05 + x2 * p_15_real128
    p_15_real128 = numerator_deg15_pow03 + x2 * p_15_real128
    p_15_real128 = numerator_deg15_pow01 + x2 * p_15_real128

    p_15_real128 = p_15_real128 * x / denominator_deg15

  end function p_15_real128






  real(real64) elemental function p_16_real64(x)
    !! version: experimental
    !! \( P_{16} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_16_real64 = numerator_deg16_pow14 + x2 * numerator_deg16_pow16
    p_16_real64 = numerator_deg16_pow12 + x2 * p_16_real64
    p_16_real64 = numerator_deg16_pow10 + x2 * p_16_real64
    p_16_real64 = numerator_deg16_pow08 + x2 * p_16_real64
    p_16_real64 = numerator_deg16_pow06 + x2 * p_16_real64
    p_16_real64 = numerator_deg16_pow04 + x2 * p_16_real64
    p_16_real64 = numerator_deg16_pow02 + x2 * p_16_real64
    p_16_real64 = numerator_deg16_pow00 + x2 * p_16_real64

    p_16_real64 = p_16_real64 / denominator_deg16

  end function p_16_real64


  real(real128) elemental function p_16_real128(x)
    !! version: experimental
    !! \( P_{16} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_16_real128 = numerator_deg16_pow14 + x2 * numerator_deg16_pow16
    p_16_real128 = numerator_deg16_pow12 + x2 * p_16_real128
    p_16_real128 = numerator_deg16_pow10 + x2 * p_16_real128
    p_16_real128 = numerator_deg16_pow08 + x2 * p_16_real128
    p_16_real128 = numerator_deg16_pow06 + x2 * p_16_real128
    p_16_real128 = numerator_deg16_pow04 + x2 * p_16_real128
    p_16_real128 = numerator_deg16_pow02 + x2 * p_16_real128
    p_16_real128 = numerator_deg16_pow00 + x2 * p_16_real128

    p_16_real128 = p_16_real128 / denominator_deg16

  end function p_16_real128






  real(real64) elemental function p_17_real64(x)
    !! version: experimental
    !! \( P_{17} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_17_real64 = numerator_deg17_pow15 + x2 * numerator_deg17_pow17
    p_17_real64 = numerator_deg17_pow13 + x2 * p_17_real64
    p_17_real64 = numerator_deg17_pow11 + x2 * p_17_real64
    p_17_real64 = numerator_deg17_pow09 + x2 * p_17_real64
    p_17_real64 = numerator_deg17_pow07 + x2 * p_17_real64
    p_17_real64 = numerator_deg17_pow05 + x2 * p_17_real64
    p_17_real64 = numerator_deg17_pow03 + x2 * p_17_real64
    p_17_real64 = numerator_deg17_pow01 + x2 * p_17_real64

    p_17_real64 = p_17_real64 * x / denominator_deg17

  end function p_17_real64


  real(real128) elemental function p_17_real128(x)
    !! version: experimental
    !! \( P_{17} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_17_real128 = numerator_deg17_pow15 + x2 * numerator_deg17_pow17
    p_17_real128 = numerator_deg17_pow13 + x2 * p_17_real128
    p_17_real128 = numerator_deg17_pow11 + x2 * p_17_real128
    p_17_real128 = numerator_deg17_pow09 + x2 * p_17_real128
    p_17_real128 = numerator_deg17_pow07 + x2 * p_17_real128
    p_17_real128 = numerator_deg17_pow05 + x2 * p_17_real128
    p_17_real128 = numerator_deg17_pow03 + x2 * p_17_real128
    p_17_real128 = numerator_deg17_pow01 + x2 * p_17_real128

    p_17_real128 = p_17_real128 * x / denominator_deg17

  end function p_17_real128






  real(real64) elemental function p_18_real64(x)
    !! version: experimental
    !! \( P_{18} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_18_real64 = numerator_deg18_pow16 + x2 * numerator_deg18_pow18
    p_18_real64 = numerator_deg18_pow14 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow12 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow10 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow08 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow06 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow04 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow02 + x2 * p_18_real64
    p_18_real64 = numerator_deg18_pow00 + x2 * p_18_real64

    p_18_real64 = p_18_real64 / denominator_deg18

  end function p_18_real64


  real(real128) elemental function p_18_real128(x)
    !! version: experimental
    !! \( P_{18} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_18_real128 = numerator_deg18_pow16 + x2 * numerator_deg18_pow18
    p_18_real128 = numerator_deg18_pow14 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow12 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow10 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow08 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow06 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow04 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow02 + x2 * p_18_real128
    p_18_real128 = numerator_deg18_pow00 + x2 * p_18_real128

    p_18_real128 = p_18_real128 / denominator_deg18

  end function p_18_real128






  real(real64) elemental function p_19_real64(x)
    !! version: experimental
    !! \( P_{19} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_19_real64 = numerator_deg19_pow17 + x2 * numerator_deg19_pow19
    p_19_real64 = numerator_deg19_pow15 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow13 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow11 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow09 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow07 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow05 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow03 + x2 * p_19_real64
    p_19_real64 = numerator_deg19_pow01 + x2 * p_19_real64

    p_19_real64 = p_19_real64 * x / denominator_deg19

  end function p_19_real64


  real(real128) elemental function p_19_real128(x)
    !! version: experimental
    !! \( P_{19} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_19_real128 = numerator_deg19_pow17 + x2 * numerator_deg19_pow19
    p_19_real128 = numerator_deg19_pow15 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow13 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow11 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow09 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow07 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow05 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow03 + x2 * p_19_real128
    p_19_real128 = numerator_deg19_pow01 + x2 * p_19_real128

    p_19_real128 = p_19_real128 * x / denominator_deg19

  end function p_19_real128






  real(real64) elemental function p_20_real64(x)
    !! version: experimental
    !! \( P_{20} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_20_real64 = numerator_deg20_pow18 + x2 * numerator_deg20_pow20
    p_20_real64 = numerator_deg20_pow16 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow14 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow12 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow10 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow08 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow06 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow04 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow02 + x2 * p_20_real64
    p_20_real64 = numerator_deg20_pow00 + x2 * p_20_real64

    p_20_real64 = p_20_real64 / denominator_deg20

  end function p_20_real64


  real(real128) elemental function p_20_real128(x)
    !! version: experimental
    !! \( P_{20} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_20_real128 = numerator_deg20_pow18 + x2 * numerator_deg20_pow20
    p_20_real128 = numerator_deg20_pow16 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow14 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow12 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow10 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow08 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow06 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow04 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow02 + x2 * p_20_real128
    p_20_real128 = numerator_deg20_pow00 + x2 * p_20_real128

    p_20_real128 = p_20_real128 / denominator_deg20

  end function p_20_real128






  real(real64) elemental function p_21_real64(x)
    !! version: experimental
    !! \( P_{21} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_21_real64 = numerator_deg21_pow19 + x2 * numerator_deg21_pow21
    p_21_real64 = numerator_deg21_pow17 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow15 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow13 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow11 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow09 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow07 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow05 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow03 + x2 * p_21_real64
    p_21_real64 = numerator_deg21_pow01 + x2 * p_21_real64

    p_21_real64 = p_21_real64 * x / denominator_deg21

  end function p_21_real64


  real(real128) elemental function p_21_real128(x)
    !! version: experimental
    !! \( P_{21} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_21_real128 = numerator_deg21_pow19 + x2 * numerator_deg21_pow21
    p_21_real128 = numerator_deg21_pow17 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow15 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow13 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow11 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow09 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow07 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow05 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow03 + x2 * p_21_real128
    p_21_real128 = numerator_deg21_pow01 + x2 * p_21_real128

    p_21_real128 = p_21_real128 * x / denominator_deg21

  end function p_21_real128






  real(real64) elemental function p_22_real64(x)
    !! version: experimental
    !! \( P_{22} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_22_real64 = numerator_deg22_pow20 + x2 * numerator_deg22_pow22
    p_22_real64 = numerator_deg22_pow18 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow16 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow14 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow12 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow10 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow08 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow06 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow04 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow02 + x2 * p_22_real64
    p_22_real64 = numerator_deg22_pow00 + x2 * p_22_real64

    p_22_real64 = p_22_real64 / denominator_deg22

  end function p_22_real64


  real(real128) elemental function p_22_real128(x)
    !! version: experimental
    !! \( P_{22} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_22_real128 = numerator_deg22_pow20 + x2 * numerator_deg22_pow22
    p_22_real128 = numerator_deg22_pow18 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow16 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow14 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow12 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow10 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow08 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow06 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow04 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow02 + x2 * p_22_real128
    p_22_real128 = numerator_deg22_pow00 + x2 * p_22_real128

    p_22_real128 = p_22_real128 / denominator_deg22

  end function p_22_real128






  real(real64) elemental function p_23_real64(x)
    !! version: experimental
    !! \( P_{23} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_23_real64 = numerator_deg23_pow21 + x2 * numerator_deg23_pow23
    p_23_real64 = numerator_deg23_pow19 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow17 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow15 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow13 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow11 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow09 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow07 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow05 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow03 + x2 * p_23_real64
    p_23_real64 = numerator_deg23_pow01 + x2 * p_23_real64

    p_23_real64 = p_23_real64 * x / denominator_deg23

  end function p_23_real64


  real(real128) elemental function p_23_real128(x)
    !! version: experimental
    !! \( P_{23} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_23_real128 = numerator_deg23_pow21 + x2 * numerator_deg23_pow23
    p_23_real128 = numerator_deg23_pow19 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow17 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow15 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow13 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow11 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow09 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow07 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow05 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow03 + x2 * p_23_real128
    p_23_real128 = numerator_deg23_pow01 + x2 * p_23_real128

    p_23_real128 = p_23_real128 * x / denominator_deg23

  end function p_23_real128






  real(real64) elemental function p_24_real64(x)
    !! version: experimental
    !! \( P_{24} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_24_real64 = numerator_deg24_pow22 + x2 * numerator_deg24_pow24
    p_24_real64 = numerator_deg24_pow20 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow18 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow16 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow14 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow12 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow10 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow08 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow06 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow04 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow02 + x2 * p_24_real64
    p_24_real64 = numerator_deg24_pow00 + x2 * p_24_real64

    p_24_real64 = p_24_real64 / denominator_deg24

  end function p_24_real64


  real(real128) elemental function p_24_real128(x)
    !! version: experimental
    !! \( P_{24} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_24_real128 = numerator_deg24_pow22 + x2 * numerator_deg24_pow24
    p_24_real128 = numerator_deg24_pow20 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow18 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow16 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow14 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow12 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow10 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow08 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow06 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow04 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow02 + x2 * p_24_real128
    p_24_real128 = numerator_deg24_pow00 + x2 * p_24_real128

    p_24_real128 = p_24_real128 / denominator_deg24

  end function p_24_real128






  real(real64) elemental function p_25_real64(x)
    !! version: experimental
    !! \( P_{25} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_25_real64 = numerator_deg25_pow23 + x2 * numerator_deg25_pow25
    p_25_real64 = numerator_deg25_pow21 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow19 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow17 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow15 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow13 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow11 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow09 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow07 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow05 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow03 + x2 * p_25_real64
    p_25_real64 = numerator_deg25_pow01 + x2 * p_25_real64

    p_25_real64 = p_25_real64 * x / denominator_deg25

  end function p_25_real64


  real(real128) elemental function p_25_real128(x)
    !! version: experimental
    !! \( P_{25} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_25_real128 = numerator_deg25_pow23 + x2 * numerator_deg25_pow25
    p_25_real128 = numerator_deg25_pow21 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow19 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow17 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow15 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow13 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow11 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow09 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow07 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow05 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow03 + x2 * p_25_real128
    p_25_real128 = numerator_deg25_pow01 + x2 * p_25_real128

    p_25_real128 = p_25_real128 * x / denominator_deg25

  end function p_25_real128






  real(real64) elemental function p_26_real64(x)
    !! version: experimental
    !! \( P_{26} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_26_real64 = numerator_deg26_pow24 + x2 * numerator_deg26_pow26
    p_26_real64 = numerator_deg26_pow22 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow20 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow18 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow16 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow14 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow12 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow10 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow08 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow06 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow04 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow02 + x2 * p_26_real64
    p_26_real64 = numerator_deg26_pow00 + x2 * p_26_real64

    p_26_real64 = p_26_real64 / denominator_deg26

  end function p_26_real64


  real(real128) elemental function p_26_real128(x)
    !! version: experimental
    !! \( P_{26} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_26_real128 = numerator_deg26_pow24 + x2 * numerator_deg26_pow26
    p_26_real128 = numerator_deg26_pow22 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow20 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow18 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow16 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow14 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow12 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow10 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow08 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow06 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow04 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow02 + x2 * p_26_real128
    p_26_real128 = numerator_deg26_pow00 + x2 * p_26_real128

    p_26_real128 = p_26_real128 / denominator_deg26

  end function p_26_real128






  real(real64) elemental function p_27_real64(x)
    !! version: experimental
    !! \( P_{27} (x) \) for `real64`

    real(real64), intent(in) :: x


    real(real64) :: x2


    x2 = x * x

    p_27_real64 = numerator_deg27_pow25 + x2 * numerator_deg27_pow27
    p_27_real64 = numerator_deg27_pow23 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow21 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow19 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow17 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow15 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow13 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow11 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow09 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow07 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow05 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow03 + x2 * p_27_real64
    p_27_real64 = numerator_deg27_pow01 + x2 * p_27_real64

    p_27_real64 = p_27_real64 * x / denominator_deg27

  end function p_27_real64


  real(real128) elemental function p_27_real128(x)
    !! version: experimental
    !! \( P_{27} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_27_real128 = numerator_deg27_pow25 + x2 * numerator_deg27_pow27
    p_27_real128 = numerator_deg27_pow23 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow21 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow19 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow17 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow15 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow13 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow11 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow09 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow07 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow05 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow03 + x2 * p_27_real128
    p_27_real128 = numerator_deg27_pow01 + x2 * p_27_real128

    p_27_real128 = p_27_real128 * x / denominator_deg27

  end function p_27_real128








  real(real128) elemental function p_28_real128(x)
    !! version: experimental
    !! \( P_{28} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_28_real128 = numerator_deg28_pow26 + x2 * numerator_deg28_pow28
    p_28_real128 = numerator_deg28_pow24 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow22 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow20 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow18 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow16 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow14 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow12 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow10 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow08 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow06 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow04 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow02 + x2 * p_28_real128
    p_28_real128 = numerator_deg28_pow00 + x2 * p_28_real128

    p_28_real128 = p_28_real128 / denominator_deg28

  end function p_28_real128








  real(real128) elemental function p_29_real128(x)
    !! version: experimental
    !! \( P_{29} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_29_real128 = numerator_deg29_pow27 + x2 * numerator_deg29_pow29
    p_29_real128 = numerator_deg29_pow25 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow23 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow21 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow19 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow17 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow15 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow13 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow11 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow09 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow07 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow05 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow03 + x2 * p_29_real128
    p_29_real128 = numerator_deg29_pow01 + x2 * p_29_real128

    p_29_real128 = p_29_real128 * x / denominator_deg29

  end function p_29_real128








  real(real128) elemental function p_30_real128(x)
    !! version: experimental
    !! \( P_{30} (x) \) for `real128`

    real(real128), intent(in) :: x


    real(real128) :: x2


    x2 = x * x

    p_30_real128 = numerator_deg30_pow28 + x2 * numerator_deg30_pow30
    p_30_real128 = numerator_deg30_pow26 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow24 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow22 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow20 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow18 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow16 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow14 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow12 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow10 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow08 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow06 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow04 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow02 + x2 * p_30_real128
    p_30_real128 = numerator_deg30_pow00 + x2 * p_30_real128

    p_30_real128 = p_30_real128 / denominator_deg30

  end function p_30_real128




  real(real32) elemental function p_n_real32(degree, x)
    !! version: experimental
    !! \( P_n (x) \ ( n = 0 , 1, \dots , 13 ) \) for `real32`

    integer(int32), intent(in) :: degree

    real(real32), intent(in) :: x


    select case (degree)

      case (  0_int32 ); p_n_real32 = p_00(x)
      case (  1_int32 ); p_n_real32 = p_01(x)
      case (  2_int32 ); p_n_real32 = p_02(x)
      case (  3_int32 ); p_n_real32 = p_03(x)
      case (  4_int32 ); p_n_real32 = p_04(x)
      case (  5_int32 ); p_n_real32 = p_05(x)
      case (  6_int32 ); p_n_real32 = p_06(x)
      case (  7_int32 ); p_n_real32 = p_07(x)
      case (  8_int32 ); p_n_real32 = p_08(x)
      case (  9_int32 ); p_n_real32 = p_09(x)
      case ( 10_int32 ); p_n_real32 = p_10(x)
      case ( 11_int32 ); p_n_real32 = p_11(x)
      case ( 12_int32 ); p_n_real32 = p_12(x)
      case ( 13_int32 ); p_n_real32 = p_13(x)

      case default; p_n_real32 = ieee_value(x, ieee_signaling_nan)

    end select

  end function p_n_real32


  real(real64) elemental function p_n_real64(degree, x)
    !! version: experimental
    !! \( P_n (x) \ ( n = 0 , 1, \dots , 27 ) \) for `real64`

    integer(int32), intent(in) :: degree

    real(real64), intent(in) :: x


    select case (degree)

      case (  0_int32 ); p_n_real64 = p_00(x)
      case (  1_int32 ); p_n_real64 = p_01(x)
      case (  2_int32 ); p_n_real64 = p_02(x)
      case (  3_int32 ); p_n_real64 = p_03(x)
      case (  4_int32 ); p_n_real64 = p_04(x)
      case (  5_int32 ); p_n_real64 = p_05(x)
      case (  6_int32 ); p_n_real64 = p_06(x)
      case (  7_int32 ); p_n_real64 = p_07(x)
      case (  8_int32 ); p_n_real64 = p_08(x)
      case (  9_int32 ); p_n_real64 = p_09(x)
      case ( 10_int32 ); p_n_real64 = p_10(x)
      case ( 11_int32 ); p_n_real64 = p_11(x)
      case ( 12_int32 ); p_n_real64 = p_12(x)
      case ( 13_int32 ); p_n_real64 = p_13(x)
      case ( 14_int32 ); p_n_real64 = p_14(x)
      case ( 15_int32 ); p_n_real64 = p_15(x)
      case ( 16_int32 ); p_n_real64 = p_16(x)
      case ( 17_int32 ); p_n_real64 = p_17(x)
      case ( 18_int32 ); p_n_real64 = p_18(x)
      case ( 19_int32 ); p_n_real64 = p_19(x)
      case ( 20_int32 ); p_n_real64 = p_20(x)
      case ( 21_int32 ); p_n_real64 = p_21(x)
      case ( 22_int32 ); p_n_real64 = p_22(x)
      case ( 23_int32 ); p_n_real64 = p_23(x)
      case ( 24_int32 ); p_n_real64 = p_24(x)
      case ( 25_int32 ); p_n_real64 = p_25(x)
      case ( 26_int32 ); p_n_real64 = p_26(x)
      case ( 27_int32 ); p_n_real64 = p_27(x)

      case default; p_n_real64 = ieee_value(x, ieee_signaling_nan)

    end select

  end function p_n_real64


  real(real128) elemental function p_n_real128(degree, x)
    !! version: experimental
    !! \( P_n (x) \ ( n = 0 , 1, \dots , 30 ) \) for `real128`

    integer(int32), intent(in) :: degree

    real(real128), intent(in) :: x


    select case (degree)

      case (  0_int32 ); p_n_real128 = p_00(x)
      case (  1_int32 ); p_n_real128 = p_01(x)
      case (  2_int32 ); p_n_real128 = p_02(x)
      case (  3_int32 ); p_n_real128 = p_03(x)
      case (  4_int32 ); p_n_real128 = p_04(x)
      case (  5_int32 ); p_n_real128 = p_05(x)
      case (  6_int32 ); p_n_real128 = p_06(x)
      case (  7_int32 ); p_n_real128 = p_07(x)
      case (  8_int32 ); p_n_real128 = p_08(x)
      case (  9_int32 ); p_n_real128 = p_09(x)
      case ( 10_int32 ); p_n_real128 = p_10(x)
      case ( 11_int32 ); p_n_real128 = p_11(x)
      case ( 12_int32 ); p_n_real128 = p_12(x)
      case ( 13_int32 ); p_n_real128 = p_13(x)
      case ( 14_int32 ); p_n_real128 = p_14(x)
      case ( 15_int32 ); p_n_real128 = p_15(x)
      case ( 16_int32 ); p_n_real128 = p_16(x)
      case ( 17_int32 ); p_n_real128 = p_17(x)
      case ( 18_int32 ); p_n_real128 = p_18(x)
      case ( 19_int32 ); p_n_real128 = p_19(x)
      case ( 20_int32 ); p_n_real128 = p_20(x)
      case ( 21_int32 ); p_n_real128 = p_21(x)
      case ( 22_int32 ); p_n_real128 = p_22(x)
      case ( 23_int32 ); p_n_real128 = p_23(x)
      case ( 24_int32 ); p_n_real128 = p_24(x)
      case ( 25_int32 ); p_n_real128 = p_25(x)
      case ( 26_int32 ); p_n_real128 = p_26(x)
      case ( 27_int32 ); p_n_real128 = p_27(x)
      case ( 28_int32 ); p_n_real128 = p_28(x)
      case ( 29_int32 ); p_n_real128 = p_29(x)
      case ( 30_int32 ); p_n_real128 = p_30(x)

      case default; p_n_real128 = ieee_value(x, ieee_signaling_nan)

    end select

  end function p_n_real128

end module legendre_polynomial_closed_form_fortran
