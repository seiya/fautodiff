program run_add
  use simple_math
  use simple_math_ad
  real :: a, b, c
  real :: a_ad, b_ad, c_ad

  a = 2.0
  b = 3.0
  c = add_numbers(a, b)

  a_ad = 0.0
  b_ad = 0.0
  c_ad = 1.0
  call add_numbers_ad(a, a_ad, b, b_ad, c_ad)

  print *, c, a_ad, b_ad
end program run_add
