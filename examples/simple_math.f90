module simple_math
  implicit none

contains

  function add_numbers(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    real :: work

    work = a + b
    c = a + 1.0
    c = c + 2.0 + work

    return
  end function add_numbers

  subroutine multiply_numbers(a, b, c)
    real, intent(in) :: a, b
    real, intent(out) :: c

    c = a * b
    c = c * 3.0 + a

    return
  end subroutine multiply_numbers

  function subtract_numbers(a, b) result(c)
    real, intent(in) :: a, b
    real :: c

    c = a - b

    return
  end function subtract_numbers

  subroutine divide_numbers(a, b, c)
    real, intent(in) :: a, b
    real, intent(out) :: c

    c = a / b
    c = c / 2.0 + a

    return
  end subroutine divide_numbers

  function power_numbers(a, b) result(c)
    real, intent(in) :: a, b
    real :: c

    c = a ** b

    return
  end function power_numbers

end module simple_math
