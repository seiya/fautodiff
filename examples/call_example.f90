module call_example
contains
  subroutine foo(a)
    real, intent(inout) :: a
    a = a * 2.0
  end subroutine foo
  subroutine bar(x)
    real, intent(inout) :: x
    call foo(x)
  end subroutine bar
end module call_example
