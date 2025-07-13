module save_vars_ad
  use save_vars
  implicit none

contains

  subroutine simple_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(in)  :: y
    real, intent(in)  :: y_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad
    real :: work_ad
    real :: work

    work_ad = x_ad ! work = x + 1.0
    work = x + 1.0
    z_ad = work_ad + y_ad ! z = work + y
    z = work + y
    work_ad = work_ad * 2.0 * work ! work = work**2
    work = work**2
    z_ad = work_ad * x + x_ad * work + z_ad ! z = work * x + z
    z = work * x + z
    work_ad = x_ad * 2.0 * x ! work = x**2
    work = x**2
    z_ad = work_ad * x + x_ad * work + z_ad ! z = work * x + z
    z = work * x + z

    return
  end subroutine simple_fwd_ad

  subroutine simple_rev_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real, intent(inout) :: z_ad
    real :: work_ad
    real :: work
    real :: work_save_19_ad
    real :: work_save_21_ad

    work = x + 1.0
    work_save_19_ad = work
    work = work**2
    work_save_21_ad = work
    work = x**2

    work_ad = z_ad * x ! z = work * x + z
    x_ad = z_ad * work ! z = work * x + z
    work = work_save_21_ad
    x_ad = work_ad * 2.0 * x + x_ad ! work = x**2
    work_ad = z_ad * x ! z = work * x + z
    x_ad = z_ad * work + x_ad ! z = work * x + z
    work = work_save_19_ad
    work_ad = work_ad * 2.0 * work ! work = work**2
    work_ad = z_ad + work_ad ! z = work + y
    y_ad = z_ad ! z = work + y
    z_ad = 0.0 ! z = work + y
    x_ad = work_ad + x_ad ! work = x + 1.0

    return
  end subroutine simple_rev_ad

  subroutine if_example_fwd_ad(x, x_ad, y, y_ad, z, z_ad)
    real, intent(in)  :: x
    real, intent(in)  :: x_ad
    real, intent(in)  :: y
    real, intent(in)  :: y_ad
    real, intent(out) :: z
    real, intent(out) :: z_ad
    real :: work_ad
    real :: work

    work_ad = x_ad ! work = x + 1.0
    work = x + 1.0
    z_ad = work_ad * y + y_ad * work ! z = work * y
    z = work * y
    if (work > 0.0) then
      work_ad = work_ad * 2.0 * work ! work = work**2
      work = work**2
      z_ad = work_ad * x + x_ad * work + z_ad ! z = work * x + z
      z = work * x + z
    else if (work < 0.0) then
      work_ad = x_ad ! work = x
      work = x
      z_ad = work_ad * y + y_ad * work ! z = work * y
      z = work * y
      work_ad = work_ad * x + x_ad * work ! work = work * x
      work = work * x
    else
      z_ad = work_ad * x + x_ad * work ! z = work * x
      z = work * x
    end if
    work_ad = work_ad * x + x_ad * work ! work = work * x
    work = work * x
    z_ad = work_ad * x + x_ad * work + z_ad ! z = work * x + z
    z = work * x + z

    return
  end subroutine if_example_fwd_ad

  subroutine if_example_rev_ad(x, x_ad, y, y_ad, z_ad)
    real, intent(in)  :: x
    real, intent(out) :: x_ad
    real, intent(in)  :: y
    real, intent(out) :: y_ad
    real, intent(inout) :: z_ad
    real :: work_ad
    real :: work
    real :: work_save_47_ad
    real :: work_save_39_ad
    real :: work_save_48_ad

    work = x + 1.0
    work_save_47_ad = work
    if (work > 0.0) then
      work = work**2
    else if (work < 0.0) then
      work = x
      work = work * x
    end if
    work_save_48_ad = work
    work = work * x

    y_ad = 0.0

    work_ad = z_ad * x ! z = work * x + z
    x_ad = z_ad * work ! z = work * x + z
    work = work_save_48_ad
    x_ad = work_ad * work + x_ad ! work = work * x
    work_ad = work_ad * x ! work = work * x
    work = work_save_47_ad
    if (work > 0.0) then
      work_save_39_ad = work
      work = work**2
      work_ad = z_ad * x + work_ad ! z = work * x + z
      x_ad = z_ad * work + x_ad ! z = work * x + z
      work = work_save_39_ad
      work_ad = work_ad * 2.0 * work ! work = work**2
    else if (work < 0.0) then
      work = x
      x_ad = work_ad * work + x_ad ! work = work * x
      work_ad = work_ad * x ! work = work * x
      work_ad = z_ad * y + work_ad ! z = work * y
      y_ad = z_ad * work ! z = work * y
      z_ad = 0.0 ! z = work * y
      x_ad = work_ad + x_ad ! work = x
      work_ad = 0.0 ! work = x
    else
      work_ad = z_ad * x + work_ad ! z = work * x
      x_ad = z_ad * work + x_ad ! z = work * x
      z_ad = 0.0 ! z = work * x
    end if
    work = work_save_47_ad
    work_ad = z_ad * y + work_ad ! z = work * y
    y_ad = z_ad * work + y_ad ! z = work * y
    z_ad = 0.0 ! z = work * y
    x_ad = work_ad + x_ad ! work = x + 1.0

    return
  end subroutine if_example_rev_ad

  subroutine do_with_array_private_fwd_ad(n, m, x, x_ad, y, y_ad, z, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(in)  :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(in)  :: y_ad(n,m)
    real, intent(out) :: z(n,m)
    real, intent(out) :: z_ad(n,m)
    real :: ary_ad(n,m)
    real :: scalar_ad
    real :: ary(n,m)
    integer :: i
    integer :: j
    real :: scalar

    ary_ad(:,:) = x_ad(:,:) ! ary(:,:) = x(:,:)
    ary(:,:) = x(:,:)
    do j = 1, m
      do i = 1, n
        z_ad(i,j) = ary_ad(i,j) * y(i,j) + y_ad(i,j) * ary(i,j) ! z(i,j) = ary(i,j) * y(i,j)
        z(i,j) = ary(i,j) * y(i,j)
        ary_ad(i,j) = x_ad(i,j) * ary(i,j) + ary_ad(i,j) * x(i,j) + z_ad(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        scalar_ad = ary_ad(i,j) * z(i,j) + z_ad(i,j) * ary(i,j) ! scalar = ary(i,j) * z(i,j)
        scalar = ary(i,j) * z(i,j)
        z_ad(i,j) = x_ad(i,j) + scalar_ad ! z(i,j) = x(i,j) + scalar
        z(i,j) = x(i,j) + scalar
        z_ad(i,j) = y_ad(i,j) * scalar + scalar_ad * y(i,j) + z_ad(i,j) ! z(i,j) = y(i,j) * scalar + z(i,j)
        z(i,j) = y(i,j) * scalar + z(i,j)
        scalar_ad = z_ad(i,j) * y(i,j) + y_ad(i,j) * z(i,j) ! scalar = z(i,j) * y(i,j)
        scalar = z(i,j) * y(i,j)
        z_ad(i,j) = z_ad(i,j) * scalar + scalar_ad * z(i,j) ! z(i,j) = z(i,j) * scalar
        z(i,j) = z(i,j) * scalar
      end do
    end do

    return
  end subroutine do_with_array_private_fwd_ad

  subroutine do_with_array_private_rev_ad(n, m, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(out) :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    real, intent(inout) :: z_ad(n,m)
    real :: ary_ad(n,m)
    real :: scalar_ad
    real :: ary(n,m)
    integer :: i
    integer :: j
    real :: z(n,m)
    real :: scalar
    real :: ary_save_77_ad
    real :: z_save_79_ad
    real :: scalar_save_81_ad

    ary(:,:) = x(:,:)

    do j = m, 1, - 1
      do i = n, 1, - 1
        z(i,j) = ary(i,j) * y(i,j)
        ary_save_77_ad = ary(i,j)
        ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        scalar = ary(i,j) * z(i,j)
        z_save_79_ad = z(i,j)
        z(i,j) = x(i,j) + scalar
        z(i,j) = y(i,j) * scalar + z(i,j)
        scalar_save_81_ad = scalar
        scalar = z(i,j) * y(i,j)
        scalar_ad = z_ad(i,j) * z(i,j) ! z(i,j) = z(i,j) * scalar
        z_ad(i,j) = z_ad(i,j) * scalar ! z(i,j) = z(i,j) * scalar
        scalar = scalar_save_81_ad
        z_ad(i,j) = scalar_ad * y(i,j) + z_ad(i,j) ! scalar = z(i,j) * y(i,j)
        y_ad(i,j) = scalar_ad * z(i,j) ! scalar = z(i,j) * y(i,j)
        y_ad(i,j) = z_ad(i,j) * scalar + y_ad(i,j) ! z(i,j) = y(i,j) * scalar + z(i,j)
        scalar_ad = z_ad(i,j) * y(i,j) ! z(i,j) = y(i,j) * scalar + z(i,j)
        z(i,j) = z_save_79_ad
        x_ad(i,j) = z_ad(i,j) ! z(i,j) = x(i,j) + scalar
        scalar_ad = z_ad(i,j) + scalar_ad ! z(i,j) = x(i,j) + scalar
        ary_ad(i,j) = scalar_ad * z(i,j) ! scalar = ary(i,j) * z(i,j)
        z_ad(i,j) = scalar_ad * ary(i,j) ! scalar = ary(i,j) * z(i,j)
        ary(i,j) = ary_save_77_ad
        x_ad(i,j) = ary_ad(i,j) * ary(i,j) + x_ad(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        z_ad(i,j) = ary_ad(i,j) + z_ad(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        ary_ad(i,j) = ary_ad(i,j) * x(i,j) ! ary(i,j) = x(i,j) * ary(i,j) + z(i,j)
        ary_ad(i,j) = z_ad(i,j) * y(i,j) + ary_ad(i,j) ! z(i,j) = ary(i,j) * y(i,j)
        y_ad(i,j) = z_ad(i,j) * ary(i,j) + y_ad(i,j) ! z(i,j) = ary(i,j) * y(i,j)
        z_ad(i,j) = 0.0 ! z(i,j) = ary(i,j) * y(i,j)
      end do
    end do
    x_ad(:,:) = ary_ad(:,:) + x_ad(:,:) ! ary(:,:) = x(:,:)

    return
  end subroutine do_with_array_private_rev_ad

  subroutine do_with_array_fwd_ad(n, m, x, x_ad, y, y_ad, z, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(in)  :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(in)  :: y_ad(n,m)
    real, intent(out) :: z(n,m)
    real, intent(out) :: z_ad(n,m)
    real :: ary_ad(n,m)
    integer :: i
    integer :: j
    real :: ary(n,m)

    do j = 1, m
      do i = 1, n
        ary_ad(i,j) = x_ad(i,j) ! ary(i,j) = x(i,j) + 1.0
        ary(i,j) = x(i,j) + 1.0
      end do
    end do
    do j = 1, m
      do i = 1, n
        z_ad(i,j) = ary_ad(i,j) * x(i,j) + x_ad(i,j) * ary(i,j) ! z(i,j) = ary(i,j) * x(i,j)
        z(i,j) = ary(i,j) * x(i,j)
        ary_ad(i,j) = ary_ad(i,j) + z_ad(i,j) * y(i,j) + y_ad(i,j) * z(i,j) ! ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
        ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
      end do
    end do
    do j = 1, m
      do i = 1, n
        z_ad(i,j) = z_ad(i,j) * x(i,j) + x_ad(i,j) * z(i,j) + ary_ad(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
        z(i,j) = z(i,j) * x(i,j) + ary(i,j)
        ary_ad(i,j) = y_ad(i,j) * ary(i,j) + ary_ad(i,j) * y(i,j) ! ary(i,j) = y(i,j) * ary(i,j)
        ary(i,j) = y(i,j) * ary(i,j)
        z_ad(i,j) = z_ad(i,j) + ary_ad(i,j) ! z(i,j) = z(i,j) + ary(i,j)
        z(i,j) = z(i,j) + ary(i,j)
      end do
    end do

    return
  end subroutine do_with_array_fwd_ad

  subroutine do_with_array_rev_ad(n, m, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(out) :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    real, intent(inout) :: z_ad(n,m)
    real :: ary_ad(n,m)
    integer :: i
    integer :: j
    real :: ary(n,m)
    real :: z(n,m)
    real :: ary_save_111_ad(n,m)

    do j = 1, m
      do i = 1, n
        ary(i,j) = x(i,j) + 1.0
      end do
    end do
    do j = 1, m
      ary_save_111_ad(1:n,j) = ary(1:n,j)
      do i = 1, n
        z(i,j) = ary(i,j) * x(i,j)
        ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
      end do
    end do

    do j = m, 1, - 1
      do i = n, 1, - 1
        ary_ad(i,j) = z_ad(i,j) ! z(i,j) = z(i,j) + ary(i,j)
        y_ad(i,j) = ary_ad(i,j) * ary(i,j) ! ary(i,j) = y(i,j) * ary(i,j)
        ary_ad(i,j) = ary_ad(i,j) * y(i,j) ! ary(i,j) = y(i,j) * ary(i,j)
        x_ad(i,j) = z_ad(i,j) * z(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
        ary_ad(i,j) = z_ad(i,j) + ary_ad(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
        z_ad(i,j) = z_ad(i,j) * x(i,j) ! z(i,j) = z(i,j) * x(i,j) + ary(i,j)
      end do
    end do
    do j = m, 1, - 1
      ary(1:n,j) = ary_save_111_ad(1:n,j)
      do i = n, 1, - 1
        z(i,j) = ary(i,j) * x(i,j)
        z_ad(i,j) = ary_ad(i,j) * y(i,j) + z_ad(i,j) ! ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
        y_ad(i,j) = ary_ad(i,j) * z(i,j) + y_ad(i,j) ! ary(i,j) = ary(i,j) + z(i,j) * y(i,j)
        ary_ad(i,j) = z_ad(i,j) * x(i,j) + ary_ad(i,j) ! z(i,j) = ary(i,j) * x(i,j)
        x_ad(i,j) = z_ad(i,j) * ary(i,j) + x_ad(i,j) ! z(i,j) = ary(i,j) * x(i,j)
        z_ad(i,j) = 0.0 ! z(i,j) = ary(i,j) * x(i,j)
      end do
    end do
    do j = m, 1, - 1
      do i = n, 1, - 1
        x_ad(i,j) = ary_ad(i,j) + x_ad(i,j) ! ary(i,j) = x(i,j) + 1.0
      end do
    end do

    return
  end subroutine do_with_array_rev_ad

  subroutine do_with_local_array_fwd_ad(n, m, x, x_ad, y, y_ad, z, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(in)  :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(in)  :: y_ad(n,m)
    real, intent(out) :: z(n,m)
    real, intent(out) :: z_ad(n,m)
    real :: work1_ad(2,n,m)
    real :: work2_ad(2,m)
    real :: work3_ad(2)
    integer :: i
    integer :: j
    real :: work3(2)
    real :: work2(2,m)
    real :: work1(2,n,m)
    integer :: k

    do j = 1, m
      do i = 1, n
        work3_ad(1) = x_ad(i,j) + y_ad(i,j) ! work3(1) = x(i,j) + y(i,j)
        work3(1) = x(i,j) + y(i,j)
        work3_ad(2) = x_ad(i,j) - y_ad(i,j) ! work3(2) = x(i,j) - y(i,j)
        work3(2) = x(i,j) - y(i,j)
        work2_ad(1,i) = work3_ad(1) + work3_ad(2) * x(i,j) + x_ad(i,j) * work3(2) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        work2(1,i) = work3(1) + work3(2) * x(i,j)
        work2_ad(2,i) = work3_ad(1) * work3(2) + work3_ad(2) * work3(1) + y_ad(i,j) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        work2(2,i) = work3(1) * work3(2) + y(i,j)
      end do
      do i = 1, n
        work1_ad(1,i,j) = work2_ad(1,i) * x(i,j) + x_ad(i,j) * work2(1,i) ! work1(1,i,j) = work2(1,i) * x(i,j)
        work1(1,i,j) = work2(1,i) * x(i,j)
        work1_ad(2,i,j) = work2_ad(2,i) * y(i,j) + y_ad(i,j) * work2(2,i) ! work1(2,i,j) = work2(2,i) * y(i,j)
        work1(2,i,j) = work2(2,i) * y(i,j)
      end do
    end do
    do j = 1, m
      do i = 1, n
        z_ad(i,j) = work1_ad(1,i,j) * y(i,j) + y_ad(i,j) * work1(1,i,j) + work1_ad(2,i,j) * x(i,j) + x_ad(i,j) * work1(2,i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        do k = 1, 2
          work3_ad(k) = work1_ad(k,i,j) ! work3(k) = work1(k,i,j)
          work3(k) = work1(k,i,j)
          work1_ad(k,i,j) = x_ad(i,j) * work3(k) + work3_ad(k) * x(i,j) ! work1(k,i,j) = x(i,j) * work3(k)
          work1(k,i,j) = x(i,j) * work3(k)
        end do
        z_ad(i,j) = z_ad(i,j) * (work3(1) + work3(2)) + work3_ad(1) * z(i,j) + work3_ad(2) * z(i,j) + work1_ad(1,i,j) * y(i,j) + y_ad(i,j) * work1(1,i,j) + work1_ad(2,i,j) * x(i,j) + x_ad(i,j) * work1(2,i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
      end do
    end do

    return
  end subroutine do_with_local_array_fwd_ad

  subroutine do_with_local_array_rev_ad(n, m, x, x_ad, y, y_ad, z_ad)
    integer, intent(in)  :: n
    integer, intent(in)  :: m
    real, intent(in)  :: x(n,m)
    real, intent(out) :: x_ad(n,m)
    real, intent(in)  :: y(n,m)
    real, intent(out) :: y_ad(n,m)
    real, intent(inout) :: z_ad(n,m)
    real :: work1_ad(2,n,m)
    real :: work2_ad(2,m)
    real :: work3_ad(2)
    integer :: i
    integer :: j
    real :: work3(2)
    real :: work2(2,m)
    real :: work1(2,n,m)
    real :: z(n,m)
    integer :: k
    real :: work1_save_166_ad(2)

    do j = 1, m
      do i = 1, n
        work3(1) = x(i,j) + y(i,j)
        work3(2) = x(i,j) - y(i,j)
        work2(1,i) = work3(1) + work3(2) * x(i,j)
        work2(2,i) = work3(1) * work3(2) + y(i,j)
      end do
      do i = 1, n
        work1(1,i,j) = work2(1,i) * x(i,j)
        work1(2,i,j) = work2(2,i) * y(i,j)
      end do
    end do

    work2_ad(:,:) = 0.0
    work3_ad(:) = 0.0

    do j = m, 1, - 1
      do i = n, 1, - 1
        z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        do k = 1, 2
          work1_save_166_ad(k) = work1(k,i,j)
          work3(k) = work1(k,i,j)
          work1(k,i,j) = x(i,j) * work3(k)
        end do
        work3_ad(1) = z_ad(i,j) * z(i,j) + work3_ad(1) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work3_ad(2) = z_ad(i,j) * z(i,j) + work3_ad(2) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work1_ad(1,i,j) = z_ad(i,j) * y(i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        y_ad(i,j) = z_ad(i,j) * work1(1,i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work1_ad(2,i,j) = z_ad(i,j) * x(i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        x_ad(i,j) = z_ad(i,j) * work1(2,i,j) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        z_ad(i,j) = z_ad(i,j) * (work3(1) + work3(2)) ! z(i,j) = z(i,j) * (work3(1) + work3(2)) + work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        do k = 2, 1, - 1
          work1(k,i,j) = work1_save_166_ad(k)
          work3(k) = work1(k,i,j)
          x_ad(i,j) = work1_ad(k,i,j) * work3(k) + x_ad(i,j) ! work1(k,i,j) = x(i,j) * work3(k)
          work3_ad(k) = work1_ad(k,i,j) * x(i,j) + work3_ad(k) ! work1(k,i,j) = x(i,j) * work3(k)
          work1_ad(k,i,j) = work3_ad(k) ! work3(k) = work1(k,i,j)
          work3_ad(k) = 0.0 ! work3(k) = work1(k,i,j)
          work1(k,i,j) = work1_save_166_ad(k)
        end do
        work1_ad(1,i,j) = z_ad(i,j) * y(i,j) + work1_ad(1,i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        y_ad(i,j) = z_ad(i,j) * work1(1,i,j) + y_ad(i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        work1_ad(2,i,j) = z_ad(i,j) * x(i,j) + work1_ad(2,i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        x_ad(i,j) = z_ad(i,j) * work1(2,i,j) + x_ad(i,j) ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
        z_ad(i,j) = 0.0 ! z(i,j) = work1(1,i,j) * y(i,j) + work1(2,i,j) * x(i,j)
      end do
    end do
    do j = m, 1, - 1
      do i = 1, n
        work3(1) = x(i,j) + y(i,j)
        work3(2) = x(i,j) - y(i,j)
        work2(1,i) = work3(1) + work3(2) * x(i,j)
        work2(2,i) = work3(1) * work3(2) + y(i,j)
      end do
      do i = n, 1, - 1
        work2_ad(2,i) = work1_ad(2,i,j) * y(i,j) + work2_ad(2,i) ! work1(2,i,j) = work2(2,i) * y(i,j)
        y_ad(i,j) = work1_ad(2,i,j) * work2(2,i) + y_ad(i,j) ! work1(2,i,j) = work2(2,i) * y(i,j)
        work2_ad(1,i) = work1_ad(1,i,j) * x(i,j) + work2_ad(1,i) ! work1(1,i,j) = work2(1,i) * x(i,j)
        x_ad(i,j) = work1_ad(1,i,j) * work2(1,i) + x_ad(i,j) ! work1(1,i,j) = work2(1,i) * x(i,j)
      end do
      do i = n, 1, - 1
        work3(1) = x(i,j) + y(i,j)
        work3(2) = x(i,j) - y(i,j)
        work3_ad(1) = work2_ad(2,i) * work3(2) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        work3_ad(2) = work2_ad(2,i) * work3(1) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        y_ad(i,j) = work2_ad(2,i) + y_ad(i,j) ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        work2_ad(2,i) = 0.0 ! work2(2,i) = work3(1) * work3(2) + y(i,j)
        work3_ad(1) = work2_ad(1,i) + work3_ad(1) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        work3_ad(2) = work2_ad(1,i) * x(i,j) + work3_ad(2) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        x_ad(i,j) = work2_ad(1,i) * work3(2) + x_ad(i,j) ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        work2_ad(1,i) = 0.0 ! work2(1,i) = work3(1) + work3(2) * x(i,j)
        x_ad(i,j) = work3_ad(2) + x_ad(i,j) ! work3(2) = x(i,j) - y(i,j)
        y_ad(i,j) = - work3_ad(2) + y_ad(i,j) ! work3(2) = x(i,j) - y(i,j)
        x_ad(i,j) = work3_ad(1) + x_ad(i,j) ! work3(1) = x(i,j) + y(i,j)
        y_ad(i,j) = work3_ad(1) + y_ad(i,j) ! work3(1) = x(i,j) + y(i,j)
      end do
    end do

    return
  end subroutine do_with_local_array_rev_ad

  subroutine do_with_stencil_array_fwd_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x(n)
    real, intent(inout) :: x_ad(n)
    integer :: i

    x_ad(1) = x_ad(1) * x(2) * 0.5 + x_ad(2) * x(1) * 0.5 ! x(1) = x(1) * x(2) * 0.5
    x(1) = x(1) * x(2) * 0.5
    do i = 2, n - 1
      x_ad(i) = x_ad(i) * (x(i + 1) - x(i - 1)) * 0.5 + x_ad(i + 1) * x(i) * 0.5 - x_ad(i - 1) * x(i) * 0.5 ! x(i) = x(i) * (x(i+1) - x(i-1)) * 0.5
      x(i) = x(i) * (x(i + 1) - x(i - 1)) * 0.5
    end do
    x_ad(n) = - x_ad(n) * x(n - 1) * 0.5 - x_ad(n - 1) * x(n) * 0.5 ! x(n) = - x(n) * x(n-1) * 0.5

    return
  end subroutine do_with_stencil_array_fwd_ad

  subroutine do_with_stencil_array_rev_ad(n, x, x_ad)
    integer, intent(in)  :: n
    real, intent(inout) :: x(n)
    real, intent(inout) :: x_ad(n)
    integer :: i
    real :: x_save_184_ad
    real :: x_save_185_ad(n)

    x_save_184_ad = x(1)
    x(1) = x(1) * x(2) * 0.5
    do i = 2, n - 1
      x_save_185_ad(i) = x(i)
      x(i) = x(i) * (x(i + 1) - x(i - 1)) * 0.5
    end do

    x_ad(n - 1) = - x_ad(n) * x(n) * 0.5 + x_ad(n - 1) ! x(n) = - x(n) * x(n-1) * 0.5
    x_ad(n) = - x_ad(n) * x(n - 1) * 0.5 ! x(n) = - x(n) * x(n-1) * 0.5
    do i = n - 1, 2, - 1
      x(i) = x_save_185_ad(i)
      x_ad(i + 1) = x_ad(i) * x(i) * 0.5 + x_ad(i + 1) ! x(i) = x(i) * (x(i+1) - x(i-1)) * 0.5
      x_ad(i - 1) = - x_ad(i) * x(i) * 0.5 + x_ad(i - 1) ! x(i) = x(i) * (x(i+1) - x(i-1)) * 0.5
      x_ad(i) = x_ad(i) * (x(i + 1) - x(i - 1)) * 0.5 ! x(i) = x(i) * (x(i+1) - x(i-1)) * 0.5
    end do
    x(1) = x_save_184_ad
    x_ad(2) = x_ad(1) * x(1) * 0.5 + x_ad(2) ! x(1) = x(1) * x(2) * 0.5
    x_ad(1) = x_ad(1) * x(2) * 0.5 ! x(1) = x(1) * x(2) * 0.5

    return
  end subroutine do_with_stencil_array_rev_ad

end module save_vars_ad
