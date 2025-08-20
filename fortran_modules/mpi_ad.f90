module mpi_ad
  use mpi
  use iso_c_binding
  implicit none
  private
  public :: mpi_bcast_fwd_ad
  public :: mpi_bcast_rev_ad
  public :: mpi_reduce_fwd_ad
  public :: mpi_reduce_rev_ad
  public :: mpi_allreduce_fwd_ad
  public :: mpi_allreduce_rev_ad
  public :: mpi_scatter_fwd_ad
  public :: mpi_scatter_rev_ad
  public :: mpi_gather_fwd_ad
  public :: mpi_gather_rev_ad
  public :: mpi_alltoall_fwd_ad
  public :: mpi_alltoall_rev_ad
  public :: mpi_recv_fwd_ad
  public :: mpi_recv_rev_ad
  public :: mpi_send_fwd_ad
  public :: mpi_send_rev_ad
  public :: mpi_sendrecv_fwd_ad
  public :: mpi_sendrecv_rev_ad
  public :: mpi_isend_fwd_ad
  public :: mpi_isend_fwd_rev_ad
  public :: mpi_isend_rev_ad
  public :: mpi_irecv_fwd_ad
  public :: mpi_irecv_fwd_rev_ad
  public :: mpi_irecv_rev_ad
  public :: mpi_put_fwd_ad
  public :: mpi_put_rev_ad
  public :: mpi_get_fwd_ad
  public :: mpi_get_rev_ad
  public :: mpi_accumulate_fwd_ad
  public :: mpi_accumulate_rev_ad
  public :: mpi_send_init_fwd_ad
  public :: mpi_send_init_fwd_rev_ad
  public :: mpi_send_init_rev_ad
  public :: mpi_recv_init_fwd_ad
  public :: mpi_recv_init_fwd_rev_ad
  public :: mpi_recv_init_rev_ad
  public :: mpi_start_fwd_ad
  public :: mpi_start_rev_ad
  public :: mpi_startall_fwd_ad
  public :: mpi_startall_rev_ad
  public :: mpi_wait_fwd_ad
  public :: mpi_wait_rev_ad
  public :: mpi_waitall_fwd_ad
  public :: mpi_waitall_rev_ad
  public :: mpi_barrier_fwd_ad
  public :: mpi_barrier_rev_ad
  public :: mpi_win_fence_fwd_ad
  public :: mpi_win_fence_rev_ad
  public :: mpi_comm_group_fwd_ad
  public :: mpi_comm_group_rev_ad
  public :: mpi_group_size_fwd_ad
  public :: mpi_group_size_rev_ad
  public :: mpi_group_rank_fwd_ad
  public :: mpi_group_rank_rev_ad
  public :: mpi_group_incl_fwd_ad
  public :: mpi_group_incl_rev_ad
  public :: mpi_group_excl_fwd_ad
  public :: mpi_group_excl_rev_ad
  public :: mpi_group_union_fwd_ad
  public :: mpi_group_union_rev_ad
  public :: mpi_group_intersection_fwd_ad
  public :: mpi_group_intersection_rev_ad
  public :: mpi_group_difference_fwd_ad
  public :: mpi_group_difference_rev_ad
  public :: mpi_group_translate_ranks_fwd_ad
  public :: mpi_group_translate_ranks_rev_ad
  public :: mpi_group_free_fwd_ad
  public :: mpi_group_free_rev_ad
  public :: mpi_cart_create_fwd_ad
  public :: mpi_cart_create_rev_ad
  public :: mpi_cart_sub_fwd_ad
  public :: mpi_cart_sub_rev_ad
  public :: mpi_cart_rank_fwd_ad
  public :: mpi_cart_rank_rev_ad
  public :: mpi_cart_coords_fwd_ad
  public :: mpi_cart_coords_rev_ad
  public :: mpi_cart_shift_fwd_ad
  public :: mpi_cart_shift_rev_ad
  public :: mpi_dims_create_fwd_ad
  public :: mpi_dims_create_rev_ad
  public :: mpi_error_class_fwd_ad
  public :: mpi_error_class_rev_ad
  public :: mpi_error_string_fwd_ad
  public :: mpi_error_string_rev_ad
  public :: mpi_errhandler_set_fwd_ad
  public :: mpi_errhandler_set_rev_ad
  public :: mpi_errhandler_free_fwd_ad
  public :: mpi_errhandler_free_rev_ad

  interface mpi_bcast_fwd_ad
     module procedure mpi_bcast_fwd_ad_r4
     module procedure mpi_bcast_fwd_ad_r8
  end interface
  interface mpi_bcast_rev_ad
     module procedure mpi_bcast_rev_ad_r4
     module procedure mpi_bcast_rev_ad_r8
  end interface
  interface mpi_reduce_fwd_ad
     module procedure mpi_reduce_fwd_ad_r4
     module procedure mpi_reduce_fwd_ad_r8
  end interface
  interface mpi_reduce_rev_ad
     module procedure mpi_reduce_rev_ad_r4
     module procedure mpi_reduce_rev_ad_r8
  end interface
  interface mpi_allreduce_fwd_ad
     module procedure mpi_allreduce_fwd_ad_r4
     module procedure mpi_allreduce_fwd_ad_r8
     module procedure mpi_allreduce_fwd_ad_r4_inplace
     module procedure mpi_allreduce_fwd_ad_r8_inplace
  end interface
  interface mpi_allreduce_rev_ad
     module procedure mpi_allreduce_rev_ad_r4
     module procedure mpi_allreduce_rev_ad_r8
     module procedure mpi_allreduce_rev_ad_r4_inplace
     module procedure mpi_allreduce_rev_ad_r8_inplace
  end interface
  interface mpi_scatter_fwd_ad
     module procedure mpi_scatter_fwd_ad_r4
     module procedure mpi_scatter_fwd_ad_r8
  end interface
  interface mpi_scatter_rev_ad
     module procedure mpi_scatter_rev_ad_r4
     module procedure mpi_scatter_rev_ad_r8
  end interface
  interface mpi_gather_fwd_ad
     module procedure mpi_gather_fwd_ad_r4
     module procedure mpi_gather_fwd_ad_r8
  end interface
  interface mpi_gather_rev_ad
     module procedure mpi_gather_rev_ad_r4
     module procedure mpi_gather_rev_ad_r8
  end interface
  interface mpi_alltoall_fwd_ad
     module procedure mpi_alltoall_fwd_ad_r4
     module procedure mpi_alltoall_fwd_ad_r8
  end interface
  interface mpi_alltoall_rev_ad
     module procedure mpi_alltoall_rev_ad_r4
     module procedure mpi_alltoall_rev_ad_r8
  end interface
  interface mpi_recv_fwd_ad
     module procedure mpi_recv_fwd_ad_r4
     module procedure mpi_recv_fwd_ad_r8
  end interface
  interface mpi_recv_rev_ad
     module procedure mpi_recv_rev_ad_r4
     module procedure mpi_recv_rev_ad_r8
  end interface
  interface mpi_send_fwd_ad
     module procedure mpi_send_fwd_ad_r4
     module procedure mpi_send_fwd_ad_r8
  end interface
  interface mpi_send_rev_ad
     module procedure mpi_send_rev_ad_r4
     module procedure mpi_send_rev_ad_r8
  end interface
  interface mpi_sendrecv_fwd_ad
     module procedure mpi_sendrecv_fwd_ad_r4
     module procedure mpi_sendrecv_fwd_ad_r8
  end interface
  interface mpi_sendrecv_rev_ad
     module procedure mpi_sendrecv_rev_ad_r4
     module procedure mpi_sendrecv_rev_ad_r8
  end interface
  interface mpi_isend_fwd_ad
     module procedure mpi_isend_fwd_ad_r4
     module procedure mpi_isend_fwd_ad_r8
  end interface
  interface mpi_isend_fwd_rev_ad
     module procedure mpi_isend_fwd_rev_ad_r4
     module procedure mpi_isend_fwd_rev_ad_r8
  end interface
  interface mpi_isend_rev_ad
     module procedure mpi_isend_rev_ad_r4
     module procedure mpi_isend_rev_ad_r8
  end interface
  interface mpi_irecv_fwd_ad
     module procedure mpi_irecv_fwd_ad_r4
     module procedure mpi_irecv_fwd_ad_r8
  end interface
  interface mpi_irecv_fwd_rev_ad
     module procedure mpi_irecv_fwd_rev_ad_r4
     module procedure mpi_irecv_fwd_rev_ad_r8
  end interface
  interface mpi_irecv_rev_ad
     module procedure mpi_irecv_rev_ad_r4
     module procedure mpi_irecv_rev_ad_r8
  end interface
  interface mpi_put_fwd_ad
     module procedure mpi_put_fwd_ad_r4
     module procedure mpi_put_fwd_ad_r8
  end interface
  interface mpi_put_rev_ad
     module procedure mpi_put_rev_ad_r4
     module procedure mpi_put_rev_ad_r8
  end interface
  interface mpi_get_fwd_ad
     module procedure mpi_get_fwd_ad_r4
     module procedure mpi_get_fwd_ad_r8
  end interface
  interface mpi_get_rev_ad
     module procedure mpi_get_rev_ad_r4
     module procedure mpi_get_rev_ad_r8
  end interface
  interface mpi_accumulate_fwd_ad
     module procedure mpi_accumulate_fwd_ad_r4
     module procedure mpi_accumulate_fwd_ad_r8
  end interface
  interface mpi_accumulate_rev_ad
     module procedure mpi_accumulate_rev_ad_r4
     module procedure mpi_accumulate_rev_ad_r8
  end interface
  interface mpi_send_init_fwd_ad
     module procedure mpi_send_init_fwd_ad_r4
     module procedure mpi_send_init_fwd_ad_r8
  end interface
  interface mpi_send_init_fwd_rev_ad
     module procedure mpi_send_init_fwd_rev_ad_r4
     module procedure mpi_send_init_fwd_rev_ad_r8
  end interface
  interface mpi_send_init_rev_ad
     module procedure mpi_send_init_rev_ad_r4
     module procedure mpi_send_init_rev_ad_r8
  end interface
  interface mpi_recv_init_fwd_ad
     module procedure mpi_recv_init_fwd_ad_r4
     module procedure mpi_recv_init_fwd_ad_r8
  end interface
  interface mpi_recv_init_fwd_rev_ad
     module procedure mpi_recv_init_fwd_rev_ad_r4
     module procedure mpi_recv_init_fwd_rev_ad_r8
  end interface
  interface mpi_recv_init_rev_ad
     module procedure mpi_recv_init_rev_ad_r4
     module procedure mpi_recv_init_rev_ad_r8
  end interface

  integer, parameter :: FAD_MPI_OP_SEND = 1
  integer, parameter :: FAD_MPI_OP_RECV = 2
  integer, parameter :: MAX_REQUESTS = 1024

  type :: req_map_t
     type(c_ptr) :: ptr_advar = c_null_ptr
     integer :: count = 0
     integer :: datatype = 0
     integer :: source = 0
     integer :: dest = 0
     integer :: tag = 0
     integer :: comm = 0
     integer :: op_type = 0
     integer :: request = MPI_REQUEST_NULL
  end type req_map_t

  type, extends(req_map_t) :: req_map_r4_t
     real, allocatable :: recvbuf(:)
  end type req_map_r4_t

  type, extends(req_map_t) :: req_map_r8_t
     real(8), allocatable :: recvbuf(:)
  end type req_map_r8_t

  type(req_map_r4_t) :: req_map_r4(MAX_REQUESTS)
  type(req_map_r8_t) :: req_map_r8(MAX_REQUESTS)

contains

  subroutine mpi_bcast_fwd_ad_r4(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout) :: buffer(..)
    real, intent(inout) :: buffer_ad(..)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r4

  subroutine mpi_bcast_rev_ad_r4(buffer_ad, count, datatype, root, comm, ierr)
    real, intent(inout), target, contiguous :: buffer_ad(..)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    real, pointer :: b_ad(:)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call c_f_pointer(c_loc(buffer_ad), b_ad, [count])
    call MPI_Reduce(b_ad, tmp, count, datatype, MPI_SUM, root, comm, ierr)
    if (rank == root) then
      b_ad(1:count) = tmp(1:count)
    else
      b_ad(1:count) = 0.0
    end if
  end subroutine mpi_bcast_rev_ad_r4

  subroutine mpi_bcast_fwd_ad_r8(buffer, buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout) :: buffer(..)
    real(8), intent(inout) :: buffer_ad(..)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Bcast(buffer, count, datatype, root, comm, ierr)
    call MPI_Bcast(buffer_ad, count, datatype, root, comm, ierr)
  end subroutine mpi_bcast_fwd_ad_r8

  subroutine mpi_bcast_rev_ad_r8(buffer_ad, count, datatype, root, comm, ierr)
    real(8), intent(inout), target, contiguous :: buffer_ad(..)
    integer, intent(in) :: count, datatype, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    real(8), pointer :: b_ad(:)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call c_f_pointer(c_loc(buffer_ad), b_ad, [count])
    call MPI_Reduce(b_ad, tmp, count, datatype, MPI_SUM, root, comm, ierr)
    if (rank == root) then
      b_ad(1:count) = tmp(1:count)
    else
      b_ad(1:count) = 0.0_8
    end if
  end subroutine mpi_bcast_rev_ad_r8
  subroutine mpi_reduce_fwd_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(in) :: sendbuf(..)
    real, intent(in) :: sendbuf_ad(..)
    real, intent(out) :: recvbuf(..)
    real, intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: ierr2

    call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
    select case (op)
    case (MPI_SUM)
      call MPI_Reduce(sendbuf_ad, recvbuf_ad, count, datatype, MPI_SUM, root, comm, ierr)
    case (MPI_MAX, MPI_MIN)
      call MPI_Bcast(recvbuf, count, datatype, root, comm, ierr2)
      tmp = merge(sendbuf_ad, 0.0, sendbuf == recvbuf)
      call MPI_Reduce(tmp, recvbuf_ad, count, datatype, MPI_SUM, root, comm, ierr)
    case default
      print *, "Error: unsupported MPI_Op in mpi_reduce_fwd_ad"
      call MPI_abort(comm, -1, ierr2)
    end select
  end subroutine mpi_reduce_fwd_ad_r4

  subroutine mpi_reduce_rev_ad_r4(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real, intent(inout), target, contiguous :: sendbuf_ad(..)
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    real, pointer :: sb_ad(:), rb_ad(:)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [count])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [count])
    if (rank == root) tmp(:) = rb_ad(1:count)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sb_ad(1:count) = sb_ad(1:count) + tmp(1:count)
    if (rank == root) rb_ad(1:count) = 0.0
  end subroutine mpi_reduce_rev_ad_r4

  subroutine mpi_reduce_fwd_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(in) :: sendbuf(..)
    real(8), intent(in) :: sendbuf_ad(..)
    real(8), intent(out) :: recvbuf(..)
    real(8), intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: ierr2

    call MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
    select case (op)
    case (MPI_SUM)
      call MPI_Reduce(sendbuf_ad, recvbuf_ad, count, datatype, MPI_SUM, root, comm, ierr)
    case (MPI_MAX, MPI_MIN)
      call MPI_Bcast(recvbuf, count, datatype, root, comm, ierr2)
      tmp = merge(sendbuf_ad, 0.0_8, sendbuf == recvbuf)
      call MPI_Reduce(tmp, recvbuf_ad, count, datatype, MPI_SUM, root, comm, ierr)
    case default
      print *, "Error: unsupported MPI_Op in mpi_reduce_fwd_ad"
      call MPI_abort(comm, -1, ierr2)
    end select
  end subroutine mpi_reduce_fwd_ad_r8

  subroutine mpi_reduce_rev_ad_r8(sendbuf_ad, recvbuf_ad, count, datatype, op, root, comm, ierr)
    real(8), intent(inout), target, contiguous :: sendbuf_ad(..)
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, root, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    real(8), pointer :: sb_ad(:), rb_ad(:)
    integer :: rank, ierr2

    call MPI_Comm_rank(comm, rank, ierr2)
    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [count])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [count])
    if (rank == root) tmp(:) = rb_ad(1:count)
    call MPI_Bcast(tmp, count, datatype, root, comm, ierr)
    sb_ad(1:count) = sb_ad(1:count) + tmp(1:count)
    if (rank == root) rb_ad(1:count) = 0.0_8
  end subroutine mpi_reduce_rev_ad_r8

  subroutine mpi_allreduce_fwd_ad_r4(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in) :: sendbuf(..)
    real, intent(in) :: sendbuf_ad(..)
    real, intent(out) :: recvbuf(..)
    real, intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    integer :: ierr2

    call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
    select case (op)
    case (MPI_SUM)
      call MPI_Allreduce(sendbuf_ad, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case (MPI_MAX, MPI_MIN)
      tmp = merge(sendbuf_ad, 0.0, sendbuf == recvbuf)
      call MPI_Allreduce(tmp, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case default
      print *, "Error: unsupported MPI_Op in mpi_allreduce_fwd_ad"
      call MPI_abort(comm, -1, ierr2)
    end select
  end subroutine mpi_allreduce_fwd_ad_r4

  subroutine mpi_allreduce_rev_ad_r4(sendbuf, sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
    real, intent(in), target, contiguous :: sendbuf(..)
    real, intent(inout), target, contiguous :: sendbuf_ad(..)
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    real, pointer :: sb(:), sb_ad(:), rb_ad(:)
    real :: rb(count)
    integer :: n

    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [count])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [count])
    select case(op)
    case (MPI_SUM)
      call MPI_Allreduce(rb_ad, rb, count, datatype, MPI_SUM, comm, ierr)
      sb_ad(1:count) = rb(1:count)
    case (MPI_MAX, MPI_MIN)
      call c_f_pointer(c_loc(sendbuf), sb, [count])
      call MPI_Allreduce(sendbuf, rb, count, datatype, op, comm, ierr)
      do n = 1, count
        if (sb(n) == rb(n)) then
          sb_ad(n) = rb_ad(n)
        else
          sb_ad(n) = 0.0
        end if
      end do
    case default
      print *, "Error: Unsupported operation for reverse mode Allreduce."
      call MPI_Abort(comm, -1, ierr)
    end select
    rb_ad(1:count) = 0.0
  end subroutine mpi_allreduce_rev_ad_r4

  subroutine mpi_allreduce_fwd_ad_r8(sendbuf, sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in) :: sendbuf(..)
    real(8), intent(in) :: sendbuf_ad(..)
    real(8), intent(out) :: recvbuf(..)
    real(8), intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    integer :: ierr2

    call MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
    select case (op)
    case (MPI_SUM)
      call MPI_Allreduce(sendbuf_ad, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case (MPI_MAX, MPI_MIN)
      tmp = merge(sendbuf_ad, 0.0_8, sendbuf == recvbuf)
      call MPI_Allreduce(tmp, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case default
      print *, "Error: unsupported MPI_Op in mpi_allreduce_fwd_ad"
      call MPI_abort(comm, -1, ierr2)
    end select
  end subroutine mpi_allreduce_fwd_ad_r8

  subroutine mpi_allreduce_rev_ad_r8(sendbuf, sendbuf_ad, recvbuf_ad, count, datatype, op, comm, ierr)
    real(8), intent(in), target, contiguous :: sendbuf(..)
    real(8), intent(inout), target, contiguous :: sendbuf_ad(..)
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    real(8), pointer :: sb(:), sb_ad(:), rb_ad(:)
    real(8) :: rb(count)
    integer :: n

    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [count])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [count])
    select case(op)
    case (MPI_SUM)
      call MPI_Allreduce(rb_ad, rb, count, datatype, MPI_SUM, comm, ierr)
      sb_ad(1:count) = rb(1:count)
    case (MPI_MAX, MPI_MIN)
      call c_f_pointer(c_loc(sendbuf), sb, [count])
      call MPI_Allreduce(sendbuf, rb, count, datatype, op, comm, ierr)
      do n = 1, count
        if (sb(n) == rb(n)) then
          sb_ad(n) = rb_ad(n)
        else
          sb_ad(n) = 0.0_8
        end if
      end do
    case default
      print *, "Error: Unsupported operation for reverse mode Allreduce."
      call MPI_Abort(comm, -1, ierr)
    end select
    rb_ad(1:count) = 0.0_8
  end subroutine mpi_allreduce_rev_ad_r8

  subroutine mpi_allreduce_fwd_ad_r4_inplace(sendbuf, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    integer, intent(in) :: sendbuf
    real, intent(inout) :: recvbuf(..)
    real, intent(inout) :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real :: rb(count), rb_ad(count), tmp(count)
    integer :: ierr2

    if (sendbuf /= MPI_IN_PLACE) then
      print *, "Error: sendbuf must be MPI_IN_PLACE for in-place Allreduce."
      call MPI_abort(comm, -1, ierr)
    end if

    rb(1:count) = recvbuf(1:count)
    rb_ad(1:count) = recvbuf_ad(1:count)
    call MPI_Allreduce(MPI_IN_PLACE, recvbuf, count, datatype, op, comm, ierr)
    select case (op)
    case (MPI_SUM)
      call MPI_Allreduce(MPI_IN_PLACE, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case (MPI_MAX, MPI_MIN)
      tmp = merge(rb_ad, 0.0, rb == recvbuf)
      call MPI_Allreduce(tmp, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case default
      print *, "Error: unsupported MPI_Op in mpi_allreduce_fwd_ad"
      call MPI_abort(comm, -1, ierr2)
    end select
  end subroutine mpi_allreduce_fwd_ad_r4_inplace

  subroutine mpi_allreduce_rev_ad_r4_inplace(sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    integer, intent(in) :: sendbuf_ad
    real, intent(in), target, contiguous :: recvbuf(..)
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    real, pointer :: sb(:), sb_ad(:), rb_ad(:)
    real :: rb(count)
    integer :: n

    if (sendbuf_ad /= MPI_IN_PLACE) then
      print *, "Error: sendbuf_ad must be MPI_IN_PLACE for in-place Allreduce."
      call MPI_abort(comm, -1, ierr)
    end if

    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [count])
    sb_ad => rb_ad
    select case(op)
    case (MPI_SUM)
      call MPI_Allreduce(rb_ad, rb, count, datatype, MPI_SUM, comm, ierr)
      sb_ad(1:count) = rb(1:count)
    case (MPI_MAX, MPI_MIN)
      call c_f_pointer(c_loc(recvbuf), sb, [count])
      call MPI_Allreduce(sb, rb, count, datatype, op, comm, ierr)
      do n = 1, count
        if (sb(n) == rb(n)) then
          sb_ad(n) = rb_ad(n)
        else
          sb_ad(n) = 0.0
        end if
      end do
    case default
      print *, "Error: Unsupported operation for reverse mode Allreduce."
      call MPI_Abort(comm, -1, ierr)
    end select
  end subroutine mpi_allreduce_rev_ad_r4_inplace

  subroutine mpi_allreduce_fwd_ad_r8_inplace(sendbuf, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    integer, intent(in) :: sendbuf
    real(8), intent(inout) :: recvbuf(..)
    real(8), intent(inout) :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr
    real(8) :: rb(count), rb_ad(count), tmp(count)
    integer :: ierr2

    if (sendbuf /= MPI_IN_PLACE) then
      print *, "Error: sendbuf_ad must be MPI_IN_PLACE for in-place Allreduce."
      call MPI_abort(comm, -1, ierr)
    end if

    rb(1:count) = recvbuf(1:count)
    rb_ad(1:count) = recvbuf_ad(1:count)
    call MPI_Allreduce(MPI_IN_PLACE, recvbuf, count, datatype, op, comm, ierr)
    select case (op)
    case (MPI_SUM)
      call MPI_Allreduce(MPI_IN_PLACE, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case (MPI_MAX, MPI_MIN)
      tmp = merge(rb_ad, 0.0_8, rb == recvbuf)
      call MPI_Allreduce(tmp, recvbuf_ad, count, datatype, MPI_SUM, comm, ierr)
    case default
      print *, "Error: unsupported MPI_Op in mpi_allreduce_fwd_ad"
      call MPI_abort(comm, -1, ierr2)
    end select
  end subroutine mpi_allreduce_fwd_ad_r8_inplace

  subroutine mpi_allreduce_rev_ad_r8_inplace(sendbuf_ad, recvbuf, recvbuf_ad, count, datatype, op, comm, ierr)
    integer, intent(in) :: sendbuf_ad
    real(8), intent(in), target, contiguous :: recvbuf(..)
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: count, datatype, op, comm
    integer, intent(out), optional :: ierr

    real(8), pointer :: sb(:), sb_ad(:), rb_ad(:)
    real(8) :: rb(count)
    integer :: n

    if (sendbuf_ad /= MPI_IN_PLACE) then
      print *, "Error: sendbuf_ad must be MPI_IN_PLACE for in-place Allreduce."
      call MPI_abort(comm, -1, ierr)
    end if

    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [count])
    sb_ad => rb_ad
    select case(op)
    case (MPI_SUM)
      call MPI_Allreduce(rb_ad, rb, count, datatype, MPI_SUM, comm, ierr)
      sb_ad(1:count) = rb(1:count)
    case (MPI_MAX, MPI_MIN)
      call c_f_pointer(c_loc(recvbuf), sb, [count])
      call MPI_Allreduce(sb, rb, count, datatype, op, comm, ierr)
      do n = 1, count
        if (sb(n) == rb(n)) then
          sb_ad(n) = rb_ad(n)
        else
          sb_ad(n) = 0.0
        end if
      end do
    case default
      print *, "Error: Unsupported operation for reverse mode Allreduce."
      call MPI_Abort(comm, -1, ierr)
    end select
  end subroutine mpi_allreduce_rev_ad_r8_inplace

  subroutine mpi_scatter_fwd_ad_r4(sendbuf, sendbuf_ad, sendcount, sendtype, recvbuf, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real, intent(in) :: sendbuf(..)
    real, intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real, intent(out) :: recvbuf(..)
    real, intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr)
    call MPI_Scatter(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
  end subroutine mpi_scatter_fwd_ad_r4

  subroutine mpi_scatter_rev_ad_r4(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real, intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr
    integer :: rank, size, ierr2
    real, pointer :: sb_ad(:), rb_ad(:)
    real, allocatable :: tmp(:)
    real :: dummy(1)

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Comm_size(comm, size, ierr2)
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount])
    if (rank == root) then
      allocate(tmp(sendcount * size))
      call MPI_Gather(rb_ad, recvcount, recvtype, tmp, sendcount, sendtype, root, comm, ierr)
      call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount * size])
      sb_ad(1:sendcount*size) = sb_ad(1:sendcount*size) + tmp(1:sendcount*size)
      deallocate(tmp)
    else
      call MPI_Gather(rb_ad, recvcount, recvtype, dummy, sendcount, sendtype, root, comm, ierr)
    end if
    rb_ad(1:recvcount) = 0.0
  end subroutine mpi_scatter_rev_ad_r4

  subroutine mpi_scatter_fwd_ad_r8(sendbuf, sendbuf_ad, sendcount, sendtype, recvbuf, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real(8), intent(in) :: sendbuf(..)
    real(8), intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real(8), intent(out) :: recvbuf(..)
    real(8), intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr)
    call MPI_Scatter(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
  end subroutine mpi_scatter_fwd_ad_r8

  subroutine mpi_scatter_rev_ad_r8(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real(8), intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr
    integer :: rank, size, ierr2
    real(8), pointer :: sb_ad(:), rb_ad(:)
    real(8), allocatable :: tmp(:)
    real(8) :: dummy(1)

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Comm_size(comm, size, ierr2)
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount])
    if (rank == root) then
      allocate(tmp(sendcount * size))
      call MPI_Gather(rb_ad, recvcount, recvtype, tmp, sendcount, sendtype, root, comm, ierr)
      call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount * size])
      sb_ad(1:sendcount*size) = sb_ad(1:sendcount*size) + tmp(1:sendcount*size)
      deallocate(tmp)
    else
      call MPI_Gather(rb_ad, recvcount, recvtype, dummy, sendcount, sendtype, root, comm, ierr)
    end if
    rb_ad(1:recvcount) = 0.0_8
  end subroutine mpi_scatter_rev_ad_r8

  subroutine mpi_gather_fwd_ad_r4(sendbuf, sendbuf_ad, sendcount, sendtype, recvbuf, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real, intent(in) :: sendbuf(..)
    real, intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real, intent(out) :: recvbuf(..)
    real, intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr)
    call MPI_Gather(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
  end subroutine mpi_gather_fwd_ad_r4

  subroutine mpi_gather_rev_ad_r4(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real, intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr
    integer :: rank, size, ierr2
    real :: tmp(max(sendcount, recvcount))
    real, pointer :: sb_ad(:), rb_ad(:)

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Comm_size(comm, size, ierr2)
    call MPI_Scatter(recvbuf_ad, recvcount, recvtype, tmp, sendcount, sendtype, root, comm, ierr)
    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount])
    sb_ad(1:sendcount) = sb_ad(1:sendcount) + tmp(1:sendcount)
    if (rank == root) then
      call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount * size])
      rb_ad(1:recvcount*size) = 0.0
    end if
  end subroutine mpi_gather_rev_ad_r4

  subroutine mpi_gather_fwd_ad_r8(sendbuf, sendbuf_ad, sendcount, sendtype, recvbuf, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real(8), intent(in) :: sendbuf(..)
    real(8), intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real(8), intent(out) :: recvbuf(..)
    real(8), intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr

    call MPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr)
    call MPI_Gather(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
  end subroutine mpi_gather_fwd_ad_r8

  subroutine mpi_gather_rev_ad_r8(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, root, comm, ierr)
    real(8), intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, root, comm
    integer, intent(out), optional :: ierr
    integer :: rank, size, ierr2
    real(8) :: tmp(max(sendcount, recvcount))
    real(8), pointer :: sb_ad(:), rb_ad(:)

    call MPI_Comm_rank(comm, rank, ierr2)
    call MPI_Comm_size(comm, size, ierr2)
    call MPI_Scatter(recvbuf_ad, recvcount, recvtype, tmp, sendcount, sendtype, root, comm, ierr)
    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount])
    sb_ad(1:sendcount) = sb_ad(1:sendcount) + tmp(1:sendcount)
    if (rank == root) then
      call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount * size])
      rb_ad(1:recvcount*size) = 0.0_8
    end if
  end subroutine mpi_gather_rev_ad_r8

  subroutine mpi_alltoall_fwd_ad_r4(sendbuf, sendbuf_ad, sendcount, sendtype, recvbuf, recvbuf_ad, recvcount, recvtype, comm, ierr)
    real, intent(in) :: sendbuf(..)
    real, intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real, intent(out) :: recvbuf(..)
    real, intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, comm
    integer, intent(out), optional :: ierr

    call MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr)
    call MPI_Alltoall(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, comm, ierr)
  end subroutine mpi_alltoall_fwd_ad_r4

  subroutine mpi_alltoall_rev_ad_r4(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, comm, ierr)
    real, intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, comm
    integer, intent(out), optional :: ierr
    integer :: size, ierr2
    real, pointer :: sb_ad(:), rb_ad(:)
    real, allocatable :: tmp(:)

    call MPI_Comm_size(comm, size, ierr2)
    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount * size])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount * size])
    allocate(tmp(sendcount * size))
    call MPI_Alltoall(rb_ad, recvcount, recvtype, tmp, sendcount, sendtype, comm, ierr)
    sb_ad(1:sendcount*size) = sb_ad(1:sendcount*size) + tmp(1:sendcount*size)
    rb_ad(1:recvcount*size) = 0.0
    deallocate(tmp)
  end subroutine mpi_alltoall_rev_ad_r4

  subroutine mpi_alltoall_fwd_ad_r8(sendbuf, sendbuf_ad, sendcount, sendtype, recvbuf, recvbuf_ad, recvcount, recvtype, comm, ierr)
    real(8), intent(in) :: sendbuf(..)
    real(8), intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real(8), intent(out) :: recvbuf(..)
    real(8), intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, comm
    integer, intent(out), optional :: ierr

    call MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr)
    call MPI_Alltoall(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, comm, ierr)
  end subroutine mpi_alltoall_fwd_ad_r8

  subroutine mpi_alltoall_rev_ad_r8(sendbuf_ad, sendcount, sendtype, recvbuf_ad, recvcount, recvtype, comm, ierr)
    real(8), intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, comm
    integer, intent(out), optional :: ierr
    integer :: size, ierr2
    real(8), pointer :: sb_ad(:), rb_ad(:)
    real(8), allocatable :: tmp(:)

    call MPI_Comm_size(comm, size, ierr2)
    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount * size])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount * size])
    allocate(tmp(sendcount * size))
    call MPI_Alltoall(rb_ad, recvcount, recvtype, tmp, sendcount, sendtype, comm, ierr)
    sb_ad(1:sendcount*size) = sb_ad(1:sendcount*size) + tmp(1:sendcount*size)
    rb_ad(1:recvcount*size) = 0.0_8
    deallocate(tmp)
  end subroutine mpi_alltoall_rev_ad_r8
  subroutine mpi_recv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real, intent(out) :: buf(..)
    real, intent(out) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r4

  subroutine mpi_recv_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out), optional :: ierr

    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [count])
    call MPI_Send(b_ad, count, datatype, source, tag, comm, ierr)
    b_ad(1:count) = 0.0
  end subroutine mpi_recv_rev_ad_r4

  subroutine mpi_recv_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, status, ierr)
    real(8), intent(out) :: buf(..)
    real(8), intent(out) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Recv(buf, count, datatype, source, tag, comm, status, ierr)
    call MPI_Recv(buf_ad, count, datatype, source, tag, comm, status, ierr)
  end subroutine mpi_recv_fwd_ad_r8

  subroutine mpi_recv_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out), optional :: ierr

    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [count])
    call MPI_Send(b_ad, count, datatype, source, tag, comm, ierr)
    b_ad(1:count) = 0.0_8
  end subroutine mpi_recv_rev_ad_r8
  subroutine mpi_send_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(in) :: buf(..)
    real, intent(in) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr

    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r4

  subroutine mpi_send_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real :: tmp(count)
    real, pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [count])
    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    b_ad(1:count) = b_ad(1:count) + tmp(1:count)
  end subroutine mpi_send_rev_ad_r4

  subroutine mpi_send_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(in) :: buf(..)
    real(8), intent(in) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr

    call MPI_Send(buf, count, datatype, dest, tag, comm, ierr)
    call MPI_Send(buf_ad, count, datatype, dest, tag, comm, ierr)
  end subroutine mpi_send_fwd_ad_r8

  subroutine mpi_send_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(count)
    real(8), pointer :: b_ad(:)

    call c_f_pointer(c_loc(buf_ad), b_ad, [count])
    call MPI_Recv(tmp, count, datatype, dest, tag, comm, MPI_STATUS_IGNORE, ierr)
    b_ad(1:count) = b_ad(1:count) + tmp(1:count)
  end subroutine mpi_send_rev_ad_r8
  subroutine mpi_sendrecv_fwd_ad_r4(sendbuf, sendbuf_ad, sendcount, sendtype, dest, sendtag, &
                                    recvbuf, recvbuf_ad, recvcount, recvtype, source, recvtag, &
                                    comm, status, ierr)
    real, intent(in) :: sendbuf(..)
    real, intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype, dest, sendtag
    real, intent(out) :: recvbuf(..)
    real, intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, source, recvtag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, &
                      recvtype, source, recvtag, comm, status, ierr)
    call MPI_Sendrecv(sendbuf_ad, sendcount, sendtype, dest, sendtag, recvbuf_ad, recvcount, &
                      recvtype, source, recvtag, comm, status, ierr)
  end subroutine mpi_sendrecv_fwd_ad_r4

  subroutine mpi_sendrecv_rev_ad_r4(sendbuf_ad, sendcount, sendtype, dest, sendtag, &
                                    recvbuf_ad, recvcount, recvtype, source, recvtag, comm, ierr)
    real, intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype, dest, sendtag
    real, intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, source, recvtag, comm
    integer, intent(out), optional :: ierr
    real :: tmp(sendcount)
    real, pointer :: sb_ad(:), rb_ad(:)

    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount])
    call MPI_Sendrecv(rb_ad, recvcount, recvtype, source, recvtag, tmp, sendcount, &
                      sendtype, dest, sendtag, comm, MPI_STATUS_IGNORE, ierr)
    sb_ad(1:sendcount) = sb_ad(1:sendcount) + tmp(1:sendcount)
    rb_ad(1:recvcount) = 0.0
  end subroutine mpi_sendrecv_rev_ad_r4

  subroutine mpi_sendrecv_fwd_ad_r8(sendbuf, sendbuf_ad, sendcount, sendtype, dest, sendtag, &
                                    recvbuf, recvbuf_ad, recvcount, recvtype, source, recvtag, &
                                    comm, status, ierr)
    real(8), intent(in) :: sendbuf(..)
    real(8), intent(in) :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype, dest, sendtag
    real(8), intent(out) :: recvbuf(..)
    real(8), intent(out) :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, source, recvtag, comm
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, &
                      recvtype, source, recvtag, comm, status, ierr)
    call MPI_Sendrecv(sendbuf_ad, sendcount, sendtype, dest, sendtag, recvbuf_ad, recvcount, &
                      recvtype, source, recvtag, comm, status, ierr)
  end subroutine mpi_sendrecv_fwd_ad_r8

  subroutine mpi_sendrecv_rev_ad_r8(sendbuf_ad, sendcount, sendtype, dest, sendtag, &
                                    recvbuf_ad, recvcount, recvtype, source, recvtag, comm, ierr)
    real(8), intent(inout), target, contiguous :: sendbuf_ad(..)
    integer, intent(in) :: sendcount, sendtype, dest, sendtag
    real(8), intent(inout), target, contiguous :: recvbuf_ad(..)
    integer, intent(in) :: recvcount, recvtype, source, recvtag, comm
    integer, intent(out), optional :: ierr
    real(8) :: tmp(sendcount)
    real(8), pointer :: sb_ad(:), rb_ad(:)

    call c_f_pointer(c_loc(sendbuf_ad), sb_ad, [sendcount])
    call c_f_pointer(c_loc(recvbuf_ad), rb_ad, [recvcount])
    call MPI_Sendrecv(rb_ad, recvcount, recvtype, source, recvtag, tmp, sendcount, &
                      sendtype, dest, sendtag, comm, MPI_STATUS_IGNORE, ierr)
    sb_ad(1:sendcount) = sb_ad(1:sendcount) + tmp(1:sendcount)
    rb_ad(1:recvcount) = 0.0_8
  end subroutine mpi_sendrecv_rev_ad_r8
  subroutine mpi_isend_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in) :: buf(..)
    real, intent(in) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Isend(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_r4

  subroutine mpi_isend_fwd_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (i == MPI_REQUEST_NULL) cycle
      if (req_map_r4(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_isend_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r4(i)%recvbuf(count))
    req_map_r4(i)%ptr_advar = c_loc(buf_ad)
    req_map_r4(i)%count = count
    req_map_r4(i)%datatype = datatype
    req_map_r4(i)%source = dest
    req_map_r4(i)%tag = tag
    req_map_r4(i)%comm = comm
    req_map_r4(i)%op_type = FAD_MPI_OP_RECV
    request_ad = i
  end subroutine mpi_isend_fwd_rev_ad_r4

  subroutine mpi_isend_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_isend_rev_ad_r4: ", request_ad
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Wait(req_map_r4(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r4(idx))
    if (allocated(req_map_r4(idx)%recvbuf)) deallocate(req_map_r4(idx)%recvbuf)
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_isend_rev_ad_r4

  subroutine mpi_isend_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real(8), intent(in) :: buf(..)
    real(8), intent(in) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Isend(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_isend_fwd_ad_r8

  subroutine mpi_isend_fwd_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (i + MAX_REQUESTS == MPI_REQUEST_NULL) cycle
      if (req_map_r8(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_isend_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r8(i)%recvbuf(count))
    req_map_r8(i)%ptr_advar = c_loc(buf_ad)
    req_map_r8(i)%count = count
    req_map_r8(i)%datatype = datatype
    req_map_r8(i)%source = dest
    req_map_r8(i)%tag = tag
    req_map_r8(i)%comm = comm
    req_map_r8(i)%op_type = FAD_MPI_OP_RECV
    request_ad = i + MAX_REQUESTS  ! Offset to differentiate from r4 requests
  end subroutine mpi_isend_fwd_rev_ad_r8

  subroutine mpi_isend_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad - MAX_REQUESTS  ! Adjust index for r8 requests
    call MPI_Wait(req_map_r8(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r8(idx))
    if (allocated(req_map_r8(idx)%recvbuf)) deallocate(req_map_r8(idx)%recvbuf)
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_isend_rev_ad_r8

  subroutine mpi_irecv_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out) :: buf(..)
    real, intent(out) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Irecv(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_r4

  subroutine mpi_irecv_fwd_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i
    real, pointer :: b_ad(:)

    do i = 1, MAX_REQUESTS
      if (i == MPI_REQUEST_NULL) cycle
      if (req_map_r4(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_irecv_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r4(i)%recvbuf(count))
    call c_f_pointer(c_loc(buf_ad), b_ad, [count])
    req_map_r4(i)%recvbuf = b_ad(1:count)
    req_map_r4(i)%ptr_advar = c_loc(buf_ad)
    req_map_r4(i)%count = count
    req_map_r4(i)%datatype = datatype
    req_map_r4(i)%dest = source
    req_map_r4(i)%tag = tag
    req_map_r4(i)%comm = comm
    req_map_r4(i)%op_type = FAD_MPI_OP_SEND
    request_ad = i
  end subroutine mpi_irecv_fwd_rev_ad_r4

  subroutine mpi_irecv_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_irecv_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Wait(req_map_r4(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r4(idx))
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_irecv_rev_ad_r4

  subroutine mpi_irecv_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real(8), intent(out) :: buf(..)
    real(8), intent(out) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Irecv(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_irecv_fwd_ad_r8

  subroutine mpi_irecv_fwd_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: i

    do i = 1, MAX_REQUESTS
      if (i + MAX_REQUESTS == MPI_REQUEST_NULL) cycle
      if (req_map_r8(i)%op_type == 0) exit
    end do
    if (i > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_irecv_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    req_map_r8(i)%ptr_advar = c_loc(buf_ad)
    req_map_r8(i)%count = count
    req_map_r8(i)%datatype = datatype
    req_map_r8(i)%dest = source
    req_map_r8(i)%tag = tag
    req_map_r8(i)%comm = comm
    req_map_r8(i)%op_type = FAD_MPI_OP_SEND
    request_ad = i + MAX_REQUESTS  ! Offset to differentiate from r4 requests
  end subroutine mpi_irecv_fwd_rev_ad_r8

  subroutine mpi_irecv_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad - MAX_REQUESTS  ! Adjust index for r8 requests
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_irecv_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Wait(req_map_r8(idx)%request, MPI_STATUS_IGNORE, ierr)
    call update_advar(req_map_r8(idx))
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_irecv_rev_ad_r8
  subroutine mpi_put_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(in) :: origin(..)
    real, intent(in) :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Put(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Put(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_r4

  subroutine mpi_put_rev_ad_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(inout), target, contiguous :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real :: tmp(origin_count)
    real, pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [origin_count])
    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    o_ad(1:origin_count) = o_ad(1:origin_count) + tmp(1:origin_count)
    tmp = 0.0
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_r4

  subroutine mpi_put_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(in) :: origin(..)
    real(8), intent(in) :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Put(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Put(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_fwd_ad_r8

  subroutine mpi_put_rev_ad_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(inout), target, contiguous :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8) :: tmp(origin_count)
    real(8), pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [origin_count])
    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    o_ad(1:origin_count) = o_ad(1:origin_count) + tmp(1:origin_count)
    tmp = 0.0_8
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_put_rev_ad_r8
  subroutine mpi_get_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(out) :: origin(..)
    real, intent(out) :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Get(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Get(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_r4

  subroutine mpi_get_rev_ad_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real, intent(inout), target, contiguous :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    real, pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [origin_count])
    call MPI_Accumulate(o_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, MPI_SUM, win, ierr)
    o_ad(1:origin_count) = 0.0
  end subroutine mpi_get_rev_ad_r4

  subroutine mpi_get_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(out) :: origin(..)
    real(8), intent(out) :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Get(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    call MPI_Get(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_get_fwd_ad_r8

  subroutine mpi_get_rev_ad_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    real(8), intent(inout), target, contiguous :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    real(8), pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [origin_count])
    call MPI_Accumulate(o_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, MPI_SUM, win, ierr)
    o_ad(1:origin_count) = 0.0_8
  end subroutine mpi_get_rev_ad_r8
  subroutine mpi_accumulate_fwd_ad_r4(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(in) :: origin(..)
    real, intent(in) :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Accumulate(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_r4

  subroutine mpi_accumulate_rev_ad_r4(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real, intent(inout), target, contiguous :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real :: tmp(origin_count)
    real, pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [origin_count])
    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    o_ad(1:origin_count) = o_ad(1:origin_count) + tmp(1:origin_count)
    tmp = 0.0
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_accumulate_rev_ad_r4

  subroutine mpi_accumulate_fwd_ad_r8(origin, origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(in) :: origin(..)
    real(8), intent(in) :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr

    call MPI_Accumulate(origin, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    call MPI_Accumulate(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
  end subroutine mpi_accumulate_fwd_ad_r8

  subroutine mpi_accumulate_rev_ad_r8(origin_ad, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr)
    real(8), intent(inout), target, contiguous :: origin_ad(..)
    integer, intent(in) :: origin_count, origin_datatype, target_rank, target_count, target_datatype, op, win
    integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
    integer, intent(out), optional :: ierr
    real(8) :: tmp(origin_count)
    real(8), pointer :: o_ad(:)

    call c_f_pointer(c_loc(origin_ad), o_ad, [origin_count])
    call MPI_Get(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
    o_ad(1:origin_count) = o_ad(1:origin_count) + tmp(1:origin_count)
    tmp = 0.0_8
    call MPI_Put(tmp, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr)
  end subroutine mpi_accumulate_rev_ad_r8
  subroutine mpi_send_init_fwd_ad_r4(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real, intent(in) :: buf(..)
    real, intent(in) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Send_init(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_r4

  subroutine mpi_send_init_fwd_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
      if (idx == MPI_REQUEST_NULL) cycle
       if (req_map_r4(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_send_init_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r4(idx)%recvbuf(count))
    req_map_r4(idx)%ptr_advar = c_loc(buf_ad)
    req_map_r4(idx)%count = count
    req_map_r4(idx)%datatype = datatype
    req_map_r4(idx)%op_type = FAD_MPI_OP_RECV
    call MPI_Recv_init(req_map_r4(idx)%recvbuf, count, datatype, dest, tag, comm, req_map_r4(idx)%request, ierr)
    request_ad = idx
  end subroutine mpi_send_init_fwd_rev_ad_r4

  subroutine mpi_send_init_rev_ad_r4(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real, intent(inout) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad
    call MPI_Request_free(req_map_r4(idx)%request, ierr)
    if (allocated(req_map_r4(idx)%recvbuf)) deallocate(req_map_r4(idx)%recvbuf)
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_send_init_rev_ad_r4

  subroutine mpi_send_init_fwd_ad_r8(buf, buf_ad, count, datatype, dest, tag, comm, request, request_ad, ierr)
    real(8), intent(in) :: buf(..)
    real(8), intent(in) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierr)
    call MPI_Send_init(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
  end subroutine mpi_send_init_fwd_ad_r8

  subroutine mpi_send_init_fwd_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
      if (idx + MAX_REQUESTS == MPI_REQUEST_NULL) cycle
      if (req_map_r8(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_send_init_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    allocate(req_map_r8(idx)%recvbuf(count))
    req_map_r8(idx)%ptr_advar = c_loc(buf_ad)
    req_map_r8(idx)%count = count
    req_map_r8(idx)%datatype = datatype
    req_map_r8(idx)%op_type = FAD_MPI_OP_RECV
    call MPI_Recv_init(req_map_r8(idx)%recvbuf, count, datatype, dest, tag, comm, req_map_r8(idx)%request, ierr)
    request_ad = idx + MAX_REQUESTS ! Offset to distinguish from real(4) requests
  end subroutine mpi_send_init_fwd_rev_ad_r8

  subroutine mpi_send_init_rev_ad_r8(buf_ad, count, datatype, dest, tag, comm, request_ad, ierr)
    real(8), intent(inout) :: buf_ad(..)
    integer, intent(in) :: count, datatype, dest, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad - MAX_REQUESTS ! Adjust index for real(8) requests
    call MPI_Request_free(req_map_r8(idx)%request, ierr)
    if (allocated(req_map_r8(idx)%recvbuf)) deallocate(req_map_r8(idx)%recvbuf)
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_send_init_rev_ad_r8

  subroutine mpi_recv_init_fwd_ad_r4(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real, intent(out) :: buf(..)
    real, intent(out) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Recv_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_r4

  subroutine mpi_recv_init_fwd_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
      if (idx == MPI_REQUEST_NULL) cycle
      if (req_map_r4(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_recv_init_fwd_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if

    req_map_r4(idx)%ptr_advar = c_loc(buf_ad)
    req_map_r4(idx)%count = count
    req_map_r4(idx)%datatype = datatype
    req_map_r4(idx)%op_type = FAD_MPI_OP_SEND
    call MPI_Send_init(buf_ad, count, datatype, source, tag, comm, req_map_r4(idx)%request, ierr)
    request_ad = idx
  end subroutine mpi_recv_init_fwd_rev_ad_r4

  subroutine mpi_recv_init_rev_ad_r4(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real, intent(inout) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_recv_init_rev_ad_r4"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Request_free(req_map_r4(idx)%request, ierr)
    req_map_r4(idx)%ptr_advar = c_null_ptr
    req_map_r4(idx)%count = 0
    req_map_r4(idx)%datatype = 0
    req_map_r4(idx)%op_type = 0
    req_map_r4(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_recv_init_rev_ad_r4

  subroutine mpi_recv_init_fwd_ad_r8(buf, buf_ad, count, datatype, source, tag, comm, request, request_ad, ierr)
    real(8), intent(out) :: buf(..)
    real(8), intent(out) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierr)
    call MPI_Recv_init(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
  end subroutine mpi_recv_init_fwd_ad_r8

  subroutine mpi_recv_init_fwd_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout), target, contiguous :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(out) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    do idx = 1, MAX_REQUESTS
      if (idx + MAX_REQUESTS == MPI_REQUEST_NULL) cycle
      if (req_map_r8(idx)%request == MPI_REQUEST_NULL) exit
    end do
    if (idx > MAX_REQUESTS) then
      print *, "Error: Too many requests in mpi_recv_init_fwd_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if

    req_map_r8(idx)%ptr_advar = c_loc(buf_ad)
    req_map_r8(idx)%count = count
    req_map_r8(idx)%datatype = datatype
    req_map_r8(idx)%op_type = FAD_MPI_OP_SEND
    call MPI_Send_init(buf_ad, count, datatype, source, tag, comm, req_map_r8(idx)%request, ierr)
    request_ad = idx + MAX_REQUESTS ! Offset to distinguish from real(4) requests
  end subroutine mpi_recv_init_fwd_rev_ad_r8

  subroutine mpi_recv_init_rev_ad_r8(buf_ad, count, datatype, source, tag, comm, request_ad, ierr)
    real(8), intent(inout) :: buf_ad(..)
    integer, intent(in) :: count, datatype, source, tag, comm
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    idx = request_ad - MAX_REQUESTS ! Adjust index for real(8) requests
    if (idx < 1 .or. idx > MAX_REQUESTS) then
      print *, "Error: Invalid request_ad in mpi_recv_init_rev_ad_r8"
      call MPI_abort(comm, -1, ierr)
    end if
    call MPI_Request_free(req_map_r8(idx)%request, ierr)
    req_map_r8(idx)%ptr_advar = c_null_ptr
    req_map_r8(idx)%count = 0
    req_map_r8(idx)%datatype = 0
    req_map_r8(idx)%op_type = 0
    req_map_r8(idx)%request = MPI_REQUEST_NULL
    request_ad = MPI_REQUEST_NULL
  end subroutine mpi_recv_init_rev_ad_r8
  subroutine mpi_start_fwd_ad(request, request_ad, ierr)
    integer, intent(inout) :: request, request_ad
    integer, intent(out), optional :: ierr

    call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
    call MPI_Wait(request_ad, MPI_STATUS_IGNORE, ierr)
  end subroutine mpi_start_fwd_ad

  subroutine mpi_start_rev_ad(request_ad, ierr)
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    if (request_ad < 0 .and. request_ad >= -MAX_REQUESTS) then
      idx = -request_ad
      call MPI_Wait(req_map_r4(idx)%request, MPI_STATUS_IGNORE, ierr)
      call update_advar(req_map_r4(idx))
      return
    else if (request_ad < - MAX_REQUESTS .and. request_ad >= - MAX_REQUESTS * 2) then
      idx = -request_ad - MAX_REQUESTS
      call MPI_Wait(req_map_r8(idx)%request, MPI_STATUS_IGNORE, ierr)
      call update_advar(req_map_r8(idx))
      return
    else
      print *, "Error: Invalid request_ad in mpi_start_rev_ad"
      call MPI_abort(MPI_COMM_WORLD, -1, ierr)
    end if
  end subroutine mpi_start_rev_ad

  subroutine mpi_startall_fwd_ad(count, array_of_requests, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests(count)
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr

    call MPI_Waitall(count, array_of_requests, MPI_STATUSES_IGNORE, ierr)
    call MPI_Waitall(count, array_of_requests_ad, MPI_STATUSES_IGNORE, ierr)
  end subroutine mpi_startall_fwd_ad

  subroutine mpi_startall_rev_ad(count, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr
    integer :: reqs(count)
    integer :: i, idx

    do i = 1, count
      if (array_of_requests_ad(i) == MPI_REQUEST_NULL) then
        reqs(i) = MPI_REQUEST_NULL
        cycle
      end if
      if (array_of_requests_ad(i) < 0 .and. array_of_requests_ad(i) >= -MAX_REQUESTS) then
        idx = -array_of_requests_ad(i)
        reqs(i) = req_map_r4(idx)%request
      else if (array_of_requests_ad(i) < - MAX_REQUESTS .and. array_of_requests_ad(i) >= - MAX_REQUESTS * 2) then
        idx = -array_of_requests_ad(i) - MAX_REQUESTS
        reqs(i) = req_map_r8(idx)%request
      end if
    end do

    call MPI_Waitall(count, reqs, MPI_STATUSES_IGNORE, ierr)

    do i = 1, count
      if (array_of_requests_ad(i) == MPI_REQUEST_NULL) cycle
      if (array_of_requests_ad(i) < 0 .and. array_of_requests_ad(i) >= -MAX_REQUESTS) then
        idx = -array_of_requests_ad(i)
        call update_advar(req_map_r4(idx))
      else if (array_of_requests_ad(i) < - MAX_REQUESTS .and. array_of_requests_ad(i) >= - MAX_REQUESTS * 2) then
        idx = -array_of_requests_ad(i) - MAX_REQUESTS
        call update_advar(req_map_r8(idx))
      end if
    end do
  end subroutine mpi_startall_rev_ad

  subroutine mpi_wait_fwd_ad(request, request_ad, status, status_ad, ierr)
    integer, intent(inout) :: request, request_ad
    integer, intent(inout) :: status(MPI_STATUS_SIZE)
    integer, intent(inout) :: status_ad(MPI_STATUS_SIZE)
    integer, intent(out), optional :: ierr

    call MPI_Wait(request, status, ierr)
    call MPI_Wait(request_ad, status_ad, ierr)
  end subroutine mpi_wait_fwd_ad

  subroutine mpi_wait_rev_ad(request_ad, ierr)
    integer, intent(inout) :: request_ad
    integer, intent(out), optional :: ierr
    real, pointer :: ptr_r4(:)
    real(8), pointer :: ptr_r8(:)
    integer :: idx

    if (request_ad == MPI_REQUEST_NULL) then
      return
    end if
    if (request_ad < 0 .and. request_ad >= -MAX_REQUESTS) then
      idx = -request_ad
      call MPI_Start(req_map_r4(idx)%request, ierr)
      return
    else if (request_ad < - MAX_REQUESTS .and. request_ad >= - MAX_REQUESTS * 2) then
      idx = -request_ad - MAX_REQUESTS
      call MPI_Start(req_map_r8(idx)%request, ierr)
      return
    else if (request_ad > 0 .and. request_ad <= MAX_REQUESTS) then
      idx = request_ad
      select case (req_map_r4(idx)%op_type)
      case (FAD_MPI_OP_SEND)
        call c_f_pointer(req_map_r4(idx)%ptr_advar, ptr_r4, [req_map_r4(idx)%count])
        call MPI_ISEND(ptr_r4, req_map_r4(idx)%count, req_map_r4(idx)%datatype, req_map_r4(idx)%dest, req_map_r4(idx)%tag, req_map_r4(idx)%comm, req_map_r4(idx)%request, ierr)
      case (FAD_MPI_OP_RECV)
        call MPI_IRECV(req_map_r4(idx)%recvbuf, req_map_r4(idx)%count, req_map_r4(idx)%datatype, req_map_r4(idx)%source, req_map_r4(idx)%tag, req_map_r4(idx)%comm, req_map_r4(idx)%request, ierr)
      end select
      return
    else if (request_ad > MAX_REQUESTS .and. request_ad <= MAX_REQUESTS * 2) then
      idx = request_ad - MAX_REQUESTS
      select case (req_map_r8(idx)%op_type)
        case (FAD_MPI_OP_SEND)
          call c_f_pointer(req_map_r8(idx)%ptr_advar, ptr_r8, [req_map_r8(idx)%count])
          call MPI_ISEND(ptr_r8, req_map_r8(idx)%count, req_map_r8(idx)%datatype, req_map_r8(idx)%dest, req_map_r8(idx)%tag, req_map_r8(idx)%comm, req_map_r8(idx)%request, ierr)
        case (FAD_MPI_OP_RECV)
          call MPI_IRECV(req_map_r8(idx)%recvbuf, req_map_r8(idx)%count, req_map_r8(idx)%datatype, req_map_r8(idx)%source, req_map_r8(idx)%tag, req_map_r8(idx)%comm, req_map_r8(idx)%request, ierr)
      end select
      return
    else
      print *, "Error: Invalid request_ad in mpi_wait_rev_ad: ", request_ad
      call MPI_abort(MPI_COMM_WORLD, -1, ierr)
    end if
  end subroutine mpi_wait_rev_ad

  subroutine mpi_waitall_fwd_ad(count, array_of_requests, array_of_requests_ad, statuses, statuses_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests(count)
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(inout) :: statuses(MPI_STATUS_SIZE, count)
    integer, intent(inout) :: statuses_ad(MPI_STATUS_SIZE, count)
    integer, intent(out), optional :: ierr

    call MPI_Waitall(count, array_of_requests, statuses, ierr)
    call MPI_Waitall(count, array_of_requests_ad, statuses_ad, ierr)
  end subroutine mpi_waitall_fwd_ad

  subroutine mpi_waitall_rev_ad(count, array_of_requests_ad, ierr)
    integer, intent(in) :: count
    integer, intent(inout) :: array_of_requests_ad(count)
    integer, intent(out), optional :: ierr
    integer :: req, reqs(count)
    integer :: i, idx
    logical :: flag_persistent

    flag_persistent = .false.
    do i = 1, count
      req = array_of_requests_ad(i)
      if (req == MPI_REQUEST_NULL) then
        reqs(i) = MPI_REQUEST_NULL
        cycle
      end if
      if (req < 0 .and. req >= -MAX_REQUESTS) then
        idx = -req
        reqs(i) = req_map_r4(idx)%request
        flag_persistent = .true.
      else if (req < - MAX_REQUESTS .and. req >= - MAX_REQUESTS * 2) then
        idx = -req - MAX_REQUESTS
        reqs(i) = req_map_r8(idx)%request
        flag_persistent = .true.
      else if (req > 0 .and. req <= MAX_REQUESTS) then
        idx = req
        if (flag_persistent) then
          print *, "Error: Coexistence of persistent and non-persistent requests is not supported in mpi_waitall_rev_ad"
          call MPI_abort(req_map_r4(idx)%comm, -1, ierr)
        end if
        call mpi_wait_rev_ad(req, ierr)
      else if (req <= MAX_REQUESTS * 2) then
        idx = req - MAX_REQUESTS
        if (flag_persistent) then
          print *, "Error: Coexistence of persistent and non-persistent requests is not supported in mpi_waitall_rev_ad"
          call MPI_abort(req_map_r8(idx)%comm, -1, ierr)
        end if
        call mpi_wait_rev_ad(req, ierr)
      else
        print *, "Error: Invalid request_ad in mpi_waitall_rev_ad"
      end if
    end do

    if (flag_persistent) then
      call MPI_Startall(count, reqs, ierr)
    end if
  end subroutine mpi_waitall_rev_ad

  subroutine mpi_barrier_fwd_ad(comm, ierr)
    integer, intent(in) :: comm
    integer, intent(out), optional :: ierr

    call MPI_Barrier(comm, ierr)
  end subroutine mpi_barrier_fwd_ad

  subroutine mpi_barrier_rev_ad(comm, ierr)
    integer, intent(in) :: comm
    integer, intent(out), optional :: ierr

    call MPI_Barrier(comm, ierr)
  end subroutine mpi_barrier_rev_ad

  subroutine mpi_win_fence_fwd_ad(assert, win, ierr)
    integer, intent(in) :: assert
    integer, intent(in) :: win
    integer, intent(out), optional :: ierr

    call MPI_Win_fence(assert, win, ierr)
  end subroutine mpi_win_fence_fwd_ad

  subroutine mpi_win_fence_rev_ad(assert, win, ierr)
    integer, intent(in) :: assert
    integer, intent(in) :: win
    integer, intent(out), optional :: ierr

    call MPI_Win_fence(assert, win, ierr)
  end subroutine mpi_win_fence_rev_ad

  subroutine mpi_comm_group_fwd_ad(comm, group, ierr)
    integer, intent(in) :: comm
    integer, intent(out) :: group
    integer, intent(out), optional :: ierr

    call MPI_Comm_group(comm, group, ierr)
  end subroutine mpi_comm_group_fwd_ad

  subroutine mpi_comm_group_rev_ad(comm, group, ierr)
    integer, intent(in) :: comm
    integer, intent(inout) :: group
    integer, intent(out), optional :: ierr

    call MPI_Comm_group(comm, group, ierr)
  end subroutine mpi_comm_group_rev_ad

  subroutine mpi_group_size_fwd_ad(group, size, ierr)
    integer, intent(in) :: group
    integer, intent(out) :: size
    integer, intent(out), optional :: ierr

    call MPI_Group_size(group, size, ierr)
  end subroutine mpi_group_size_fwd_ad

  subroutine mpi_group_size_rev_ad(group, size, ierr)
    integer, intent(in) :: group
    integer, intent(inout) :: size
    integer, intent(out), optional :: ierr

    call MPI_Group_size(group, size, ierr)
  end subroutine mpi_group_size_rev_ad

  subroutine mpi_group_rank_fwd_ad(group, rank, ierr)
    integer, intent(in) :: group
    integer, intent(out) :: rank
    integer, intent(out), optional :: ierr

    call MPI_Group_rank(group, rank, ierr)
  end subroutine mpi_group_rank_fwd_ad

  subroutine mpi_group_rank_rev_ad(group, rank, ierr)
    integer, intent(in) :: group
    integer, intent(inout) :: rank
    integer, intent(out), optional :: ierr

    call MPI_Group_rank(group, rank, ierr)
  end subroutine mpi_group_rank_rev_ad

  subroutine mpi_group_incl_fwd_ad(group, n, ranks, newgroup, ierr)
    integer, intent(in) :: group, n
    integer, intent(in) :: ranks(n)
    integer, intent(out) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_incl(group, n, ranks, newgroup, ierr)
  end subroutine mpi_group_incl_fwd_ad

  subroutine mpi_group_incl_rev_ad(group, n, ranks, newgroup, ierr)
    integer, intent(in) :: group, n
    integer, intent(in) :: ranks(n)
    integer, intent(inout) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_incl(group, n, ranks, newgroup, ierr)
  end subroutine mpi_group_incl_rev_ad

  subroutine mpi_group_excl_fwd_ad(group, n, ranks, newgroup, ierr)
    integer, intent(in) :: group, n
    integer, intent(in) :: ranks(n)
    integer, intent(out) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_excl(group, n, ranks, newgroup, ierr)
  end subroutine mpi_group_excl_fwd_ad

  subroutine mpi_group_excl_rev_ad(group, n, ranks, newgroup, ierr)
    integer, intent(in) :: group, n
    integer, intent(in) :: ranks(n)
    integer, intent(inout) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_excl(group, n, ranks, newgroup, ierr)
  end subroutine mpi_group_excl_rev_ad

  subroutine mpi_group_union_fwd_ad(group1, group2, newgroup, ierr)
    integer, intent(in) :: group1, group2
    integer, intent(out) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_union(group1, group2, newgroup, ierr)
  end subroutine mpi_group_union_fwd_ad

  subroutine mpi_group_union_rev_ad(group1, group2, newgroup, ierr)
    integer, intent(in) :: group1, group2
    integer, intent(inout) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_union(group1, group2, newgroup, ierr)
  end subroutine mpi_group_union_rev_ad

  subroutine mpi_group_intersection_fwd_ad(group1, group2, newgroup, ierr)
    integer, intent(in) :: group1, group2
    integer, intent(out) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_intersection(group1, group2, newgroup, ierr)
  end subroutine mpi_group_intersection_fwd_ad

  subroutine mpi_group_intersection_rev_ad(group1, group2, newgroup, ierr)
    integer, intent(in) :: group1, group2
    integer, intent(inout) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_intersection(group1, group2, newgroup, ierr)
  end subroutine mpi_group_intersection_rev_ad

  subroutine mpi_group_difference_fwd_ad(group1, group2, newgroup, ierr)
    integer, intent(in) :: group1, group2
    integer, intent(out) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_difference(group1, group2, newgroup, ierr)
  end subroutine mpi_group_difference_fwd_ad

  subroutine mpi_group_difference_rev_ad(group1, group2, newgroup, ierr)
    integer, intent(in) :: group1, group2
    integer, intent(inout) :: newgroup
    integer, intent(out), optional :: ierr

    call MPI_Group_difference(group1, group2, newgroup, ierr)
  end subroutine mpi_group_difference_rev_ad

  subroutine mpi_group_translate_ranks_fwd_ad(group1, n, ranks1, group2, ranks2, ierr)
    integer, intent(in) :: group1, n, group2
    integer, intent(in) :: ranks1(n)
    integer, intent(out) :: ranks2(n)
    integer, intent(out), optional :: ierr

    call MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierr)
  end subroutine mpi_group_translate_ranks_fwd_ad

  subroutine mpi_group_translate_ranks_rev_ad(group1, n, ranks1, group2, ranks2, ierr)
    integer, intent(in) :: group1, n, group2
    integer, intent(in) :: ranks1(n)
    integer, intent(inout) :: ranks2(n)
    integer, intent(out), optional :: ierr

    call MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierr)
  end subroutine mpi_group_translate_ranks_rev_ad

  subroutine mpi_group_free_fwd_ad(group, ierr)
    integer, intent(inout) :: group
    integer, intent(out), optional :: ierr

    call MPI_Group_free(group, ierr)
  end subroutine mpi_group_free_fwd_ad

  subroutine mpi_group_free_rev_ad(group, ierr)
    integer, intent(inout) :: group
    integer, intent(out), optional :: ierr

    call MPI_Group_free(group, ierr)
  end subroutine mpi_group_free_rev_ad

  subroutine mpi_cart_create_fwd_ad(comm_old, ndims, dims, periods, reorder, comm_cart, ierr)
    integer, intent(in) :: comm_old, ndims
    integer, intent(in) :: dims(ndims)
    logical, intent(in) :: periods(ndims)
    logical, intent(in) :: reorder
    integer, intent(out) :: comm_cart
    integer, intent(out), optional :: ierr

    call MPI_Cart_create(comm_old, ndims, dims, periods, reorder, comm_cart, ierr)
  end subroutine mpi_cart_create_fwd_ad

  subroutine mpi_cart_create_rev_ad(comm_old, ndims, dims, periods, reorder, comm_cart, ierr)
    integer, intent(in) :: comm_old, ndims
    integer, intent(in) :: dims(ndims)
    logical, intent(in) :: periods(ndims)
    logical, intent(in) :: reorder
    integer, intent(inout) :: comm_cart
    integer, intent(out), optional :: ierr

    call MPI_Cart_create(comm_old, ndims, dims, periods, reorder, comm_cart, ierr)
  end subroutine mpi_cart_create_rev_ad

  subroutine mpi_cart_sub_fwd_ad(comm, remain_dims, new_comm, ierr)
    integer, intent(in) :: comm
    logical, intent(in) :: remain_dims(:)
    integer, intent(out) :: new_comm
    integer, intent(out), optional :: ierr

    call MPI_Cart_sub(comm, remain_dims, new_comm, ierr)
  end subroutine mpi_cart_sub_fwd_ad

  subroutine mpi_cart_sub_rev_ad(comm, remain_dims, new_comm, ierr)
    integer, intent(in) :: comm
    logical, intent(in) :: remain_dims(:)
    integer, intent(inout) :: new_comm
    integer, intent(out), optional :: ierr

    call MPI_Cart_sub(comm, remain_dims, new_comm, ierr)
  end subroutine mpi_cart_sub_rev_ad

  subroutine mpi_cart_rank_fwd_ad(comm, coords, rank, ierr)
    integer, intent(in) :: comm
    integer, intent(in) :: coords(:)
    integer, intent(out) :: rank
    integer, intent(out), optional :: ierr

    call MPI_Cart_rank(comm, coords, rank, ierr)
  end subroutine mpi_cart_rank_fwd_ad

  subroutine mpi_cart_rank_rev_ad(comm, coords, rank, ierr)
    integer, intent(in) :: comm
    integer, intent(in) :: coords(:)
    integer, intent(inout) :: rank
    integer, intent(out), optional :: ierr

    call MPI_Cart_rank(comm, coords, rank, ierr)
  end subroutine mpi_cart_rank_rev_ad

  subroutine mpi_cart_coords_fwd_ad(comm, rank, maxdims, coords, ierr)
    integer, intent(in) :: comm, rank, maxdims
    integer, intent(out) :: coords(maxdims)
    integer, intent(out), optional :: ierr

    call MPI_Cart_coords(comm, rank, maxdims, coords, ierr)
  end subroutine mpi_cart_coords_fwd_ad

  subroutine mpi_cart_coords_rev_ad(comm, rank, maxdims, coords, ierr)
    integer, intent(in) :: comm, rank, maxdims
    integer, intent(inout) :: coords(maxdims)
    integer, intent(out), optional :: ierr

    call MPI_Cart_coords(comm, rank, maxdims, coords, ierr)
  end subroutine mpi_cart_coords_rev_ad

  subroutine mpi_cart_shift_fwd_ad(comm, direction, disp, source, dest, ierr)
    integer, intent(in) :: comm, direction, disp
    integer, intent(out) :: source, dest
    integer, intent(out), optional :: ierr

    call MPI_Cart_shift(comm, direction, disp, source, dest, ierr)
  end subroutine mpi_cart_shift_fwd_ad

  subroutine mpi_cart_shift_rev_ad(comm, direction, disp, source, dest, ierr)
    integer, intent(in) :: comm, direction, disp
    integer, intent(inout) :: source, dest
    integer, intent(out), optional :: ierr

    call MPI_Cart_shift(comm, direction, disp, source, dest, ierr)
  end subroutine mpi_cart_shift_rev_ad

  subroutine mpi_dims_create_fwd_ad(nnodes, ndims, dims, ierr)
    integer, intent(in) :: nnodes, ndims
    integer, intent(inout) :: dims(ndims)
    integer, intent(out), optional :: ierr

    call MPI_Dims_create(nnodes, ndims, dims, ierr)
  end subroutine mpi_dims_create_fwd_ad

  subroutine mpi_dims_create_rev_ad(nnodes, ndims, dims, ierr)
    integer, intent(in) :: nnodes, ndims
    integer, intent(inout) :: dims(ndims)
    integer, intent(out), optional :: ierr

    call MPI_Dims_create(nnodes, ndims, dims, ierr)
  end subroutine mpi_dims_create_rev_ad

  subroutine mpi_error_class_fwd_ad(errorcode, errorclass, ierr)
    integer, intent(in) :: errorcode
    integer, intent(out) :: errorclass
    integer, intent(out), optional :: ierr

    call MPI_Error_class(errorcode, errorclass, ierr)
  end subroutine mpi_error_class_fwd_ad

  subroutine mpi_error_class_rev_ad(errorcode, errorclass, ierr)
    integer, intent(in) :: errorcode
    integer, intent(inout) :: errorclass
    integer, intent(out), optional :: ierr

    call MPI_Error_class(errorcode, errorclass, ierr)
  end subroutine mpi_error_class_rev_ad

  subroutine mpi_error_string_fwd_ad(errorcode, string, resultlen, ierr)
    integer, intent(in) :: errorcode
    character(len=*), intent(out) :: string
    integer, intent(out) :: resultlen
    integer, intent(out), optional :: ierr

    call MPI_Error_string(errorcode, string, resultlen, ierr)
  end subroutine mpi_error_string_fwd_ad

  subroutine mpi_error_string_rev_ad(errorcode, string, resultlen, ierr)
    integer, intent(in) :: errorcode
    character(len=*), intent(inout) :: string
    integer, intent(inout) :: resultlen
    integer, intent(out), optional :: ierr

    call MPI_Error_string(errorcode, string, resultlen, ierr)
  end subroutine mpi_error_string_rev_ad

  subroutine mpi_errhandler_set_fwd_ad(comm, errhandler, ierr)
    integer, intent(in) :: comm, errhandler
    integer, intent(out), optional :: ierr

    call MPI_Errhandler_set(comm, errhandler, ierr)
  end subroutine mpi_errhandler_set_fwd_ad

  subroutine mpi_errhandler_set_rev_ad(comm, errhandler, ierr)
    integer, intent(in) :: comm, errhandler
    integer, intent(out), optional :: ierr

    call MPI_Errhandler_set(comm, errhandler, ierr)
  end subroutine mpi_errhandler_set_rev_ad

  subroutine mpi_errhandler_free_fwd_ad(errhandler, ierr)
    integer, intent(inout) :: errhandler
    integer, intent(out), optional :: ierr

    call MPI_Errhandler_free(errhandler, ierr)
  end subroutine mpi_errhandler_free_fwd_ad

  subroutine mpi_errhandler_free_rev_ad(errhandler, ierr)
    integer, intent(inout) :: errhandler
    integer, intent(out), optional :: ierr

    call MPI_Errhandler_free(errhandler, ierr)
  end subroutine mpi_errhandler_free_rev_ad

  subroutine update_advar(req_map)
    class(req_map_t), intent(inout) :: req_map
    real, pointer :: ptr_r4(:)
    real(8), pointer :: ptr_r8(:)
    integer :: i

    select type(req_map)
    type is (req_map_r4_t)
      call c_f_pointer(req_map%ptr_advar, ptr_r4, [req_map%count])
      if (req_map%op_type == FAD_MPI_OP_RECV) then
          ptr_r4(:) = ptr_r4(:) + req_map%recvbuf(:req_map%count)
          req_map%recvbuf = 0.0
      else if (req_map%op_type == FAD_MPI_OP_SEND) then
          ptr_r4(:) = 0.0
      end if
    type is (req_map_r8_t)
      call c_f_pointer(req_map%ptr_advar, ptr_r8, [req_map%count])
      if (req_map%op_type == FAD_MPI_OP_SEND) then
          ptr_r8(:) = ptr_r8(:) + req_map%recvbuf(:req_map%count)
          req_map%recvbuf = 0.0_8
      else if (req_map%op_type == FAD_MPI_OP_RECV) then
          ptr_r8(:) = 0.0_8
      end if
    end select
  end subroutine update_advar

end module mpi_ad
