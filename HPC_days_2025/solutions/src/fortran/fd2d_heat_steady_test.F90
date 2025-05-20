program main

!*****************************************************************************80
!
!! FD2D_HEAT_STEADY_TEST() tests FD2D_HEAT_STEADY().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
  use f90getopt
  
  implicit none

  integer nx
  integer ny
  integer stat

  integer, parameter :: rk = kind ( 1.0D+00 )

  ! START for longopts only (optional)
  ! ----------------------------------
  ! option_s derived type:
  !   1st value = long option name (character array, max. 80)
  !   2nd value = if option has an argument (logical)
  !   3rd value = short option name (single character), same as in getopt()
  ! option_s is not needed if you just use short options
  type(option_s) :: opts(2)
  opts(1) = option_s("nx", .true., "x")
  opts(2) = option_s("ny",  .true.,  "y")
  ! END longopts

  nx = 41
  ny = 21


  ! START Processing options
  ! ------------------------
  ! Process short options one by one and long options if specified in option_s
  !
  ! getopt(optstr, longopt):
  !  - optstr  = character array of short option characters without a space
  !              colon ":" after a character says that this option requires an argument
  !  - longopt = long option declaration, if specified in option_s (optional)
  do
      select case(getopt("x:y:", opts))
          case(char(0)) ! When all options are processed
              exit
          case("x")
              read(optarg, *, iostat=stat) nx
              if ( stat /= 0 ) then
                print *,'Conversion of string ',optarg,' failed!'
              endif
          case("y")
              read(optarg, *, iostat=stat) ny
              if ( stat /= 0 ) then
                print *,'Conversion of string ',optarg,' failed!'
              endif
      end select
  end do
  ! END processing options



  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD2D_HEAT_STEADY_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FD2D_HEAT_STEADY library.'

  call test01 (nx, ny )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD2D_HEAT_STEADY_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end

subroutine test01 (nx, ny )

!*****************************************************************************80
!
!! TEST01 computes the solution for a steady state heat equation problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
  use iso_c_binding
  use smartredis_client, only : client_type
  implicit none

#include "enum_fortran.inc"

  type(client_type) :: client
  

  integer, parameter :: rk = kind ( 1.0D+00 )

  character ( len = 80 ) command_filename
  integer command_unit
  real ( kind = rk ), external :: d
  character ( len = 80 ) data_filename
  integer data_unit
  real ( kind = rk ), external :: f
  integer i
  integer j
  integer nx
  integer ny
  real ( kind = rk ), allocatable :: umat(:,:)
  real ( kind = rk ) u_mean
  real ( kind = rk ), allocatable :: xmat(:,:)
  real ( kind = rk ), allocatable :: xvec(:)
  real ( kind = rk ), allocatable :: ymat(:,:)
  real ( kind = rk ), allocatable :: yvec(:)

  integer result
!
!  Specify the spatial grid.
!
  allocate ( xvec(1:nx) )
  call r8vec_linspace ( nx, 0.0D+00, 2.0D+00, xvec )

  allocate ( yvec(1:ny) )
  call r8vec_linspace ( ny, 0.0D+00, 1.0D+00, yvec )

  allocate ( xmat(1:nx,1:ny) )
  allocate ( ymat(1:nx,1:ny) )
  call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
!
!  Solve the finite difference approximation to the steady 2D heat equation.
!
  allocate ( umat(1:nx,1:ny) )
  call fd2d_heat_steady ( nx, ny, xvec, yvec, d, f, umat )

!
!   Start a SmartRedis Client
!   Put the umat, xmat, and ymat arrays as "tensors" on the DB, their keys should be
!   "steady_state_u", "steady_state_x", and "steady_state_y"
!

  result = client%initialize(.false., "heated_plate") ! Change .false. to .true. if not using a clustered database
  if (result .ne. SRNoError) error stop 'client%initialize failed'

  ! Send a tensor to the database via the client and verify that we can retrieve it
  result = client%put_tensor("steady_state_u", umat, shape(umat))
  if (result .ne. SRNoError) error stop 'client%put_tensor failed'
  result = client%put_tensor("steady_state_x", xmat, shape(xmat))
  if (result .ne. SRNoError) error stop 'client%put_tensor failed'
  result = client%put_tensor("steady_state_y", ymat, shape(ymat))
  if (result .ne. SRNoError) error stop 'client%put_tensor failed'
!
!  Report the average value of U.
!
  u_mean = sum ( umat(1:nx,1:ny) ) / real ( nx * ny, kind = rk )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Mean value of U is ', u_mean
!
!  Free memory.
!
  deallocate ( umat )
  deallocate ( xmat )
  deallocate ( xvec )
  deallocate ( ymat )
  deallocate ( yvec )

  return
end
function d ( x, y )

!*****************************************************************************80
!
!! D evaluates the heat conductivity coefficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, Y, the evaluation point.
!
!    Output, real ( kind = rk ) D, the value of the heat conductivity at (X,Y).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) d
  real ( kind = rk ) x
  real ( kind = rk ) y

  call r8_fake_use ( x )
  call r8_fake_use ( y )

  d = 1.0D+00

  return
end
function f ( x, y )

!*****************************************************************************80
!
!! F evaluates the heat source term.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, Y, the evaluation point.
!
!    Output, real ( kind = rk ) F, the value of the heat source term at (X,Y).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) f
  real ( kind = rk ) x
  real ( kind = rk ) y

  call r8_fake_use ( x )
  call r8_fake_use ( y )

  f = 0.0D+00

  return
end
subroutine boundary ( nx, ny, x, y, n, a, rhs )

!*****************************************************************************80
!
!! BOUNDARY sets up the matrix and right hand side at boundary nodes.
!
!  Discussion:
!
!    For this simple problem, the boundary conditions specify that the solution
!    is 100 on the left side, and insulated on the right, top and bottom.
!
!    Nodes are assigned a single index K, which increases as:
!
!    (NY-1)*NX+1  (NY-1)*NX+2  ...  NY * NX
!           ....         ....  ...    .....
!           NX+1         NX+2  ...   2 * NX
!              1            2  ...       NX
!
!    The index K of a node on the lower boundary satisfies:
!      1 <= K <= NX
!    The index K of a node on the upper boundary satisfies:
!      (NY-1)*NX+1 <= K <= NY * NX
!    The index K of a node on the left boundary satisfies:
!      mod ( K, NX ) = 1
!    The index K of a node on the right boundary satisfies:
!      mod ( K, NX ) = 0
!
!    If we number rows from bottom I = 1 to top I = NY
!    and columns from left J = 1 to right J = NX, then the relationship
!    between the single index K and the row and column indices I and J is:
!      K = ( I - 1 ) * NX + J
!    and
!      J = 1 + mod ( K - 1, NX )
!      I = 1 + ( K - J ) / NX
!      
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NX, NY, the number of grid points in X and Y.
!
!    Input, real ( kind = rk ) X(NX), Y(NY), the coordinates of grid lines.
!
!    Input, integer N, the number of nodes.
!
!    Input/output, real ( kind = rk ) A(N,N).  On input, the system matrix, with the 
!    entries for the interior nodes filled in.  On output, the entries for
!    the boundary nodes have been set as well.
!
!    Input, real ( kind = rk ) RHS(N), on input, the system right hand side, 
!    with the entries for the interior nodes filled in.  On output, the entries for
!    the boundary nodes have been set as well.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n
  integer nx
  integer ny

  real ( kind = rk ) a(n,n)
  integer i
  integer j
  integer kc
  real ( kind = rk ) rhs(n)
  real ( kind = rk ) x(n)
  real ( kind = rk ) y(n)

  call r8_fake_use ( x(1) )
  call r8_fake_use ( y(1) )
!
!  Left boundary.
!
  j = 1
  do i = 2, ny - 1
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 10.0D+00
  end do
!
!  Right boundary.
!
  j = nx
  do i = 2, ny - 1
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 100.0D+00
  end do
!
!  Lower boundary.
!
  i = 1
  do j = 1, nx
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 0.0D+00
  end do
!
!  Upper boundary.
!
  i = ny
  do j = 1, nx
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 0.0D+00
  end do

  return
end

