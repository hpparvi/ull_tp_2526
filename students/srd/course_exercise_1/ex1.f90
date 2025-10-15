program leapfrog
  use geometry
  use particle
  implicit none

  integer :: i, j
  integer :: n
  real(kind=8) :: dt, t_end, t, dt_out, t_out ! Or with SELECTED_REAL_KIND for more portability
  real(kind=8) :: r2, r3
  
  type(particle3d), allocatable :: p(:) ! We re-define those using the definitions from the modules
  type(vector3d), allocatable :: a(:)
  type(vector3d) :: rji
  character(len=100) :: infile
  integer :: ios, iu

  ! The name of the input file for the initial conditions
  infile = "input_ex.dat"

  iu = 10
  open(unit=iu, file=trim(infile), status='old', action='read', iostat=ios)
  if (ios /= 0) then
     print *, "Error: can't open file ", trim(infile)
     stop
  end if

  read(iu, *) dt 		! In the example input we have the parameters ordered as here
  read(iu, *) dt_out
  read(iu, *) t_end
  read(iu, *) n

  allocate(p(n))
  allocate(a(n))

  ! Read particles (masses, positions and velocities) from the input file 
  do i = 1, n
     read(iu, *) p(i)%m, p(i)%p%x, p(i)%p%y, p(i)%p%z, &
                  p(i)%v%x, p(i)%v%y, p(i)%v%z
  end do

  close(iu)

  ! Calculating initial acelerations
  a = vector3d(0.0d0, 0.0d0, 0.0d0)
  do i = 1, n
     do j = i + 1, n
        rji%x = p(j)%p%x - p(i)%p%x
        rji%y = p(j)%p%y - p(i)%p%y
        rji%z = p(j)%p%z - p(i)%p%z

        r2 = rji%x**2 + rji%y**2 + rji%z**2
        r3 = r2 * sqrt(r2)

        a(i)%x = a(i)%x + p(j)%m * rji%x / r3
        a(i)%y = a(i)%y + p(j)%m * rji%y / r3
        a(i)%z = a(i)%z + p(j)%m * rji%z / r3

        a(j)%x = a(j)%x - p(i)%m * rji%x / r3
        a(j)%y = a(j)%y - p(i)%m * rji%y / r3
        a(j)%z = a(j)%z - p(i)%m * rji%z / r3
     end do
  end do

  ! Creating the output file
  open(unit=20, file="output.dat", status="replace", action="write", iostat=ios)
  if (ios /= 0) then
     print *, "Error: can't create output.dat"
     stop
  end if

  ! Integration using Leapfrog method
  t_out = 0.0d0
  do t = 0.0d0, t_end, dt

     do i = 1, n
        p(i)%v = p(i)%v + (0.5d0 * dt) * a(i)
     end do

     do i = 1, n
        p(i)%p = p(i)%p + dt * p(i)%v
     end do

     a = vector3d(0.0d0, 0.0d0, 0.0d0)
     do i = 1, n
        do j = i + 1, n
           rji%x = p(j)%p%x - p(i)%p%x
           rji%y = p(j)%p%y - p(i)%p%y
           rji%z = p(j)%p%z - p(i)%p%z

           r2 = rji%x**2 + rji%y**2 + rji%z**2
           r3 = r2 * sqrt(r2)

           a(i)%x = a(i)%x + p(j)%m * rji%x / r3
           a(i)%y = a(i)%y + p(j)%m * rji%y / r3
           a(i)%z = a(i)%z + p(j)%m * rji%z / r3

           a(j)%x = a(j)%x - p(i)%m * rji%x / r3
           a(j)%y = a(j)%y - p(i)%m * rji%y / r3
           a(j)%z = a(j)%z - p(i)%m * rji%z / r3
        end do
     end do

     do i = 1, n
        p(i)%v = p(i)%v + (0.5d0 * dt) * a(i)
     end do

     t_out = t_out + dt
     if (t_out >= dt_out) then
        write(20,'(F12.6, 999(F12.6))') t, (p(i)%p%x, p(i)%p%y, p(i)%p%z, i=1,n)
        t_out = 0.0d0
     end if

  end do

  close(20)

end program leapfrog

