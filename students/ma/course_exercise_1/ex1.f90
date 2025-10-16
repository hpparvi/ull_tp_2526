program ex1
  use geometry
  use particles
  implicit none

  integer :: i, j
  integer :: n ! Number of particules
  real(dp) :: dt, t_end, t, dt_out, t_out
  real(dp) :: rs, r3
  integer :: ierr ! This is to check and deallocate
  character(len=100) :: infile   ! Name of the input file
  character(len=200) :: in ! argument in the terminal for the input file


  type(particle3d), dimension(:), allocatable :: part ! It would be an array of particles
  type(vector3d), dimension(:), allocatable :: a ! We define particle acceleration
  type(vector3d) :: rji

  ! We modify the program to read the input data from a file in the terminal

  call get_command_argument(1,in)

  if (len_trim(in)==0) then ! If input is not given
     print *, "Input example: ./ex1 <inputfile>"
     stop
  endif

  infile = trim(in) ! remove empty space
  print *, "Reading input file: ", trim(infile)

  open(unit=10, file=infile, status='old', action='read', iostat=ierr)
  if (ierr /= 0) then 
     print *, "Error: The file doesn't exist or it cannot be open: ", trim(infile)
     stop
  endif


  read(10,*) dt
  read(10,*) dt_out
  read(10,*) t_end
  read(10,*) n

  ! We are going to associate the components of the particle type for simplicity, and now we read the last lines, n times to allocate n particles

  allocate(part(n))
  allocate(a(n)) 

  do i = 1,n
     read(10,*) part(i)%m, & 
                part(i)%p%x, part(i)%p%y, part(i)%p%z, &
                part(i)%v%x, part(i)%v%y, part(i)%v%z
  end do

  close(10)

  do i = 1, n
    a(i) = vector3d(0.0_dp, 0.0_dp, 0.0_dp)
  end do

  do i = 1,n
     do j = i+1,n
           rji = vector3d(part(j)%p%x-part(i)%p%x, part(j)%p%y-part(i)%p%y, part(j)%p%z-part(i)%p%z) 
           rs = distance(part(j)%p, part(i)%p) ! Here we calculate the distance with the function
           r3 = rs**3 ! Since the output of distance is the sqrt
           a(i) = a(i) + (part(j)%m * rji) / r3
           a(j) = a(j) - (part(i)%m * rji) / r3
        end do
  end do

 ! We create the output file
  open(unit=12, file='output.dat', status='replace', action='write')

  t_out = 0.0


  ! If we change the number of particles the code could be slower, therefore it is convenient to write messages in order to clarify that all is 
  ! running smoothly


  print *, "Starting the simulation..."
  print *, "Number of particles:", n
  print *, "Total time:", t_end, "   Timestep:", dt
  print *, "---------------------------------------------"



  do t = 0.0, t_end, dt
   do i= 1,n
     part(i)%v = part(i)%v + (a(i) * dt)/2.0_dp
     part(i)%p = part(i)%p + (part(i)%v * dt) ! Here we are using the supv of the geometry module
     a(i) = vector3d(0.0,0.0,0.0)
   end do
     do i = 1,n
        do j = i+1,n
           rji = vector3d(part(j)%p%x-part(i)%p%x, part(j)%p%y-part(i)%p%y, part(j)%p%z-part(i)%p%z) 
           rs = distance(part(j)%p, part(i)%p) 
           r3 = rs**3 
           a(i) = a(i) + (part(j)%m * rji) / r3
           a(j) = a(j) - (part(i)%m * rji) / r3
        end do
     end do
     
     do i = 1, n
         part(i)%v = part(i)%v + a(i) * dt/2.0_dp
     end do ! Here we are using the multvr of the geometry module

     ! Now we write in the output data the position of the particles each time
     
     t_out = t_out + dt
     if (t_out >= dt_out) then
        write(12,'(ES20.10)', advance='no') t ! With advance no we avoid to make a split of line
        do i = 1,n
           write(12,'(3(1X,ES20.10))', advance='no') part(i)%p
        end do
        write(12,*) 
        t_out = 0.0
     end if
  end do

  ! we close the file

  print *, "Simulation complete :) Particles positions save in output.dat"
  close(12)

  ! Finally we check the status of the arrays and deallocate

 if (allocated(part)) deallocate(part, stat=ierr)
 if (allocated(a)) deallocate(a, stat=ierr)

end program ex1










