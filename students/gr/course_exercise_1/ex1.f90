program leapfrog
    use iso_fortran_env, only: real64
    use geometry
    use particle
    implicit none
    integer :: i, j
    integer :: n
    real(real64) :: dt, t_end, t, dt_out, t_out
    real(real64) :: r3

    ! p: array containing particles (each component is type(particle3d))
    type(particle3d), dimension(:), allocatable :: p
    ! a: array containing acelerations (each coomponent is type(vector3d))
    type(vector3d), dimension(:), allocatable :: a

    type(vector3d) :: rji
    real(real64) :: dist    ! distance between two points

    character(len=300) :: setup_file_name
    print *, 'Insert name of the simulation setup file:'
    read *, setup_file_name

    ! Open and read file with the input data
    open(unit=10, file=setup_file_name, status='old', action='read')

    ! Create and write file for the output of the simulation
    open(unit=20, file='output.dat', status='replace', action='write')

    read(10, *) dt
    read(10, *) dt_out
    read(10, *) t_end
    read(10, *) n

    allocate(p(n))
    allocate(a(n))

    do i = 1, n
        read(10, *) p(i)%m, p(i)%p, p(i)%v
    end do

    a = vector3d(0.0, 0.0, 0.0)  ! Initialize all elements of 'a' to zero

    do i = 1,n
        do j = i+1,n
            rji = p(j)%p - p(i)%p
            dist = distance(p(i)%p, p(j)%p)
            r3 = dist * dist**2
            a(i) = a(i) + p(j)%m * rji / r3  ! Acceleration on particle i due to particle j
            a(j) = a(j) - p(i)%m * rji / r3  ! Acceleration on particle j due to particle i
        end do
    end do

    t_out = 0.0
    t = 0.0
    do while (t <= t_end)

        do i = 1,n
            p(i)%v = p(i)%v + a(i) * (dt/2)
            p(i)%p = p(i)%p + p(i)%v * dt
        end do

        a = vector3d(0.0, 0.0, 0.0)
        
        do i = 1,n
            do j = i+1,n
                rji = p(j)%p - p(i)%p
                dist = distance(p(i)%p, p(j)%p)
                r3 = dist * dist**2
                a(i) = a(i) + p(j)%m * rji / r3
                a(j) = a(j) - p(i)%m * rji / r3
            end do
        end do

        do i = 1,n
            p(i)%v = p(i)%v + a(i) * (dt/2)
        end do

        t_out = t_out + dt
        if (t_out >= dt_out) then
            write(20, *) t, p%p

            t_out = 0.0
        end if

        t = t + dt
    end do

    close(10)  
    close(20)
end program leapfrog
