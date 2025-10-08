! Modify the program to read the simulation setup from a file given as a command line argument.
! Modify the program to write the simulation data into a file “output.dat”, where each line
! contains the simulation state as ”time p1x p1y p1z p2x p2y p2z ... pnx pny pnz”.
! Write a Makefile for the project

program leapfrog
    use geometry
    use particle
    implicit none
    integer :: i, j, k
    integer :: n
    real :: dt, t_end, t, dt_out, t_out
    real :: rs, r2, r3

    real, dimension(:), allocatable :: m
    real, dimension(:,:), allocatable :: r,v,a
    real, dimension(3) :: rji

    character(len=300) :: setup_file_name
    print *, 'Insert name of the simulation setup file:'
    read *, setup_file_name

    ! open and read file with the input data
    open(unit=10, file=setup_file_name, status='old', action='read')

    ! create and write file for the output of the simulation
    open(unit=20, file='output.dat', status='replace', action='write')

    read(10, *) dt
    read(10, *) dt_out
    read(10, *) t_end
    read(10, *) n

    allocate(m(n))
    allocate(r(n,3))
    allocate(v(n,3))
    allocate(a(n,3))

    do i = 1, n
        read(10, *) m(i), r(i,:),v(i,:)
    end do

    !print *, dt, dt_out, t_end, n
    !print *, m
    ! print *, r
    ! print *, r(1,:)
    ! print *, r(1,1)
    !print *, v

    a = 0.0
    do i = 1,n
        do j = i+1,n
            rji = r(j,:) - r(i,:)
            r2 = sum(rji**2)
            if (r2 /= 0.0) then
                r3 = r2 * sqrt(r2)
                a(i,:) = a(i,:) + m(j) * rji / r3  ! Acceleration on particle i due to particle j
                a(j,:) = a(j,:) - m(i) * rji / r3  ! Acceleration on particle j due to particle i
            end if
        end do
    end do

    t_out = 0.0
    t = 0.0
    do while (t <= t_end)
        v = v + a * dt/2
        r = r + v * dt
        a = 0.0
        do i = 1,n
            do j = i+1,n
                rji = r(j,:) - r(i,:)
                r2 = sum(rji**2)
                if (r2 /= 0.0) then
                    r3 = r2 * sqrt(r2)
                    a(i,:) = a(i,:) + m(j) * rji / r3
                    a(j,:) = a(j,:) - m(i) * rji / r3
                end if
            end do
        end do

        v = v + a * dt/2

        t_out = t_out + dt
        if (t_out >= dt_out) then
            ! do i = 1,n
            !     !print*, r(i,:)
            !     write(20, *) t, r(i,:)
            ! end do
            write(20, *) t, r(:,:)

            t_out = 0.0
        end if

        t = t + dt
    end do

    close(10)  
    close(20)
end program leapfrog
