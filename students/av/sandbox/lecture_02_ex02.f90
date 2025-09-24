program lecture_02_ex02
    implicit none

    integer :: i, n
    integer(kind=8) :: fact = 1

    ! Loop for validate the user input
    do
        ! Requesting the user to introduce a nautural number using the terminal
        write(*, '(/,a)', advance='no') "Introduce a natural number (>= 0): "
        read * , n

        ! Exit loop if the number is correct
        if (n >= 0) exit

        ! Requesting to repeat the input number with a valid one
        print *, "Error: Number must be non-negative (>= 0). Try it again."
    end do

    print '(/,"Factorial numbers from 0 to ", i0, " are:")', n

    ! Factorial of 0
    print '(i0, "! = ", i0)', 0, 1

    ! Computing the factorial numbers from 1 to n
    do i = 1, n
        fact = fact * i
        print '(i0, "! = ", i0)', i, fact
    end do

end program lecture_02_ex02