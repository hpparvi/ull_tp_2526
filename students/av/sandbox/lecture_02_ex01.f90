program lecture_02_ex01
    implicit none

    integer :: i, n

    ! Loop for validate the user input
    do
        ! Requesting the user to introduce a nautural number using the terminal
        write(*, '(/,a)', advance='no') "How many times do you want to print the message: "
        read * , n

        ! Exit loop if the number is correct
        if (n >= 0) exit

        ! Requesting to repeat the input number with a valid one
        print *, "Error: Number must be non-negative (>= 0). Try it again."
    end do

    ! Loop to print the message n times
    print *
    do i = 1, n, 1
        write(*, '(a)') "Hello World!"
    end do

end program lecture_02_ex01


