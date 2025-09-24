program lecture_03_ex02
    implicit none

    ! Defining variables
    integer :: number_input      ! The original number from the user
    integer :: number_temp       ! A temporary variable to work with
    integer :: reversed_number   ! The reversed version of the number
    integer :: last_digit        ! The last digit extracted in each step

    ! Requesting the user tu introduce a number using the terminal
    write(*, '(/,A)', advance='no') "Introduce an integer to check if it is a palindrome: "
    read *, number_input

    ! Negative numbers are not palindrome
    if (number_input < 0) then
        print '("The number ", i0, " is NOT a palindrome because it is negative")', number_input
        stop ! End the program early
    end if

    ! Initialize variables for the loop
    number_temp = number_input
    reversed_number = 0

    ! Reverse the number
    ! The loop continues until all digits have been processed
    ! When number_temp = number_temp / 10 = 0 due to 
    ! number_temp has just one digit (the last one to proccess)
    do while (number_temp > 0)

        ! Dividing by 10 we have a decimal value
        ! With module we obtain that decimal value
        last_digit = mod(number_temp, 10)

        ! Multiplying by 10 and adding the last digit
        ! to recover the number
        reversed_number = (reversed_number * 10) + last_digit
        
        ! Remove the last digit from the temporary number
        ! with an integer division for next loop step
        number_temp = number_temp / 10 
    end do

    ! Compare the original number with its reversed version and print the result
    if (number_input == reversed_number) then
        print '("The number ", i0, " is a palindrome")', number_input
    else
        print '("The number ", i0, " is NOT a palindrome")', number_input
    end if

end program lecture_03_ex02