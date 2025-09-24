PROGRAM lecture_03_ex02
    IMPLICIT NONE

    ! Defining variables
    INTEGER :: number_input      ! The original number from the user
    INTEGER :: number_temp       ! A temporary variable to work with
    INTEGER :: reversed_number   ! The reversed version of the number
    INTEGER :: last_digit        ! The last digit extracted in each step

    ! Requesting the user tu introduce a number using the terminal
    WRITE(*, '(/,A)', advance='no') "Introduce an integer to check if it is a palindrome: "
    READ *, number_input

    ! Negative numbers are not palindrome
    IF (number_input < 0) THEN
        PRINT '("The number ", I0, " is NOT a palindrome.")', number_input
        STOP ! End the program early
    END IF

    ! Initialize variables for the loop
    number_temp = number_input
    reversed_number = 0

    ! Reverse the number
    ! The loop continues until all digits have been processed
    ! When number_temp = number_temp / 10 = 0 due to 
    ! number_temp has just one digit (the last one to proccess)
    DO WHILE (number_temp > 0)

        ! Dividing by 10 we have a decimal value
        ! With module we obtain that decimal value
        last_digit = MOD(number_temp, 10)

        ! Multiplying by 10 and adding the last digit
        ! to recover the number
        reversed_number = (reversed_number * 10) + last_digit
        
        ! Remove the last digit from the temporary number
        ! with an integer division for next loop step
        number_temp = number_temp / 10 
    END DO

    ! Compare the original number with its reversed version and print the result
    IF (number_input == reversed_number) THEN
        PRINT '("The number ", I0, " is a palindrome.")', number_input
    ELSE
        PRINT '("The number ", I0, " is NOT a palindrome.")', number_input
    END IF

END PROGRAM lecture_03_ex02