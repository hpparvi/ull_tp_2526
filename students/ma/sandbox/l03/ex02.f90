program palindrome
    implicit none
    integer :: n, rev, real_num

    print *, "Enter a number"
    read *, n

    ! If the number only have one digit it would be a palindrome

    rev = 0
    real_num = n

    if (n < 10) then
        print *, "The number is a palindrome"

    ! The mod give the remainder of the division and by dividing by 10 we get the last digit
    else
        do while (n>0) !  We do that because we are going to divide n by 10 each iteration
            rev = rev*10+mod(n,10) ! We construct the reverse number
            n =n/10 ! We remove the last digit is like a counter
        end do

        if (real_num == rev) then
            print *, "The number is a palindrome"
        else
            print *, "The number is not a palindrome"
        end if
    end if

end program palindrome