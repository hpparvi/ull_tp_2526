program palindrome
    implicit none
    integer :: n
    character(len=50) :: numStr, revStr ! Can also be done with allocate to a dynamical lenght
    integer :: i, lenN

    print *, "Introduce a positive integer to check if it's a palindrome:"
    read *, n

    ! Convertir a cadena
    write(numStr, '(I0)') n   ! I0: convert without spaces
    lenN = len_trim(numStr)   ! number of digits

    ! String with the number read backwards
    revStr = ""
    do i = 1, lenN
        revStr(i:i) = numStr(lenN-i+1:lenN-i+1)
    end do

    ! Compare the two numbers ( we trim to delete the blank spaces )
    if (trim(numStr(1:lenN)) == trim(revStr(1:lenN))) then
        print *, n, "Is a palindrome!"
    else
        print *, n, "Is not a palindrome"
    end if

end program palindrome

