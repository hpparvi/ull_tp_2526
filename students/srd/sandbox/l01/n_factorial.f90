program hello
    implicit none
    ! Declaración de variables
    INTEGER :: N, i, factorial
    ! Input del usuario
    print *, "Insert value of N (integer)"
    READ *, N
    ! Printeamos todos los factoriales desde 1 hasta N
    print *, "Factorials from 0 to",N
    ! El factorial de cero siempre será 1
    print *, "1"
    factorial = 1
    DO i= 1,N,1
        factorial = i * factorial
        print *, factorial
    END DO
end program hello
