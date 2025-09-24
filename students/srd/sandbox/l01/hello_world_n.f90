program hello
    implicit none
    ! Declaración de variables
    INTEGER :: N, i
    ! Input del usuario
    print *, "Insert value of N (integer)"
    READ *, N
    ! Bucle de N veces
    DO i = 1,N,1
        print *, "Hello world!!!"
    END DO
end program hello
