# FMPL Addition

## Example
```fortran
program main
    use fmpl
    implicit none
    
    ! Declare variables
    type(fmp_number) :: num1, num2, result
    character(len=100) :: str1, str2

    ! Numbers to add
    str1 = '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
    str2 = '98765432109876543210987654321098765432109876543210987654321098765432109876543210'

    ! Initialize the numbers
    call fmp_init(num1, str1)
    call fmp_init(num2, str2)

    ! Print the initialized numbers
    print *, 'Number 1:'
    call fmp_print(num1)
    print *, 'Number 2:'
    call fmp_print(num2)

    ! Add the numbers
    call fmp_add(num1, num2, result)
    print *, 'Sum:'
    call fmp_print(result)
    ! Output should be:
    ! 111111111011111111101111111110111111111011111111101111111110111111111011111111100

    ! Clean up
    call fmp_drop(num1)
    call fmp_drop(num2)
    call fmp_drop(result)
end program
```
