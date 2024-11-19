program test_fmpl
    use fmpl
    implicit none

    type(fmp_number) :: num1, num2, result, power_result
    character(len=100) :: str1, str2
    integer :: exponent

    ! Initialize large numbers
    str1 = '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
    str2 = '98765432109876543210987654321098765432109876543210987654321098765432109876543210'

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

    ! Subtract the numbers
    call fmp_subtract(num2, num1, result)
    print *, 'Difference:'
    call fmp_print(result)

    ! Multiply the numbers
    call fmp_mul(num1, num2, result)
    print *, 'Product:'
    call fmp_print(result)

    ! Divide the numbers
    call fmp_div(num2, num1, result)
    print *, 'Quotient:'
    call fmp_print(result)

    ! Power operation
    exponent = 2
    call fmp_pow(num1, exponent, power_result)
    print *, 'Power (num1^2):'
    call fmp_print(power_result)

    ! Compare the numbers
    print *, 'Comparison (num1 vs num2):', fmp_compare(num1, num2)

    ! Modulo operation
    call fmp_mod(num1, num2, result)
    print *, 'Modulus:'
    call fmp_print(result)

    ! Clean up
    call fmp_drop(num1)
    call fmp_drop(num2)
    call fmp_drop(result)
    call fmp_drop(power_result)

end program test_fmpl
