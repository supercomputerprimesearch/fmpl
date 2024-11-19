module fmpl
    implicit none
    private

    integer(kind=8), parameter :: max_digits = 1000000000000_8 ! 1 trillion digits
    integer, parameter :: base = 10

    type :: fmp_number
        integer, dimension(:), allocatable :: digits
        integer :: length
    end type fmp_number

    ! Everything is public but helper functions
    public :: fmp_number, fmp_init, fmp_print, fmp_add, fmp_subtract, fmp_mul, fmp_div, fmp_pow, fmp_mod, fmp_drop, fmp_compare, fmp_powmod, fmp_gcd, fmp_lcm, fmp_sqrt

contains

    subroutine fmp_init(num, str)
        type(fmp_number), intent(out) :: num
        character(len=*), intent(in) :: str
        integer :: i, len

        len = len_trim(str)
        allocate (num%digits(len))
        num%length = len

        do i = 1, len
            num%digits(i) = ichar(str(len - i + 1:len - i + 1)) - ichar('0')
        end do
    end subroutine fmp_init

    subroutine fmp_print(num)
        type(fmp_number), intent(in) :: num
        integer :: i

        do i = num%length, 1, -1
            write (*, '(I1)', advance='no') num%digits(i)
        end do
        write (*, *)
    end subroutine fmp_print

    subroutine fmp_zero(num)
        type(fmp_number), intent(out) :: num
        allocate (num%digits(1))
        num%digits(1) = 0
        num%length = 1
    end subroutine fmp_zero

    subroutine fmp_one(num)
        type(fmp_number), intent(out) :: num
        allocate (num%digits(1))
        num%digits(1) = 1
        num%length = 1
    end subroutine fmp_one

    subroutine fmp_two(num)
        type(fmp_number), intent(out) :: num
        allocate (num%digits(1))
        num%digits(1) = 2
        num%length = 1
    end subroutine fmp_two

    subroutine fmp_from_real(num, real_num)
        type(fmp_number), intent(out) :: num
        real, intent(in) :: real_num
        character(len=32) :: str_num
        integer :: i, len, decimal_pos

        ! Convert real number to string
        write (str_num, '(F32.16)') real_num

        ! Remove trailing spaces
        len = len_trim(str_num)

        ! Find the position of the decimal point
        decimal_pos = index(str_num, '.')

        ! Initialize the fmp_number type
        allocate (num%digits(len - 1))  ! Exclude the decimal point
        num%length = len - 1

        ! Convert string digits to integer array
        do i = 1, len
            if (i /= decimal_pos) then
                if (i < decimal_pos) then
                    num%digits(i) = ichar(str_num(i:i)) - ichar('0')
                else
                    num%digits(i - 1) = ichar(str_num(i:i)) - ichar('0')
                end if
            end if
        end do
    end subroutine fmp_from_real

    subroutine fmp_add(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        integer :: carry, i, max_len

        max_len = max(a%length, b%length) + 1
        allocate (result%digits(max_len))
        result%length = max_len

        carry = 0
        do i = 1, max_len
            result%digits(i) = carry
            if (i <= a%length) result%digits(i) = result%digits(i) + a%digits(i)
            if (i <= b%length) result%digits(i) = result%digits(i) + b%digits(i)
            if (result%digits(i) >= base) then
                result%digits(i) = result%digits(i) - base
                carry = 1
            else
                carry = 0
            end if
        end do

        if (result%digits(max_len) == 0) result%length = result%length - 1
    end subroutine fmp_add

    subroutine fmp_subtract(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        integer :: borrow, i

        allocate (result%digits(a%length))
        result%length = a%length

        borrow = 0
        do i = 1, a%length
            result%digits(i) = a%digits(i) - borrow
            if (i <= b%length) result%digits(i) = result%digits(i) - b%digits(i)
            if (result%digits(i) < 0) then
                result%digits(i) = result%digits(i) + base
                borrow = 1
            else
                borrow = 0
            end if
        end do

        do i = a%length, 1, -1
            if (result%digits(i) /= 0) exit
            result%length = result%length - 1
        end do
    end subroutine fmp_subtract

    subroutine fmp_mul(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        integer :: i, j, carry, max_len
        max_len = a%length + b%length
        allocate (result%digits(max_len))
        result%length = max_len
        result%digits = 0
        do i = 1, a%length
            carry = 0
            do j = 1, b%length
                result%digits(i + j - 1) = result%digits(i + j - 1) + a%digits(i)*b%digits(j) + carry
                carry = result%digits(i + j - 1)/base
                result%digits(i + j - 1) = result%digits(i + j - 1) - carry*base
            end do
            result%digits(i + b%length) = carry
        end do
        if (result%digits(max_len) == 0) result%length = result%length - 1
    end subroutine fmp_mul

    subroutine fmp_div(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: remainder
        integer :: i, max_len, quotient
        type(fmp_number) :: temp_remainder

        max_len = a%length
        allocate (result%digits(max_len))
        result%length = max_len

        remainder = a
        result%digits = 0
        do i = a%length - b%length + 1, 1, -1
            quotient = 0
            do while (fmp_compare(remainder, b) >= 0)
                quotient = quotient + 1
                call fmp_subtract(remainder, b, temp_remainder)
                remainder = temp_remainder
            end do
            result%digits(i) = quotient
        end do

        do i = a%length, 1, -1
            if (result%digits(i) /= 0) exit
            result%length = result%length - 1
        end do
    end subroutine fmp_div

    subroutine fmp_pow(num, exp, result)
        type(fmp_number), intent(in) :: num
        integer, intent(in) :: exp
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp
        integer :: i

        call fmp_init(result, '1')
        do i = 1, exp
            temp = result
            call fmp_mul(temp, num, result)
        end do
        call fmp_drop(temp)
    end subroutine fmp_pow

    subroutine fmp_mod(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        integer :: i, max_len
        type(fmp_number) :: remainder, temp_remainder

        max_len = a%length
        allocate (result%digits(max_len))
        result%length = max_len

        remainder = a
        result%digits = 0

        do while (fmp_compare(remainder, b) >= 0)
            call fmp_subtract(remainder, b, temp_remainder)
            remainder = temp_remainder
        end do

        result = remainder

        do i = result%length, 1, -1
            if (result%digits(i) /= 0) exit
            result%length = result%length - 1
        end do
    end subroutine fmp_mod

    subroutine fmp_powmod(num, exp, mod, result)
        type(fmp_number), intent(in) :: num, mod
        integer, intent(in) :: exp
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp, base
        integer :: i

        call fmp_init(result, '1')
        base = num

        do i = 1, exp
            temp = result
            call fmp_mul(temp, base, result)
            call fmp_mod(temp, mod, result)
        end do

        call fmp_drop(temp)
        call fmp_drop(base)
    end subroutine fmp_powmod

    subroutine fmp_gcd(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp, temp_remainder

        result = a
        temp = b

        do while (temp%length /= 0)
            call fmp_mod(result, temp, temp_remainder)
            result = temp_remainder
            result = temp
            temp = result
        end do

        call fmp_drop(temp)
    end subroutine fmp_gcd

    subroutine fmp_lcm(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: gcd
        type(fmp_number) :: temp

        call fmp_gcd(a, b, gcd)
        call fmp_mul(a, b, temp)
        call fmp_div(temp, gcd, result)

        call fmp_drop(gcd)
        call fmp_drop(temp)
    end subroutine fmp_lcm

    subroutine fmp_sqrt(num, result)
        type(fmp_number), intent(inout) :: num
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp, temp2, temp3
        integer :: i, j, k, max_len, len, digit

        len = num%length
        max_len = (len + 1)/2
        allocate (result%digits(max_len))
        result%length = max_len
        result%digits = 0

        do i = max_len, 1, -1
            digit = 0
            do j = 9, 0, -1
                result%digits(i) = j
                call fmp_mul(result, result, temp)
                if (fmp_compare(temp, num) <= 0) then
                    digit = j
                    exit
                end if
            end do
            result%digits(i) = digit
            call fmp_init(temp2, '0')
            do k = 1, i - 1
                temp2%digits(k) = result%digits(k)
            end do
            temp2%digits(i) = digit
            call fmp_mul(temp2, temp2, temp3)
            call fmp_subtract(num, temp3, temp)
            num = temp
        end do

        do i = max_len, 1, -1
            if (result%digits(i) /= 0) exit
            result%length = result%length - 1
        end do
    end subroutine fmp_sqrt

    subroutine fmp_cuberoot(num, result)
        type(fmp_number), intent(inout) :: num
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp, temp2, temp3
        integer :: i, j, k, max_len, len, digit

        len = num%length
        max_len = (len + 2)/3
        allocate (result%digits(max_len))
        result%length = max_len
        result%digits = 0

        do i = max_len, 1, -1
            digit = 0
            do j = 9, 0, -1
                result%digits(i) = j
                call fmp_mul(result, result, temp)
                call fmp_mul(temp, result, temp2)
                if (fmp_compare(temp2, num) <= 0) then
                    digit = j
                    exit
                end if
            end do
            result%digits(i) = digit
            call fmp_init(temp2, '0')
            do k = 1, i - 1
                temp2%digits(k) = result%digits(k)
            end do
            temp2%digits(i) = digit
            call fmp_mul(temp2, temp2, temp3)
            call fmp_mul(temp3, temp2, temp)
            call fmp_subtract(num, temp, temp2)
            num = temp2
        end do

        do i = max_len, 1, -1
            if (result%digits(i) /= 0) exit
            result%length = result%length - 1
        end do
    end subroutine fmp_cuberoot

    function fmp_is_prime(num) result(is_prime)
        type(fmp_number), intent(inout) :: num
        logical :: is_prime
        type(fmp_number) :: i, temp, sqrt_num, temp_i

        call fmp_sqrt(num, sqrt_num)
        call fmp_init(i, '2')
        is_prime = .true.

        do while (fmp_compare(i, sqrt_num) <= 0)
            call fmp_mod(num, i, temp)
            if (fmp_compare(temp, i) == 0) then
                is_prime = .false.
                exit
            end if
            call fmp_add(i, i, temp_i)
            i = temp_i
        end do

        call fmp_drop(i)
        call fmp_drop(temp)
        call fmp_drop(sqrt_num)
    end function fmp_is_prime

    subroutine fmp_to_string(num, str)
        type(fmp_number), intent(in) :: num
        character(len=*), intent(out) :: str
        integer :: i

        str = ''
        do i = num%length, 1, -1
            write (str(len(str) + 1:len(str) + 1), '(I1)') num%digits(i)
        end do
    end subroutine fmp_to_string

    subroutine fmp_drop(num)
        type(fmp_number), intent(inout) :: num
        deallocate (num%digits)
    end subroutine fmp_drop

    function fmp_compare(a, b) result(res)
        type(fmp_number), intent(in) :: a, b
        integer :: res, i

        if (a%length > b%length) then
            res = 1
        elseif (a%length < b%length) then
            res = -1
        else
            do i = a%length, 1, -1
                if (a%digits(i) > b%digits(i)) then
                    res = 1
                    return
                elseif (a%digits(i) < b%digits(i)) then
                    res = -1
                    return
                end if
            end do
            res = 0
        end if
    end function fmp_compare

end module fmpl
