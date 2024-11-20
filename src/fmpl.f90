module fmpl
    use terminalite
    implicit none
    private

    ! Define the base as a power of 10
    integer(kind=8), parameter :: base_power = 4  ! Change this to adjust the base (e.g., 3 for 1,000)
    integer(kind=8), parameter :: base = 10**base_power

    ! Compute digits per group based on the base
    integer, parameter :: digits_per_group = base_power

    ! Maximum number of digits (adjust as needed)
    integer(kind=8), parameter :: max_digits = 1000000000000_8 * base_power

    type :: fmp_number
        integer, dimension(:), allocatable :: digits
        integer :: length
    end type fmp_number

    public :: fmp_number, fmp_init, fmp_print, fmp_add, fmp_subtract, fmp_mul, fmp_div, &
              fmp_pow, fmp_mod, fmp_drop, fmp_compare, fmp_powmod, fmp_gcd, fmp_lcm, &
              fmp_sqrt, fmp_cuberoot, fmp_is_prime, fmp_to_string, fmp_zero, fmp_one, &
              fmp_two, fmp_from_real

contains

    ! Helper function to generate format strings
    pure function get_format(first_group) result(fmt)
        logical, intent(in) :: first_group
        character(len=20) :: fmt

        if (first_group) then
            ! Most significant group: no leading zeros
            write (fmt, '(A,I0,A)') '(I', digits_per_group, ')'
        else
            ! Remaining groups: leading zeros using I format
            write (fmt, '(A,I0,A)') '(I', digits_per_group, ')'
        end if
    end function get_format

    subroutine fmp_init(num, str)
        type(fmp_number), intent(out) :: num
        character(len=*), intent(in) :: str
        integer :: i, len, start, end_pos, group

        if (len_trim(str)/digits_per_group > max_digits) then
            call template('[FG:F00F00][BOLD]Error: Number exceeds maximum digits[RESET]')
            stop
        end if

        len = len_trim(str)
        num%length = ceiling(real(len)/real(digits_per_group))
        allocate (num%digits(num%length))

        do i = 1, num%length
            start = len - (i - 1)*digits_per_group
            end_pos = max(start - (digits_per_group - 1), 1)
            group = 0
            if (start >= digits_per_group) then
                read (str(end_pos:start), '(I'//trim(adjustl(itoa(digits_per_group)))//')') group
            else
                read (str(1:start), '(I'//trim(adjustl(itoa(digits_per_group)))//')') group
            end if
            num%digits(i) = group
        end do
    end subroutine fmp_init

    subroutine fmp_print(num)
        type(fmp_number), intent(in) :: num
        integer :: i
        character(len=20) :: fmt
        character(len=1000000) :: num_str  ! Adjust length as needed
        character(len=digits_per_group) :: group_str

        if (num%length > 0) then
            num_str = ''  ! Initialize the string

            ! Convert the most significant group without leading zeros
            fmt = get_format(.true.)
            write (group_str, fmt) num%digits(num%length)
            num_str = adjustl(trim(group_str))

            ! Convert the remaining groups with leading zeros
            do i = num%length - 1, 1, -1
                fmt = get_format(.false.)
                write (group_str, fmt) num%digits(i)

                ! Replace leading spaces with zeros for non-most significant groups
                if (len_trim(group_str) < digits_per_group) then
                    group_str = repeat('0', digits_per_group - len_trim(group_str))//trim(group_str)
                end if

                num_str = trim(adjustl(num_str))//trim(adjustl(group_str))
            end do

            ! Print the complete number
            write (*, '(A)') trim(adjustl(num_str))
        else
            write (*, '(A)') '0'
        end if
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

        write (str_num, '(F0.16)') real_num  ! Adjust format as needed
        len = len_trim(str_num)
        decimal_pos = index(str_num, '.')

        if (decimal_pos > 0) then
            len = len - 1
        end if

        num%length = ceiling(real(len)/real(digits_per_group))
        allocate (num%digits(num%length))
        num%digits = 0

        do i = 1, len
            if (i /= decimal_pos) then
                if (i < decimal_pos) then
                    num%digits(ceiling(real(i)/real(digits_per_group))) = &
                        num%digits(ceiling(real(i)/real(digits_per_group)))*10 + &
                        (ichar(str_num(i:i)) - ichar('0'))
                else
                    num%digits(ceiling(real((i - 1))/real(digits_per_group))) = &
                        num%digits(ceiling(real((i - 1))/real(digits_per_group)))*10 + &
                        (ichar(str_num(i:i)) - ichar('0'))
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
        integer :: i, j, carry, temp_prod, max_len

        max_len = a%length + b%length
        allocate (result%digits(max_len))
        result%length = max_len
        result%digits = 0

        do i = 1, a%length
            carry = 0
            do j = 1, b%length
                temp_prod = a%digits(i)*b%digits(j) + result%digits(i + j - 1) + carry
                carry = temp_prod/base
                result%digits(i + j - 1) = mod(temp_prod, base)
            end do
            result%digits(i + b%length) = carry
        end do

        if (result%digits(max_len) == 0) result%length = result%length - 1
    end subroutine fmp_mul

    subroutine fmp_div(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: remainder, temp_remainder
        integer :: i, max_len, quotient
        type(fmp_number) :: temp

        if (b%length == 1 .and. b%digits(1) == 0) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

        max_len = a%length
        allocate (result%digits(max_len))
        result%length = max_len

        remainder = a
        result%digits = 0

        do i = a%length, 1, -1
            quotient = 0
            do while (fmp_compare(remainder, b) >= 0)
                quotient = quotient + 1
                call fmp_subtract(remainder, b, temp_remainder)
                remainder = temp_remainder
            end do
            result%digits(i) = quotient
        end do

        do i = max_len, 1, -1
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
            call fmp_drop(temp)
        end do
    end subroutine fmp_pow

    subroutine fmp_mod(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: remainder, temp_remainder
        integer :: i, max_len

        if (b%length == 1 .and. b%digits(1) == 0) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

        remainder = a

        do while (fmp_compare(remainder, b) >= 0)
            call fmp_subtract(remainder, b, temp_remainder)
            remainder = temp_remainder
        end do

        result = remainder
    end subroutine fmp_mod

    subroutine fmp_powmod(num, exp, mod, result)
        type(fmp_number), intent(in) :: num, mod
        integer, intent(in) :: exp
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp, base
        integer :: i

        if (mod%length == 1 .and. mod%digits(1) == 0) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

        call fmp_one(result)
        base = num

        do i = 1, exp
            call fmp_mul(result, base, temp)
            call fmp_mod(temp, mod, result)
            call fmp_drop(temp)
        end do
    end subroutine fmp_powmod

    subroutine fmp_gcd(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: temp, temp_remainder

        if ((a%length == 1 .and. a%digits(1) == 0) .or. &
            (b%length == 1 .and. b%digits(1) == 0)) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

        result = a
        temp = b

        do while (temp%length /= 0)
            call fmp_mod(result, temp, temp_remainder)
            result = temp_remainder
            temp = result
        end do
    end subroutine fmp_gcd

    subroutine fmp_lcm(a, b, result)
        type(fmp_number), intent(in) :: a, b
        type(fmp_number), intent(out) :: result
        type(fmp_number) :: gcd, temp

        if ((a%length == 1 .and. a%digits(1) == 0) .or. &
            (b%length == 1 .and. b%digits(1) == 0)) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

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

        if (num%length == 1 .and. num%digits(1) == 0) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

        len = num%length
        max_len = ceiling(real(len)/2.0)
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
                    call fmp_drop(temp)
                    exit
                end if
                call fmp_drop(temp)
            end do
            result%digits(i) = digit
            call fmp_zero(temp2)
            if (i > 1) then
                temp2%length = i
                allocate (temp2%digits(i))
                temp2%digits = 0
                do k = 1, i - 1
                    temp2%digits(k) = result%digits(k)
                end do
            end if
            call fmp_mul(temp2, temp2, temp3)
            call fmp_subtract(num, temp3, temp)
            num = temp
            call fmp_drop(temp2)
            call fmp_drop(temp3)
            call fmp_drop(temp)
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

        if (num%length == 1 .and. num%digits(1) == 0) then
            call template('[FG:F00F00][BOLD]Error: Division by zero[RESET]')
            stop
        end if

        len = num%length
        max_len = ceiling(real(len)/real(digits_per_group))
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
                    call fmp_drop(temp)
                    call fmp_drop(temp2)
                    exit
                end if
                call fmp_drop(temp)
                call fmp_drop(temp2)
            end do
            result%digits(i) = digit
            call fmp_zero(temp2)
            if (i > 1) then
                temp2%length = i
                allocate (temp2%digits(i))
                temp2%digits = 0
                do k = 1, i - 1
                    temp2%digits(k) = result%digits(k)
                end do
            end if
            call fmp_mul(temp2, temp2, temp3)
            call fmp_mul(temp3, temp2, temp)
            call fmp_subtract(num, temp, temp2)
            num = temp2
            call fmp_drop(temp2)
            call fmp_drop(temp3)
            call fmp_drop(temp)
        end do

        do i = max_len, 1, -1
            if (result%digits(i) /= 0) exit
            result%length = result%length - 1
        end do
    end subroutine fmp_cuberoot

    function fmp_is_prime(num) result(is_prime)
        type(fmp_number), intent(inout) :: num
        logical :: is_prime
        type(fmp_number) :: i, temp, sqrt_num, temp_i, one, two

        call fmp_sqrt(num, sqrt_num)
        call fmp_two(i)
        call fmp_one(one)
        call fmp_two(two)
        is_prime = .true.

        do while (fmp_compare(i, sqrt_num) <= 0)
            call fmp_mod(num, i, temp)
            if (fmp_compare(temp, one) == 0) then
                is_prime = .false.
                exit
            end if
            call fmp_add(i, two, temp_i)
            i = temp_i
            call fmp_drop(temp_i)
        end do

        call fmp_drop(i)
        call fmp_drop(temp)
        call fmp_drop(sqrt_num)
        call fmp_drop(one)
        call fmp_drop(two)
    end function fmp_is_prime

    subroutine fmp_to_string(num, str)
        type(fmp_number), intent(in) :: num
        character(len=*), intent(out) :: str
        integer :: i
        character(len=20) :: fmt
        character(len=digits_per_group) :: group_str
        integer :: pos

        str = ''  ! Initialize the string
        pos = 1

        do i = num%length, 1, -1
            if (i == num%length) then
                fmt = get_format(.true.)
            else
                fmt = get_format(.false.)
            end if
            write (group_str, fmt) num%digits(i)
            str(pos:pos + len_trim(group_str) - 1) = trim(group_str)
            pos = pos + len_trim(group_str)
        end do
    end subroutine fmp_to_string

    subroutine fmp_drop(num)
        type(fmp_number), intent(inout) :: num
        deallocate (num%digits)
        num%length = 0
    end subroutine fmp_drop

    function fmp_compare(a, b) result(res)
        type(fmp_number), intent(in) :: a, b
        integer :: res, i

        if (a%length > b%length) then
            res = 1
        elseif (a%length < b%length) then
            res = -1
        else
            res = 0
            do i = a%length, 1, -1
                if (a%digits(i) > b%digits(i)) then
                    res = 1
                    return
                elseif (a%digits(i) < b%digits(i)) then
                    res = -1
                    return
                end if
            end do
        end if
    end function fmp_compare

end module fmpl
