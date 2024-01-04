! Basic Modules

module clean

    private
    public :: clean_line, LOWCASE

    character(26), PARAMETER  :: UPCA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
                                 LOCA = 'abcdefghijklmnopqrstuvwxyz'
    character(96) :: uchara
    integer       :: chamask(0:127), iacZup, iaczlo
    logical       :: init_clean_done

    data init_clean_done/.false./

    contains

    subroutine INIT_clean
        implicit none
        INTEGER :: i, j, LL

        chamask = 0

        uchara = " !""#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        LL = len_trim(uchara)
        do i = 1, LL
            j = IACHAR(uchara(i:i))
            chamask(j) = 1
        end do
        iacZup = IACHAR('Z'); iaczlo = IACHAR('z')
        init_clean_done = .true.
    end subroutine INIT_clean

    subroutine CLEAN_line(line)
        implicit none
        character(*), intent(INOUT)  :: line
        integer                     :: ll, i, k
        character(1)                :: aa

        IF (.not. init_clean_done) CALL INIT_clean
        ll = len_trim(line)
        do i = 1, ll
            aa = line(i:i)
            k = IACHAR(aa)
            IF (chamask(k) == 0) line(i:i) = ' '
        end do
    end subroutine CLEAN_line

    subroutine LOWCASE(a)
        implicit none
        character(*), INTENT(INOUT) :: a
        character(1)             :: b
        integer                  :: i, j, k

        j = len_trim(a)
        do k = 1, j
            b = a(k:k)
            i = SCAN(UPCA, b)
            IF (i == 0) CYCLE
            a(k:k) = LOCA(i:i)
        end do

    end subroutine LOWCASE

end module clean
!----------------------------------------------------------------------------------

MODULE nrtype
    use clean

    INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
    INTEGER, PARAMETER :: I2B = SELECTED_INT_KIND(4)
    INTEGER, PARAMETER :: I1B = SELECTED_INT_KIND(2)
    INTEGER, PARAMETER :: SP = KIND(1.0)
    INTEGER, PARAMETER :: DP = KIND(1.0D0)
    INTEGER, PARAMETER :: CP = DP
    INTEGER, PARAMETER :: SPC = KIND((1.0, 1.0))
    INTEGER, PARAMETER :: DPC = KIND((1.0D0, 1.0D0))
    INTEGER, PARAMETER :: LGT = KIND(.true.)

    REAL(SP), PARAMETER :: PI_S = 3.141592653589793238462643383279502884197_sp
    REAL(SP), PARAMETER :: PIO2 = 1.57079632679489661923132169163975144209858_sp
    REAL(SP), PARAMETER :: TWOPI = 6.283185307179586476925286766559005768394_sp
    REAL(SP), PARAMETER :: SQRT2 = 1.41421356237309504880168872420969807856967_sp
    REAL(SP), PARAMETER :: EULER = 0.5772156649015328606065120900824024310422_sp

    REAL(DP), PARAMETER :: PI_D = 3.141592653589793238462643383279502884197_dp
    REAL(DP), PARAMETER :: PIO2_D = 1.57079632679489661923132169163975144209858_dp
    REAL(DP), PARAMETER :: TWOPI_D = 6.283185307179586476925286766559005768394_dp

    !Values related to pi and e
    REAL(DP), PARAMETER   ::  pi = 3.14159265358979323846264338327950288419716939937510_DP
    REAL(DP), PARAMETER   ::  two_pi = 2.0_DP*pi
    REAL(DP), PARAMETER   ::  pi2 = 2.0_DP*pi
    REAL(DP), PARAMETER   ::  twoonpi = 2.0_DP/pi
    REAL(DP), PARAMETER   ::  sqPi2 = 2.50662827463100050241576528481104525d0
    REAL(DP), PARAMETER   ::  pi2sqrt = sqPi2
    REAL(DP), PARAMETER   ::  four_pi = 4.0_DP*pi
    REAL(DP), PARAMETER   ::  four_pi_o3 = four_pi/3.0_DP
    REAL(DP), PARAMETER   ::  pi_over_2 = pi/2.0_DP
    REAL(DP), PARAMETER   ::  natural_e = 2.71828182845904523536028747135266249775724709369995_DP

    !Conversions for radians to degrees and degrees to radians
    REAL(DP), PARAMETER   ::  degrees_to_radians = pi/180.0_DP
    REAL(DP), PARAMETER   ::  radians_to_degrees = 180.0_DP/pi
    REAL(DP), PARAMETER   ::  duet2r = pi/360.0_DP
    REAL(DP), PARAMETER   ::  dueterPi = 2.09439510239319549230842892218633525d0
    REAL(DP), PARAMETER   ::  sqrt_of_pi = 1.77245385090551602729816748334114518_DP

    REAL(DP), parameter      ::  cr2th = .174532925199432957692369076848861271d-1, &
                                cr1th = .872664625997164788461845384244306356d-2

    REAL(DP), parameter ::zero = 0.0_DP, one = 1.0_DP, two = 2.0_DP, three = 3.0_DP, four = 4.0_DP, five = 5.0_DP, &
                           six = 6.0_DP, seven = 7.0_DP, eight = .0_DP, nine = .0_DP, ten = 10.0_DP, eleven = 11.0_DP, &
                           twelve = 12.0_DP, thirteen = 13.0_DP, fourteen = 14.0_DP, fifteen = 15.0_DP, sixteen = 16.0_DP, &
                           seventeen = 17.0_DP, eighteen = 18.0_DP, nineteen = 19.0_DP, twenty = 20.0_DP, hundred = 100.0_DP
    REAL(DP), PARAMETER   :: unmez = 0.5_DP, half = unmez, unqua = 0.25_DP, &
                             unter = 0.333333333333333333333333333333333333_DP, duter = 0.666666666666666666666666666666666667_DP, &
                             quter = 1.33333333333333333333333333333333333_DP, citer = 1.666666666666666666666666666666666667_DP, &
              unses = 0.166666666666666666666666666666666667_DP, unqui = 0.2_DP, uncen = 0.01_DP, undec = 0.1_DP, unven = 0.05_DP, &
                             trdut = 2.08008382305190411453005682435788539_DP, &
                             duunt = 1.25992104989487316476721060727822835_DP, &
                             sr3 = 1.73205080756887729352744634150587237_DP, &
                             sr2 = 1.41421356237309504880168872420969808_DP

    !Identify the smallest non-zero number and the number of significant figures for SP and DP
    REAL(SP), PARAMETER   ::  tiny_SP = TINY(1.0_SP)
    REAL(DP), PARAMETER   ::  tiny_DP = TINY(one)
    REAL(DP), PARAMETER   ::  tiny_CP = TINY(1.0_CP)
    INTEGER, PARAMETER   ::  sig_fig_SP = PRECISION(1.0_SP)
    INTEGER, PARAMETER   ::  sig_fig_DP = PRECISION(one)
    INTEGER, PARAMETER   ::  sig_fig_CP = PRECISION(1.0_CP)
    REAL(SP), PARAMETER   ::  eps_SP = epsilon(1.0_SP)
    REAL(DP), PARAMETER   ::  eps_DP = epsilon(one)
    REAL(DP), PARAMETER   ::  eps_CP = epsilon(1.0_CP)
    REAL(DP), save         ::  sceps_SP = 1.0e-3
    REAL(DP), save           ::  sceps_DP = 1.0d-8
    REAL(DP), save           ::  sceps_CP = 1.0d-8
    REAL(DP), save           ::  s4eps_SP = 0.03
    REAL(DP), save           ::  s4eps_DP = 1.0d-4
    REAL(DP), save           ::  s4eps_CP = 1.0d-4

    !Identify the largest number for SP and DP
    REAL(SP), PARAMETER   ::  biggest_SP = HUGE(1.0_SP)
    REAL(DP), PARAMETER   ::  biggest_DP = HUGE(one)
    REAL(DP), PARAMETER   ::  biggest_CP = HUGE(1.0_CP)
    INTEGER(I1B), PARAMETER   ::  biggest_I1B = HUGE(1_I1B)
    INTEGER(I2B), PARAMETER   ::  biggest_I2B = HUGE(1_I2B)
    INTEGER(I4B), PARAMETER   ::  biggest_I4B = HUGE(1_I4B)


    CONTAINS

    
    FUNCTION FIND_UNIT()

        INTEGER  :: iu
        INTEGER  :: FIND_UNIT
        LOGICAL       :: busy_unit

        !!!!! Select a free unit
        iuloop: do iu = 10, 99
            inquire (UNIT=iu, OPENED=busy_unit)
            IF (.not. (busy_unit)) THEN
                FIND_UNIT = iu
                EXIT iuloop
            END IF
        end do iuloop
    END FUNCTION FIND_UNIT
    
    subroutine GET_PWD(pwd, lpwd)
        implicit none
        character(LEN=*), intent(INOUT) :: pwd
        integer, intent(OUT)       :: lpwd
        integer :: iupwd
        character(1) :: separator
        character(5), parameter :: delete_command = 'rm -f'

        separator = '/'
        pwd = ''
        call system('pwd > tmp.pwd')
        iupwd = find_unit()
        open (iupwd, status='old', action='read', file='tmp.pwd')
        read (iupwd, '(a)') pwd
        pwd = trim(adjustl(pwd))
        lpwd = len_trim(pwd)
        if (pwd(lpwd:lpwd) /= separator) then
            lpwd = lpwd + 1
            pwd(lpwd:lpwd) = separator
        end if
        close (iupwd)
        call system(trim(delete_command)//' tmp.pwd')

    end subroutine GET_PWD
    !******************************************************

end module nrtype
!----------------------------------------------------------------------------------

module system
    use nrtype
    implicit None

    include 'cfg_local.inc'

    character(1),parameter :: separator      = separators(isystem)
    character(7),parameter :: opsystem       = opsystems(isystem)
    character(5),parameter :: delete_command = delete_commands(isystem)
    character(3),parameter :: ls_command     = ls_commands(isystem)
    character(4),parameter :: cp_command     = cp_commands(isystem)
    character(5),parameter :: mkdir_command  = mkdir_commands(isystem)

end module
!----------------------------------------------------------------------------------

module distributions
    use system
    implicit None

    contains

    subroutine int_to_distribution(values_in, dist_out, bins_out)
        implicit None
        integer(I4B), intent(IN)                ::  values_in(:)
        integer(I4B), intent(OUT), allocatable  ::  dist_out(:), bins_out(:)

        integer(I4B)        ::  i, j, max_value, min_value, val

        ! find min & max in values -> find range
        max_value = maxval(values_in)
        min_value = minval(values_in)
        if (allocated(bins_out)) deallocate(bins_out)
        if (allocated(dist_out)) deallocate(dist_out)
        allocate(bins_out(max_value-min_value), dist_out(max_value-min_value))

        ! count frequency
        dist_out = 0
        do i = 1, (max_value-min_value)
            val = min_value + i
            bins_out(i) = val
            do j = lbound(values_in, 1), ubound(values_in, 1)
                if (values_in(j).eq.val) dist_out(i) = dist_out(i) + 1
            end do
        end do

    end subroutine

end module
!----------------------------------------------------------------------------------

module poisson_mod
    use system
    implicit None

    contains

    ! add poisson noise; input real array --> output real array
    ! NOTE: decimals in the input array are discarded, as real is converted
    ! to nearest int before applying the poisson noise
    ! in case the source_array is specified, noise "size" will be calulated from there
    ! but applied to the in_array.
    subroutine variable_poisson_noise_array(in_array, out_array, scale, source_array)
        implicit None
        real(CP), intent(IN), target            ::  in_array(:)
        real(CP), intent(IN), optional, target  ::  source_array(:)
        real(CP), intent(OUT), allocatable      ::  out_array(:)
        real(CP), intent(OUT)                   ::  scale
        
        integer(I4B), allocatable   ::  out_int(:)
        integer(I4B)                ::  i

        real(CP), pointer   ::  used_array(:)
        real(CP)            ::  noiseval

        ! check if the source array is used and choose the target
        if(present(source_array)) then
            used_array => source_array
        else 
            used_array => in_array
        endif

        ! allocate output
        if(allocated(out_array))    deallocate(out_array)
        if(allocated(out_int))      deallocate(out_int)
        allocate(out_array(size(used_array, 1)), out_int(size(used_array, 1)))

        ! iterate over array using the scale
        do i = lbound(used_array, 1), ubound(used_array, 1)
            call poisson(used_array(i)*scale, out_int(i))
        end do 

        ! re-normalize and convert to real
        out_array = real(out_int)/scale

        ! is source array - copy the noise over the right intensity
        if(present(source_array)) then
            do i = lbound(out_array, 1), ubound(out_array, 1)
                noiseval = out_array(i) - used_array(i)
                out_array(i) = in_array(i) + noiseval
            end do
        endif     

    end subroutine

    ! transfer the noise from one array to another
    subroutine transfer_noise(original_array, noisy_array, target_array)
        implicit None
        real(CP), intent(IN)    ::  original_array(:), noisy_array(:)
        real(CP), intent(OUT)   ::  target_array(:)

        integer(I4B)    ::  i
        real(CP)        ::  noise

        ! copy noise over
        do i = lbound(original_array, 1), ubound(original_array, 1)
            noise = noisy_array(i) - original_array(i)
            target_array(i) = target_array(i) + noise
        end do     

    end subroutine

    ! applies the same noise level to the whole array, using avg_value for reference
    subroutine const_poisson_noise_array(in_array, out_array, avg_value)
        implicit None
        real(CP), intent(IN)                    ::  in_array(:)
        real(CP), intent(OUT), allocatable      ::  out_array(:)
        real(CP), intent(OUT)                   ::  avg_value

        integer(I4B)                ::  i

        ! allocate output
        if(allocated(out_array))    deallocate(out_array)
        allocate(out_array(size(in_array, 1)))

        ! iterate over array
        do i = lbound(in_array, 1), ubound(in_array, 1)
            call const_poisson(in_array(i), out_array(i), avg_value)
        end do 

    end subroutine

    ! input     --> an array of numbers (float)
    ! output    --> same array with poisson noise added (int)
    subroutine poisson_noise_array(in_array, out_array)
        implicit None
        real(CP), intent(IN)                    ::  in_array(:)
        integer(I4B), intent(OUT), allocatable  ::  out_array(:)

        integer(I4B)    ::  i

        ! allocate output
        if(allocated(out_array)) deallocate(out_array)
        allocate(out_array(size(in_array, 1)))

        ! iterate over array
        do i = lbound(in_array, 1), ubound(in_array, 1)
            call poisson(in_array(i), out_array(i))
        end do

    end subroutine

    ! Takes a avg_value and treats it as the avg of a poisson distribution
    ! Returns the input number modified in order to add a poisson noise centered
    ! on the provided avg_value
    subroutine const_poisson(num_in, num_out, avg_value)
        implicit None
        real(CP), intent(IN)        ::  num_in, avg_value
        real(CP), intent(OUT)       ::  num_out

        integer(I4B)    ::  noisy

        call poisson(avg_value, noisy)
        num_out = num_in + real(noisy) - real(nint(avg_value))


    end subroutine

    ! Takes a number and treats it as the avg of a poisson distribution
    ! Returns a random number belonging to the resulting distributon
    subroutine poisson(num_in, num_out)
        implicit None
        real(CP), intent(IN)        ::  num_in
        integer(I4B), intent(OUT)   ::  num_out

        integer(I4B)    ::  method, mode, stabilize_variance

        ! threshold values - hardcoded
        real(CP), parameter :: prod_thres   = 30.0d0  ! upper limit to use the product method
        real(CP), parameter :: boxmul_thres = 87.0d0  ! upper limit to use (stablized) Box–Muller method
        real(CP), parameter :: stabil_thres = 500.0d0 ! upper limit to use stabilized Acceptance-rejection method

        ! check if negative
        if ( num_in .lt. zero ) then
            num_out = int(num_in)
            return
        end if

        ! method selection
        if (num_in .lt. prod_thres) then
            num_out = prod_poisson(num_in)                      ! use the product method

        else if (num_in .lt. boxmul_thres) then
            num_out = poisson_from_gauss(num_in, 0, .true.)     ! use poisson from gaussian (sabilized Box–Muller)

        else if (num_in .lt. stabil_thres) then
            num_out = poisson_from_gauss(num_in, 1, .true.)     ! use poisson from gaussian (sabilized Acceptance-rejection)

        else
            num_out = poisson_from_gauss(num_in, 0, .false.)    ! use poisson from gaussian (simple Box–Muller)

        end if

    end subroutine

    ! Poisson by product method
    function prod_poisson(num_in)
        implicit None
        real(CP), intent(IN)        ::  num_in
        integer(I4B)                ::  prod_poisson

        real(CP)                    ::  rnd, ref

        prod_poisson=0

        CALL random_number(ref)
        do
            if (ref .lt. exp(-num_in)) exit
            CALL random_number(rnd)
            ref = ref*rnd
            prod_poisson = prod_poisson+1
        end do

    end function

    ! Poisson by approx of a Gaussian distribution
    ! (optional) mode: (0 - default) Box–Muller, (1) Acceptance-rejection
    ! (optional) stabilize_variance: (FALSE - default) large nums, (TRUE) small nums
    function poisson_from_gauss(num_in, mode, stabilize_variance)
        implicit None
        real(CP), intent(in)                ::  num_in
        integer(I4B), intent(in), optional  ::  mode
        logical, intent(in), optional       ::  stabilize_variance
        integer(I4B)                        ::  poisson_from_gauss
        
        integer(I4B)    :: def_mode
        logical         :: def_stabilize

        if (present(mode)) then
            def_mode = mode
        else
            def_mode = 0    ! Box–Muller is default
        end if

        if (present(stabilize_variance)) then
            def_stabilize = stabilize_variance
        else
            def_stabilize = .false.    ! unstabilized is default
        end if

        if (def_stabilize) then
            ! stabilzied variance
            poisson_from_gauss=nint( (half*normal_rnd(mode) + sqrt(num_in))**2 - 0.33d0 )
        else
            poisson_from_gauss=nint( num_in + sqrt(num_in)*normal_rnd(mode) )
        end if
        if (poisson_from_gauss .lt. zero) poisson_from_gauss = zero

    end function

    ! Generate a rnd number form a normal distribution
    ! (optional) mode: (0 - default) Box–Muller, (1) Acceptance-rejection
    function normal_rnd(mode)
        implicit None
        integer(I4B), intent(in), optional  ::  mode
        real(CP)    ::  normal_rnd

        integer(I4B)    ::  def_mode

        if (present(mode)) then
            def_mode = mode
        else
            def_mode = 0    ! Box–Muller is default
        end if
        
        if (def_mode .eq. 0) then
            normal_rnd = box_muller()

        else if (def_mode .eq. 1) then
            normal_rnd = normal_accept_reject()

        else
            normal_rnd = half ! this is an error state. Default to 0.5.
        end if

    end function

    ! Box–Muller transform for generating a random number from with 
    ! normal distribution from 2 rand numbers with a uniform distribution (0,1)
    ! ans = R cos(phi) = sqrt( -2*ln(RAND1) ) * cos ( 2*pi*RAND2)
    function box_muller()
        implicit None
        real(CP)    ::  box_muller
        real(CP)    ::  rnd1, rnd2

        CALL random_number(rnd1)
        CALL random_number(rnd2)
        box_muller = sqrt( -two*log(rnd1) )*cos(two_pi*rnd2)

    end function

    ! Acceptance-rejection method for generating a random number from with 
    ! normal disstribution from 2 rand numbers with a uniform distribution (0,1)
    function normal_accept_reject()
        implicit None
        real(CP)    ::  normal_accept_reject
        real(CP)    ::  v, u
        real(CP)    ::  sqrt_tw_e = sqrt(two/natural_e)
        logical     ::  pass

        pass=.false.
        do
            if (pass) exit
            CALL random_number(u)
            CALL random_number(v)
            v = (two*v-one)*sqrt_tw_e
            pass =  ( (unqua*v*v/(u*u)) .le. -log(u) )
        end do
        normal_accept_reject = v/u
    end function

end module

