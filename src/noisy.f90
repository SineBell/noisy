! main noisy

program noisy

    use poisson_mod

    implicit None

    ! args handling
    character(len=512), allocatable ::  args(:)
    integer(I4B)                    ::  num_args, i
    logical                         ::  file_check

    ! options
    real(CP), allocatable, target   ::  source_int(:), ext_noise(:)
    real(CP)                        ::  scale, const_val, to_add, norm, modul
    logical                         ::  silent, onlyint, const, do_norm, do_mod, use_source, &
                                        noise_out, noise_inc, use_ext

    ! file handling
    character(len=:), allocatable   ::  newname
    integer(I4B)                    ::  type_code

    ! data
    real(CP), allocatable, target   ::  intensity(:), mod_intensity(:) 
    real(CP), allocatable           ::  noisy_intensity(:), cal_data(:,:), justnoise(:)
    real(CP), pointer               ::  used_int(:)
    ! help topic management
    logical                         ::  help_call=.false.

    ! hello!
    write(*,*)
    write(*,*) "  noisy - Poisson noise for simulated intesities."
    write(*,*) "  ver 0.9.0 (Gen 2024)"
    write(*,*) "  Nicola Dengo, Università degli Studi dell'Insubria"
    write(*,*) "  nicola.dengo [at] uninsubria.it"
    write(*,*)

    ! look for the passed traces and options
    call lookup_args()

    if (num_args.le.0) then
        call sugg_info()
        write(*,*) "  WARNING: no file passed. Please provide a suitable file:"
        
        num_args = 1
        if(allocated(args)) deallocate(args)
        allocate(args(1))
        read(*,'(A)') args

    end if

    ! iterate over args
    do i = 1, num_args

        !print*, trim(args(i))
        if(.not.silent) then    ! show input file
            write(*,*) ""
            write(*,*) "  Input file:"
            write(*,*) "    "//trim(args(i))
        end if

        ! inquire on arg
        inquire(file=trim(args(i)), exist=file_check)
        if (.not.file_check) then
            write(*,*) "  WARNING: file "//trim(args(i))//" cannot be opened and will not be processed."
            call sugg_info()
            cycle
        end if

        ! check file type
        call get_file_type(trim(args(i)))
        if (type_code.le.0) then
            write(*,*) "  WARNING: file "//trim(args(i))//" has an unknown format and will not be processed."
            call sugg_info()
            cycle
        end if

        ! clear arrays (redundant)
        if(allocated(intensity))        deallocate(intensity)
        if(allocated(mod_intensity))    deallocate(mod_intensity)
        if(allocated(noisy_intensity))  deallocate(noisy_intensity)
        if(allocated(cal_data))         deallocate(cal_data)
        nullify(used_int)

        ! get intensity array from file
        if (onlyint) then
            call get_data(trim(args(i)))              ! only int
        else
            call get_data(trim(args(i)), cal_data)    ! get full data from .cal
        end if
        if(.not.allocated(intensity)) then
            write(*,*) "  WARNING: data on "//trim(args(i))//" could not be read and will not be processed."
            call sugg_info()
            cycle
        end if

        ! move the used intesities to source if allocated and proper sized
        used_int => intensity   ! use internal source
        if(allocated(source_int)) then
            if (size(source_int, 1).eq.size(intensity, 1)) then
                used_int => source_int  ! use external source
            else 
                write(*,*) "  WARNING: --source data has a different size in respect to the file "//trim(args(i))
                write(*,*) "  Internal data will be used as source."
                call sugg_info()
            end if
        end if

        ! --- intensity variations destined to the output

        ! normalize data norm
        if (do_norm) call norm_data(used_int, norm)

        ! do the addition if to_add is not zero
        if (abs(to_add).gt.sceps_DP) used_int = used_int + to_add

        ! if internal variations are required
        if (use_source) then
            ! --- internal intensity variations

            ! allocate mod intensities and copy over
            allocate(mod_intensity(size(used_int, 1)))
            mod_intensity = used_int

            ! modulate data to modulate noise
            if (do_mod) call modulate_data(mod_intensity, modul)

        end if

        ! add noise
        if (const) then
            call const_poisson_noise_array(used_int, noisy_intensity, const_val)   ! constant noise
        else
            if (use_ext) then   ! external pre-calculated noise
                call add_ext_noise(used_int, noisy_intensity, ext_noise)
            else
                if (use_source) then
                    call variable_poisson_noise_array(used_int, noisy_intensity, scale, mod_intensity)     ! proportional noise - mod
                else
                    call variable_poisson_noise_array(used_int, noisy_intensity, scale)                    ! proportional noise - simple
                end if
            end if
        end if

        if(.not.allocated(noisy_intensity)) then
            write(*,*) "  WARNING: impossible to add noise on data "//trim(args(i))
            call sugg_info()
            cycle
        end if

        ! extract only the the noise values
        if(noise_out.or.noise_inc) then
            if(allocated(justnoise))    deallocate(justnoise)
            allocate(justnoise(size(intensity,1)))
            if (use_ext) then
                justnoise = ext_noise
            else
                justnoise = noisy_intensity - used_int
            end if
        endif

        ! in case the whole file is desired
        if(.not.onlyint) then

            ! add the noise column
            if (noise_inc) call append_column(cal_data, justnoise)
            
            ! substitute simulated intensity
            if (type_code.eq.1) then        ! .cal
                cal_data(3,:) = noisy_intensity
            elseif (type_code.eq.2) then    ! .xye or .xy
                cal_data(2,:) = noisy_intensity
            elseif (type_code.eq.3) then    ! .int
                cal_data(1,:) = noisy_intensity
            else
                write(*,*) "  ERROR: corrupted or unkonwn file type code to set the new noisy intensities."
                call sugg_info()
                stop
            end if    
            if(allocated(intensity))        deallocate(intensity)
            if(allocated(mod_intensity))    deallocate(mod_intensity)
            if(allocated(noisy_intensity))  deallocate(noisy_intensity)
            nullify(used_int)
        end if

        ! write output
        call compose_filename(trim(args(i)), newname)
        if(.not.silent) then    ! show output file
            write(*,*) "  Output file:"
            write(*,*) "    "//trim(newname)
        end if

        if(onlyint) then
            call output_single_array_real(trim(newname),noisy_intensity)
        else
            call out_cal(trim(newname),cal_data)
        end if

        ! just-noise output
        if(noise_out) then
            call compose_justnoise_filename(trim(args(i)), newname)
            call output_single_array_real(trim(newname),justnoise)
        endif

    end do

    contains

    ! set options to default values
    subroutine init_options()
        implicit None

        scale       = one           ! multiplicative factor on intensies values
        silent      = .false.       ! show processed input and output files 
        onlyint     = .false.       ! output only intensities
        const       = .false.       ! apply a costant noise level instead of a proportional one
        const_val   = 500d0         ! reference avg value for the costant noise
        to_add      = zero          ! constant number to add to the intesities
        do_norm     = .false.       ! normalize data
        norm        = 1000.0_DP     ! norm value for the intensities
        do_mod      = .false.       ! modulate noise proportionality
        modul       = one           ! noise modulation value
        use_source  = .false.       ! external source for noise intensities
        noise_out   = .false.       ! extra output file with just the noise
        noise_inc   = .false.       ! extra coulumn with just the noise in the output
        use_ext     = .false.       ! get noise values direcly from an external file
    end subroutine

    ! get user-provided arguments
    subroutine lookup_args()
        implicit None

        integer(I4B)    ::  ii

        ! initialize all
        call init_options()

        num_args=COMMAND_ARGUMENT_COUNT()
        if (num_args.le.0) return

        if(allocated(args)) deallocate(args)
        allocate(args(num_args))

        do ii = 1, num_args
            CALL GET_COMMAND_ARGUMENT(ii,args(ii))
        end do
        
        call info_check()

        call check_for_flags()
        
    end subroutine

    ! check for info request
    subroutine info_check()
        implicit none

        integer(I4B) ::  j

        j = lbound(args, 1)
        do  
            if(j.eq.ubound(args, 1)+1) exit

            if (trim(adjustl(args(j)))=="--help") then
                call info()     ! in case help is called, show help topic and stops
                stop
            end if
            j = j + 1
        end do


    end subroutine

    ! search special flags in the args
    subroutine check_for_flags()
        implicit None

        character(512)                  ::  source, ext
        character(len=:), allocatable   ::  str
        character(len=512), allocatable ::  args_copy(:)
        integer(I4B)                    ::  k, j, ioerr, new_num
        logical                         ::  to_keep(size(args,1))

        to_keep=.true.

        if(.not.allocated(args)) then
            write(*,*) "  WARNING: wrong reading of the passed arguments."
            call sugg_info()
            return
        end if

        if(allocated(args_copy)) deallocate(args_copy)
        allocate(args_copy(size(args,1)))
        args_copy = args

        k = lbound(args, 1)
        do  
            if(k.eq.ubound(args, 1)+1) exit

            if(allocated(str)) deallocate(str)
            allocate(character(len=len_trim(args(k))) :: str )
            str=args(k)

            select case (str)

            case ("--scale")    ! choose multiplicative factor on intensies values
                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --scale (multiplicative factor) not specificed. Using default value 1."
                    call sugg_info()
                    to_keep(k)=.false.
                    k = k + 1
                    cycle
                end if
                read(args(k+1), *, iostat=ioerr) scale
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: --scale (multiplicative factor) not specificed. Using default value 1."
                    call sugg_info()
                    to_keep(k)=.false.
                else
                    write(*,*) "  --scale (multiplicative factor) set to: ",scale
                    to_keep(k)=.false.
                    k = k + 1   ! skipping the one with the scale value
                    to_keep(k)=.false.
                end if

            case("--const")     ! set a costant noise level instead of a proportional one
                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --const (costant noise level) not specificed. Using proportional noise."
                    call sugg_info()
                    to_keep(k)=.false.
                    k = k + 1
                    cycle
                end if
                read(args(k+1), *, iostat=ioerr) const_val
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: --const (costant noise level) not specificed. Using proportional noise."
                    call sugg_info()
                    to_keep(k)=.false.
                else
                    write(*,*) "  --const (costant noise level) set to: ",const_val
                    const=.true.
                    to_keep(k)=.false.
                    k = k + 1   ! skipping the one with the value
                    to_keep(k)=.false.
                end if

            case("--add")   ! adds a constant value to the intesities
                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --add (intensity addend) not specificed. Using default value 0."
                    call sugg_info()
                    to_keep(k)=.false.
                    k = k + 1
                    cycle
                end if
                read(args(k+1), *, iostat=ioerr) to_add
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: --add (intensity addend) not specificed. Using default value 0."
                    call sugg_info()
                    to_keep(k)=.false.
                else
                    write(*,*) "  --add (intensity addend) set to: ",to_add
                    to_keep(k)=.false.
                    k = k + 1   ! skipping the one with the value
                    to_keep(k)=.false.
                end if

            case("--norm")   ! normalize the intensities to x.xx (max value)
                do_norm = .true.
                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --norm (intensity normalization) not specificed. Using default value 1000."
                    call sugg_info()
                    to_keep(k)=.false.
                    k = k + 1
                    cycle
                end if
                read(args(k+1), *, iostat=ioerr) norm
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: ---norm (intensity normalization) not specificed. Using default value 1000."
                    call sugg_info()
                    to_keep(k)=.false.
                else
                    write(*,*) "  --norm (intensity normalization) set to: ",norm
                    to_keep(k)=.false.
                    k = k + 1   ! skipping the one with the value
                    to_keep(k)=.false.
                end if

            case("--modulate")  ! limits the proportionality of the noise using a combination of added value and normalization
                use_source  = .true.
                do_mod      = .true.
                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --modulate (proportionality modulation) not specificed. Using default value 1."
                    call sugg_info()
                    to_keep(k)=.false.
                    k = k + 1
                    cycle
                end if
                read(args(k+1), *, iostat=ioerr) modul
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: --modulate (proportionality modulation) not specificed. Using default value 1."
                    call sugg_info()
                    to_keep(k)=.false.
                else
                    write(*,*) "  --modulate (proportionality modulation) set to: ",modul
                    to_keep(k)=.false.
                    k = k + 1   ! skipping the one with the value
                    to_keep(k)=.false.
                end if

            case("--source")  ! outsource the intensities for the noise level to another xye or cal file
                to_keep(k)=.false.

                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --source (external noise input) file not specificed. Ignored."
                    call sugg_info()  
                    k = k + 1
                    cycle
                end if

                ! read filename
                read(args(k+1), '(A)', iostat=ioerr) source
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: --source (external noise input) file "//trim(source)//" not readable. Ignored."
                    call sugg_info()
                    k = k + 1
                    cycle
                end if

                ! check file type
                call get_file_type(trim(source))
                if (type_code.le.0) then
                    write(*,*)  "  WARNING: --source (external noise input) file "//&
                                trim(source)//&
                                " has an unknown format and will not be processed."
                    call sugg_info()
                    k = k + 1
                    cycle
                end if

                ! get data
                call get_data(trim(source))
                if(.not.allocated(source_int)) then
                    write(*,*) "   WARNING: --source (external noise input) file "//trim(source)//" not readable. Ignored."
                    call sugg_info()
                    k = k + 1
                    cycle
                end if

                write(*,*) "  --source (external noise input) set to: ",trim(source)
                use_source=.true.
                k = k + 1   ! skipping the one with the value
                to_keep(k)=.false.

            case("--ext")  ! outsource the noise values
                to_keep(k)=.false.

                if (k.eq.ubound(args, 1)) then
                    write(*,*) "  WARNING: --ext (external noise values) file not specificed. Ignored."
                    call sugg_info()  
                    k = k + 1
                    cycle
                end if

                ! read filename
                read(args(k+1), '(A)', iostat=ioerr) ext
                if(ioerr.ne.0) then
                    write(*,*) "  WARNING: --ext (external noise values) file "//trim(source)//" not readable. Ignored."
                    call sugg_info()
                    k = k + 1
                    cycle
                end if

                ! get data
                call get_noise_from_file(trim(ext), ext_noise)
                if(.not.allocated(ext_noise)) then
                    write(*,*) "   WARNING: --ext (external noise values) file "//trim(source)//" not readable. Ignored."
                    call sugg_info()
                    k = k + 1
                    cycle
                end if

                write(*,*) "  --ext (external noise values) set to: ",trim(ext)
                use_ext=.true.
                k = k + 1   ! skipping the one with the value
                to_keep(k)=.false.
                
            case ("--silent")   ! make less verbose
                silent=.true.
                to_keep(k)=.false.

            case ("--int-only") ! output only the noisy intensity
                onlyint=.true.
                to_keep(k)=.false.

            case("--noise-out") ! output an additional file with only the added noise
                noise_out=.true.
                to_keep(k)=.false.

            case("--noise-include") ! add en extra column in the output with the just the noise
                noise_inc=.true.
                to_keep(k)=.false.

            end select

            k = k + 1
        end do

        ! rebuild args array
        new_num=count(to_keep,1)
        if(new_num.ne.num_args) then
            num_args = new_num
            if(allocated(args)) deallocate(args)
            allocate(args(num_args))
            j = lbound(args, 1)
            do k = lbound(args_copy, 1), ubound(args_copy, 1)
                if(to_keep(k)) then
                    args(j) = args_copy(k)
                    j = j + 1
                end if
            end do
        end if

    end subroutine

    ! get file format: file.cal --> "cal". type_code = 0 means error state
    subroutine get_file_type(file)
        implicit None

        character(len=*), intent(IN)    ::  file

        character(len=3)                ::  form

        type_code=0     ! initialize in error state

        ! safety check
        if (len_trim(file).lt.3) return

        ! get format
        form=file(len_trim(file)-2:)
        if(form==".xy") form="xy " ! quick fix for xy
        select case (form)
        case ("cal")    ! std debussy .cal file
            type_code=1

        case("xye")     ! general .xye file
            type_code=2

        case("xy ")     ! general .xy file
            type_code=2

        case("int")     ! general .int file
            type_code=3
        end select

    end subroutine

    ! get data array from file
    subroutine get_data(file, full_data)
        implicit None

        character(len=*), intent(IN)                    ::  file
        real(CP), intent(INOUT), allocatable, optional  ::  full_data(:,:)

        real(CP), allocatable   ::  temp(:,:)

        ! select the right reading approach based on file fomat
        select case (type_code)
        case (1)    ! Debussy .cal - 3a colonna
            call full_data_from_file(file, temp)
            call intensity_from_cal(temp)
            if (present(full_data)) then
                if (allocated(full_data)) deallocate(full_data) ! full data used
                allocate(full_data, source=temp)
            endif
        case (2)    ! General xy or xye - 2a colonna
            call full_data_from_file(file, temp)
            call intensity_from_xye(temp)
            if (present(full_data)) then
                if (allocated(full_data)) deallocate(full_data) ! full data used
                allocate(full_data, source=temp)
            endif
        case (3)    ! Single - column data with just intensities .int
            call full_data_from_file(file, temp)
            call intensity_from_int(temp)
            if (present(full_data)) then
                if (allocated(full_data)) deallocate(full_data) ! full data used
                allocate(full_data, source=temp)
            endif
        case default
            write(*,*) "  WARNING: file "//trim(file)//" has an unknown format and will not be processed."
            call sugg_info()
            return
        end select


    end subroutine

    ! get external noise from generic file (1st col)
    subroutine get_noise_from_file(file, noise)
        implicit None
        character(*), intent(IN)                        ::  file
        real(CP), intent(OUT), allocatable              ::  noise(:)

        integer(I4B)  ::    ext_unit, ierr, lines

        ext_unit = FIND_UNIT()
        OPEN (  UNIT=ext_unit, status='old', &
                form='formatted', access='sequential', &
                file=trim(file), action='READ', iostat=ierr)
        if(ierr.ne.0) then
            write(*,*) "  WARNING: impossible to read from file "//trim(file)
            write(*,*) "  The file will not be processed."
            call sugg_info()
            return
        end if

        ! count lines
        lines=0
        do 
            read(ext_unit, *, iostat=ierr)
            if(ierr.ne.0) exit
            lines = lines + 1
        end do
        rewind(ext_unit)

        ! if the file is empty
        if(lines.le.0) then
            write(*,*) "  WARNING: the following file is empty. "//trim(file)
            call sugg_info()
            close(ext_unit)
            return
        end if

        ! allocate memory
        if(allocated(noise))     deallocate(noise)
        allocate(noise(lines))

        ! read data
        do i = 1, lines
            read(ext_unit, *, iostat=ierr) noise(i)
            if(ierr.ne.0) then
                write(*,*) "  WARNING: incomplete reading of the file "//trim(file)
                write(*,*) "  An error occured reading the line No ", i
                call sugg_info()
                close(ext_unit)
                return
            end if
        end do

        close(ext_unit)

    end subroutine

    ! get all data from cal
    ! .cal columns: [1: 2theta] [2: exp int] [3: simu int] [4 to (N-2): single phase(s)] [(N-1): bkg] [N: blank]
    subroutine full_data_from_file(file, data)
        implicit None

        character(len=*), intent(IN)        ::  file
        real(CP), intent(OUT), allocatable  ::  data(:,:)

        integer(I4B)            ::  cal_unit, ierr, lines, j, final_cols
        real(CP), allocatable   ::  cols(:)
        character(len=512)      ::  str

        cal_unit = FIND_UNIT()
        OPEN (  UNIT=cal_unit, status='old', &
                form='formatted', access='sequential', &
                file=trim(file), action='READ', iostat=ierr)
        if(ierr.ne.0) then
            write(*,*) "  WARNING: impossible to read from file "//trim(file)
            write(*,*) "  The file will not be processed."
            call sugg_info()
            return
        end if

        ! count lines
        lines=0
        do 
            read(cal_unit, *, iostat=ierr)
            if(ierr.ne.0) exit
            lines = lines + 1
        end do
        rewind(cal_unit)

        ! if the file is empty
        if(lines.le.0) then
            write(*,*) "  WARNING: the following file is empty. "//trim(file)
            call sugg_info()
            close(cal_unit)
            return
        end if

        ! count columns
        read(cal_unit, '(A)', iostat=ierr) str ! read 1 line
        if (ierr.ne.0) then
            write(*,*) "  WARNING: impossible to read from file "//trim(file)
            call sugg_info()
            close(cal_unit)
            return
        end if

        final_cols = 1
        do
            if(allocated(cols)) deallocate(cols)
            allocate(cols(final_cols))

            read(str, *, iostat=ierr) cols(:)
            if (ierr.ne.0) then
                final_cols = final_cols - 1         ! number of cols detected
                if(final_cols.le.0) then
                    write(*,*) "  WARNING: anomalous number of columns in the file "//trim(file)
                    call sugg_info()
                    close(cal_unit)
                    return
                end if
                if(allocated(cols)) deallocate(cols)  
                exit
            end if
            final_cols = final_cols + 1
        end do
        rewind(cal_unit)
        
        ! allocate memory
        if(allocated(data))     deallocate(data)
        allocate(data(final_cols, lines))

        ! read data from .cal
        do j = 1, lines
            read(cal_unit, *, iostat=ierr) data(:,j)
            if(ierr.ne.0) then
                write(*,*) "  WARNING: incomplete reading of the file "//trim(file)
                write(*,*) "  An error occured reading the line No ", j
                call sugg_info()
                close(cal_unit)
                return
            end if
        end do

        close(cal_unit)

    end subroutine

    ! intensity data from .cal data
    subroutine intensity_from_cal(data)
        implicit None
        real(CP), intent(in) ::  data(:,:)

        if(size(data, 1).lt.5) then
            write(*,*) "  WARNING: anomalous number of columns in the file "
            call sugg_info()
            return
        end if

        if(allocated(intensity)) deallocate(intensity)
        allocate(intensity, source=data(3,:))
    end subroutine

    ! intensity data from .xye data
    subroutine intensity_from_xye(data)
        implicit None
        real(CP), intent(in)    ::  data(:,:)

        if(size(data, 1).lt.2) then
            write(*,*) "  WARNING: anomalous number of columns in the file "
            call sugg_info()
            return
        end if

        if(allocated(intensity))    deallocate(intensity)
        allocate(intensity, source=data(2,:))

    end subroutine

    ! intensity data from a just-intensity file .int
    subroutine intensity_from_int(data)
        implicit None
        real(CP), intent(in)                    ::  data(:,:)

        if(allocated(intensity))    deallocate(intensity)
        allocate(intensity, source=data(1,:))

    end subroutine

    ! append column to a 2D array
    subroutine append_column(bi_array, column)
        implicit None
        real(CP), intent(INOUT), allocatable    ::  bi_array(:,:)
        real(CP), intent(IN), allocatable       ::  column(:)

        integer(I4B)            :: j
        real(CP), allocatable   :: temp(:,:)

        ! check they are allocated
        if(.not.allocated(bi_array).or..not.allocated(column)) return

        ! check the sizes are correct
        if(size(column, 1).ne.size(bi_array, 2)) return

        ! copy over to temp
        if(allocated(temp)) deallocate(temp)
        allocate(temp( size(bi_array, 1), size(bi_array, 2) ))
        temp = bi_array

        ! reshape
        deallocate(bi_array)
        allocate(bi_array( size(temp, 1)+1, size(temp, 2)))

        ! copy over
        do j = lbound(temp, 1), ubound(temp, 1)
            bi_array(j,:) = temp(j,:)
        end do
        
        ! append column 
        bi_array(size(temp, 1)+1,:)=column(:)
    end subroutine
    
    ! write new filename as filename_noisy.NNN
    ! BUG: will not work properly with .xy
    subroutine compose_filename(old, new)
        implicit None
        character(len=*), intent(IN)                ::  old
        character(len=:), allocatable, intent(out)  ::  new

        integer(I4B)    ::  namelen

        namelen=len_trim(old)
        if(allocated(new)) deallocate(new)
        allocate(character(len=namelen) :: new)

        new=''
        new=old(:len_trim(old)-4)//"_noisy"//old(len_trim(old)-3:)

    end subroutine

    ! write new filename as filename_justnoise.NNN
    subroutine compose_justnoise_filename(old, new)
        implicit None
        character(len=*), intent(IN)                ::  old
        character(len=:), allocatable, intent(out)  ::  new

        integer(I4B)    ::  namelen

        namelen=len_trim(old)
        if(allocated(new)) deallocate(new)
        allocate(character(len=namelen) :: new)

        new=''
        new=old(:len_trim(old)-4)//"_justnoise"//old(len_trim(old)-3:)

    end subroutine

    ! data normalization
    pure subroutine norm_data(arr, normv)
        implicit None
        real(CP), intent(INOUT)  ::  arr(:), normv

        real(CP)    :: maxint

        maxint  = maxval(arr)
        arr     = arr * normv / maxint

    end subroutine

    ! data modulation to achieve noise modulation
    pure subroutine modulate_data(arr, modu)
        implicit None
        real(CP), intent(INOUT) ::  arr(:)
        real(CP), intent(IN)    ::  modu

        real(CP)        ::  minint

        minint = minval(arr)
        arr    = ( (arr - minint)*modu) + minint

    end subroutine

    ! add external pre-calculated data
    subroutine add_ext_noise(ints, noisy_ints, noise)
        implicit None
        real(CP), intent(INOUT)             ::  ints(:)
        real(CP), intent(IN)                ::  noise(:)
        real(CP), intent(OUT), allocatable  ::  noisy_ints(:)

        integer(I4B)    ::  intlen

        intlen=size(ints, 1)

        if (intlen .gt. size(noise, 1)) then
            write(*,*) "  WARNING: wrong number of values in the provied external noise: ",size(noise, 1)
            call sugg_info()
            return
        endif

        if (allocated(noisy_ints)) deallocate(noisy_ints) 
        allocate(noisy_ints(intlen))
        noisy_ints = ints + noise(:intlen)
    end subroutine

    ! output .cal file
    subroutine out_cal(filename,full_data)
        implicit None
        character(len=*), intent(IN)    ::  filename
        real(CP), intent(IN)            ::  full_data(:,:)

        integer(I4B)    ::  out_unit, iost, j

        out_unit = FIND_UNIT()
        open (  out_unit, status='replace', &
                file=trim(filename), &
                action='write', iostat=iost)
        if (iost .ne. 0) then
            write(*,*) "  ERROR: Impossible to write the output file "//trim(filename)
            close(out_unit)
            return
        end if

        do j = lbound(full_data, 2), ubound(full_data, 2)
            write(out_unit,*) full_data(:,j)
        end do
        close(out_unit)
    
    end subroutine

    ! output single array of data in a column with no leading spaces
    subroutine output_single_array_real(filename,array)
        implicit None
        character(len=*), intent(IN)    ::  filename
        real(CP), intent(IN)            ::  array(:)

        character(512)  ::  str
        integer(I4B)    ::  out_unit, iost, j

        out_unit = FIND_UNIT()
        open (  out_unit, status='replace', &
                file=trim(filename), &
                action='write', iostat=iost)
        if (iost .ne. 0) then
            write(*,*) "  ERROR: Impossible to write the output file "//trim(filename)
            close(out_unit)
            return
        end if
        do j = lbound(array, 1), ubound(array, 1)
            str=''
            write(str,*) array(j)
            write(out_unit,*) trim(adjustl(str))
        end do
        close(out_unit)
    
    end subroutine

    ! suggest for info
    subroutine sugg_info()
        implicit none
        if (.not.help_call) then
            write(*,*) "  Try 'noisy --help' for more information."
            help_call=.true.
        end if
    end subroutine

    ! print pgm info
    subroutine info()
        implicit none

        write(*,*) ""
        write(*,*) "  Addition of synthetic noise to simulations."
        write(*,*) "  Originally made for Debussy files."
        write(*,*) "  HELP Topic"
        write(*,*) ""
        write(*,*) "  Usage:"
        write(*,*) "  noisy filename1.cal filename2.cal ..."
        write(*,*) "  or"
        write(*,*) "  noisy --scale x.xx filename1.cal filename2.cal ..."
        write(*,*) ""
        write(*,*) "  Noise will be added to all the files listed."
        write(*,*) "  Acceptable file formats are: .cal, .xye, .xy, .int"
        write(*,*) "  The scale parameter allows to tune the relative intensity of the added noise."
        write(*,*) "  Scale acts as a multiplicative factor applied on the provided intensities before"
        write(*,*) "  the synthetic noise is added. The output noisy intensities are re-normalized"
        write(*,*) "  on the the same value. The higher the scale, the lower the noise level."
        write(*,*) "  Output will be provided in new files named as 'filename_noisy.cal.'"
        write(*,*) "  NOTE: decimal digits in the input intesities will be discarded, as the values will"
        write(*,*) "  be converted to the nearest integers before applying the synthetic noise."
        write(*,*) ""
        write(*,*) ""
        write(*,*) "  Special flags:"
        write(*,*) "    --help"
        write(*,*) "      Display this help topic."
        write(*,*) ""
        write(*,*) "    --add x.xx"
        write(*,*) "      Add a constant value x.xx to the intensities before applying the noise."
        write(*,*) ""
        write(*,*) "    --const x.xx"
        write(*,*) "      Use a constant noise modulated around x.xx."
        write(*,*) ""
        write(*,*) "    --modulate x.xx"
        write(*,*) "      Modulate the noise proportionality to variations of the intensity"
        write(*,*) ""
        write(*,*) "    --norm x.xx"
        write(*,*) "      Normalize max intensity to x.xx."
        write(*,*) ""
        write(*,*) "    --scale x.xx"
        write(*,*) "      Specify multiplicative factor to tune the noise intensity level."
        write(*,*) ""
        write(*,*) "    --silent"
        write(*,*) "      Make noisy less verbose."
        write(*,*) ""
        write(*,*) "    --source filename"
        write(*,*) "      Use the intensity values from another file for the calculation of noise."
        write(*,*) ""
        write(*,*) "    --ext filename"
        write(*,*) "      Use noise values from another file (1st column)."
        write(*,*) ""
        write(*,*) "    --int-only"
        write(*,*) "      Output only the simulated intensity column."
        write(*,*) ""
        write(*,*) "    --noise-include"
        write(*,*) "      Add an extra column in the output file with just noise values."
        write(*,*) ""
        write(*,*) "    --noise-out"
        write(*,*) "      Output just the noise values in filename_justnoise.cal."
        write(*,*) ""
        write(*,*) "    Special flags can be in any position of arguments list."
        write(*,*) ""
        write(*,*) ""
        write(*,*) "  Examples:"
        write(*,*) ""
        write(*,*) "  noisy simulation.cal"
        write(*,*) "  Adds synthetic noise to 'simulation.cal' in the file 'simulation_noisy.cal'"
        write(*,*) ""
        write(*,*) "  noisy --scale 15.6 file_1.cal file_1.cal"
        write(*,*) "  Adds synthetic noise to 'file_1.cal' and 'file_2.cal' in the files 'file_1_noisy.cal'"
        write(*,*) "  and 'file_2_noisy.cal'. A scale value of 15.6 will be used." 
        write(*,*) ""

    end subroutine


end program
