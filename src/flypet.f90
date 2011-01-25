PROGRAM flypet
  IMPLICIT NONE
  CHARACTER(LEN=128) :: input_filename, output_filename
  INTEGER, PARAMETER :: input_fileid = 11, output_fileid = 12
  INTEGER :: num_block_transform, stat, num_data
  INTEGER :: num_data_per_block, i, j, idx
  LOGICAL :: is_num_block_transform_assigned, is_input_filename_assigned
  LOGICAL :: is_output_filename_assigned, is_eof
  REAL(KIND=8) :: average, sdev, block1, block2
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: data
  CHARACTER, PARAMETER :: COMMENT_CHAR = '#'
  INTEGER, EXTERNAL :: count_num_data

  is_input_filename_assigned = .FALSE.
  is_output_filename_assigned = .FALSE.
  
  call get_argument()

  open(UNIT=input_fileid, FILE=input_filename, STATUS='OLD', IOSTAT=stat, ACTION='READ')
  if (stat /= 0) then
     write(*,*) "Unable to open input file: ", TRIM(ADJUSTL(input_filename))
     call EXIT(1)
  end if

  if (is_output_filename_assigned) then
     open(UNIT=output_fileid, FILE=output_filename, IOSTAT=stat, ACTION='WRITE')
     if (stat /= 0) then
        write(*,*) "Unable to open output file: ", TRIM(ADJUSTL(output_filename))
        call EXIT(1)
     end if
  end if

  num_data = count_num_data(input_fileid, COMMENT_CHAR)

  ALLOCATE(data(num_data), STAT=stat)
  if (stat /=0) then
     write(*,*) "Allocation error: data"
     call EXIT(1)
  end if

  write(*,*) "Input file: ", TRIM(ADJUSTL(input_filename))
  write(*,*) "Number of data: ", num_data

  !read all data
  do i = 1, num_data
     call read_real_datum(data(i), EOF=is_eof)
  end do
  
  average = SUM(data)/SIZE(data)
  write(*,*) "Average = ", average
  
  if (is_output_filename_assigned) then
     write(output_fileid,*) "# Input file: ", TRIM(ADJUSTL(input_filename))
     write(output_fileid,*) "# Number of data: ", num_data
     write(output_fileid,*) "# Average = ", average
  end if

  num_block_transform = -1
  do while(num_data > 1)
     num_block_transform = num_block_transform + 1
     sdev = 0.0d0
     do i = 1, num_data
        sdev = sdev + (data(i) - average)*(data(i) - average)
        !blocking data for next loop
        idx = i/2 + MOD(i,2)
        if (MOD(i,2)==1) then
           data(idx) = data(i)/2.0d0
        else
           data(idx) = data(idx) + data(i)/2.0d0
        end if
     end do
     sdev = (sdev / num_data) / (num_data - 1)
     sdev = sqrt(sdev)
     call output(num_block_transform, sdev)
     num_data = num_data / 2  !ignore the last datum if total number is odd
  end do

  
CONTAINS
  SUBROUTINE get_argument()
    IMPLICIT NONE
    INTEGER :: stat, i, n
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 2
    CHARACTER(LEN=128) :: usage, arg

    n = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
    usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -f <in file> [-o <out file>]"

    if (n < LEAST_REQUIRED_NUM_ARG) then
       write(*,*) "Insufficient arguments!"
       write(*,*) usage
       call EXIT(1)
    end if

    i = 1
    do while (i <= n)
       call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
       i = i + 1
       select case (arg)
       case ('-f')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=input_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -f"
             write(*,*) usage
             call EXIT(1)
          end if
          is_input_filename_assigned = .TRUE.

       case ('-o')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=output_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -o"
             write(*,*) usage
             call EXIT(1)
          end if
          is_output_filename_assigned = .TRUE.
          
!        case ('-n')
!           call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
!           i = i + 1
!           if (stat /= 0) then
!              write(*,*) "Unable to read the value of argument -n"
!              write(*,*) usage
!              call EXIT(1)
!           end if
!           read(arg, *, IOSTAT=stat) num_block_transform
!           if (stat /= 0) then
!              write(*,*) "Unable to parse the value of argument -n, an&
!                   & integer is needed!"
!              write(*,*) usage
!              call EXIT(1)
!           end if
!           is_num_block_transform_assigned = .TRUE.
          
       case default
          write(*,*) "Unknown argument: ", arg
          call EXIT(1)
       end select
    end do

    if (.NOT. is_input_filename_assigned) then
       write(*,*) "<in file> (input filename) should be provided."
       write(*,*) usage
       call EXIT(1)
    end if
  END SUBROUTINE get_argument

  !First see num_data = 2**X + r , then num_block_transform = X-1
!   SUBROUTINE cal_num_block_transform(num_bt)
!     IMPLICIT NONE
!     INTEGER :: temp, num_bt
    
!     num_bt = INT( LOG(DBLE(num_data)) / LOG(DBLE(2)) )
!     temp = num_data / 2**num_bt
!     if (temp == 1) then !X = num_block_transform
!        num_bt = num_bt - 1
!     else if (temp == 2) then !X is underestimated due to rounding error
!        !num_block_transform = num_block_transform
!        return
!     else
!        write(*,*) "Something is wrong when calulating <num_block_transform>!"
!        write(*,*) "num_block_transform = INT( LOG(DBLE(num_data)) / LOG(2) ) = ", num_bt
!        write(*,*) "num_data / 2**num_block_transform = ", temp
!        call EXIT(1)
!     end if
!   END SUBROUTINE cal_num_block_transform

  SUBROUTINE read_real_datum(datum, eof)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(OUT) :: datum
    LOGICAL, INTENT(OUT) :: eof
    INTEGER :: stat
    CHARACTER(LEN=128) :: line

    eof = .FALSE.
    
    do while(.TRUE.)
       read(input_fileid, "(A)", IOSTAT=stat) line
       if (stat > 0) then
          write(*,*) "Error occurred while reading data line!"
          call EXIT(1)
       else if (stat < 0) then !End of file
          eof = .TRUE.
          write(*,*) "End of line occurred while reading data line!"
          call EXIT(1)
       end if

       if (TRIM(ADJUSTL(line)) /= '' .AND. &
            &INDEX(ADJUSTL(line), comment_char) /= 1) then          
          read(line, *, IOSTAT=stat) datum
          if (stat /= 0) then
             write(*,*) "Error occurred while parsing data line!"
             call EXIT(1)
          end if
          RETURN
       end if
    end do
  END SUBROUTINE read_real_datum

  SUBROUTINE output(num_bt, sd)
    IMPLICIT NONE
    INTEGER :: num_bt
    REAL(KIND=8) :: sd
    write(*,*) num_bt, sd
    if (is_output_filename_assigned) then
       write(output_fileid, *) num_bt, sd
    end if
  END SUBROUTINE output
END PROGRAM flypet

! SUBROUTINE read_real_datum(input_fileid, comment_char)
!   IMPLICIT NONE
!   INTEGER, INTENT(IN) :: input_fileid
!   CHARACTER, INTENT(IN) :: comment_char
!   REAL(KIND=8) :: read_real_datum
!   INTEGER :: stat
!   CHARACTER(LEN=128) :: line

!   do while(.TRUE.)
!      read(input_fileid, "(A)", IOSTAT=stat) line
!      if (stat /= 0) then
!         write(*,*) "Error occurred while reading data line!"
!         call EXIT(1)
!      end if
!      if (line(1:1) == comment_char) then
!         CYCLE
!      end if
!      read(line, *, IOSTAT=stat) read_real_datum
!      if (stat /= 0) then
!         write(*,*) "Error occurred while parsing data line!"
!         call EXIT(1)
!      end if     
!   end do
! END SUBROUTINE read_real_datum

FUNCTION count_num_data(input_fileid, comment_char)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: input_fileid
  CHARACTER, INTENT(IN) :: comment_char
  INTEGER :: count_num_data
  INTEGER :: line_num, stat
  CHARACTER(LEN=128) :: line

  line_num = 1
  count_num_data = 0
  
  do while (.TRUE.)
     read(input_fileid, "(A)", IOSTAT=stat) line
     if (stat > 0) then
        write(*,*) "Error occurred while reading line #", line_num
        call EXIT(1)
     else if (stat < 0) then !End of file
        exit
     else
        line_num = line_num + 1
        if (TRIM(ADJUSTL(line)) /= '' .AND. &
             &INDEX(ADJUSTL(line), comment_char) /= 1) then
           count_num_data = count_num_data + 1
        end if
     end if
  end do
  REWIND(input_fileid)
END FUNCTION count_num_data
