PROGRAM flypet
  IMPLICIT NONE
  CHARACTER(LEN=128) :: input_filename, output_filename
  INTEGER, PARAMETER :: input_fileid = 11, output_fileid = 12
  INTEGER :: num_block_transorm, stat, num_data
  LOGICAL :: is_num_block_transform_assigned, is_input_filename_assigned
  CHARACTER, PARAMETER :: COMMENT_CHAR = '#'
  INTEGER, EXTERNAL :: count_num_data

  is_input_filename_assigned = .FALSE.
  is_num_block_transform_assigned = .FALSE.
  
  call get_argument()

  open(UNIT=input_fileid, FILE=input_filename, STATUS='OLD', IOSTAT=stat, ACTION='READ')
  if (stat /= 0) then
     write(*,*) "Unable to open input file: ", TRIM(ADJUSTL(input_filename))
     call EXIT(1)
  end if

  num_data = count_num_data(input_fileid, COMMENT_CHAR)

  if (.NOT. is_num_block_transform_assigned) then
     call cal_num_block_transfrom()
  end if
  
  if (is_num_block_transform_assigned) then
     if (num_data < 2**(num_block_transorm+1) ) then
        write(*,*) "<num block transform> is too large: ", num_block_transorm
        call cal_num_block_transfrom()
        write(*,*) "For current data number: ", num_data
        write(*,*) "<num block transfom> should be less than: ", num_block_transorm
        call EXIT(1)
     end if
  else
     call cal_num_block_transfrom()     
  end if
    
  write(*,*) "Input file: ", TRIM(ADJUSTL(input_filename))
  write(*,*) "Number of data: ", num_data
  write(*,*) "Number of block transformation: ", num_block_transorm

CONTAINS
  SUBROUTINE get_argument()
    IMPLICIT NONE
    INTEGER :: stat, i, n
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 2
    CHARACTER(LEN=128) :: usage, arg

    n = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
    usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -f <in file> [-o <out file> -n <num block transform>]"

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
          
       case ('-n')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -n"
             write(*,*) usage
             call EXIT(1)
          end if
          read(arg, *, IOSTAT=stat) num_block_transorm
          if (stat /= 0) then
             write(*,*) "Unable to parse the value of argument -n, an&
                  & integer is needed!"
             write(*,*) usage
             call EXIT(1)
          end if
          is_num_block_transform_assigned = .TRUE.
          
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

  !First see num_data = 2**X + r , then num_block_transorm = X-1
  SUBROUTINE cal_num_block_transfrom()
    IMPLICIT NONE
    INTEGER :: temp
    num_block_transorm = INT( LOG(DBLE(num_data)) / LOG(DBLE(2)) )
    temp = num_data / 2**num_block_transorm
    if (temp == 1) then !X = num_block_transorm
       num_block_transorm = num_block_transorm - 1
    else if (temp == 2) then !X is underestimated due to rounding error
       !num_block_transorm = num_block_transorm
       return
    else
       write(*,*) "Something is wrong when calulating <num_block_transform>!"
       write(*,*) "num_block_transorm = INT( LOG(DBLE(num_data)) / LOG(2) ) = ", num_block_transorm
       write(*,*) "num_data / 2**num_block_transorm = ", temp
       call EXIT(1)
    end if
  END SUBROUTINE cal_num_block_transfrom

END PROGRAM flypet

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
        if (line(1:1) /= comment_char) then
           count_num_data = count_num_data + 1
        end if
     end if
  end do
END FUNCTION count_num_data
