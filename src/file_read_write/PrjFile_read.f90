! ==============================================================================
! Module: parameter_reader_module
! Purpose: Reads parameters from a specified control file.
! Features:
!   - Reads integer, real, character, and logical parameters by name.
!   - Ignores comments starting with '#'.
!   - Handles basic error checking (file not found, parameter not found,
!     type conversion errors).
!   - Case-insensitive parameter name matching.
!   - Logical parameters restricted to YES/NO (case-insensitive).
! ==============================================================================
MODULE PrjFile_reader
    IMPLICIT NONE
    PRIVATE ! Default all to private
    PUBLIC :: read_integer_param, read_real_param, read_char_param, read_logical_param

    CONTAINS

    ! --------------------------------------------------------------------------
    ! Helper function: to_upper
    ! Converts a string to uppercase for case-insensitive comparison.
    ! --------------------------------------------------------------------------
    FUNCTION to_upper(str_in) RESULT(str_out)
        CHARACTER(LEN=*), INTENT(IN) :: str_in
        CHARACTER(LEN=LEN(str_in)) :: str_out
        INTEGER :: i
        str_out = str_in
        DO i = 1, LEN(str_in)
            SELECT CASE (str_out(i:i))
                CASE ('a':'z')
                    str_out(i:i) = ACHAR(IACHAR(str_out(i:i)) - IACHAR('a') + IACHAR('A'))
            END SELECT
        END DO
    END FUNCTION to_upper

      ! --------------------------------------------------------------------------
   ! Core Subroutine: get_param_string (MODIFIED)
   ! Reads the file line by line, finds the requested parameter,
   ! and returns its value as a string. Handles TABS.
   ! --------------------------------------------------------------------------
   SUBROUTINE get_param_string(parameter_name, filename, value_str, found_status)
      CHARACTER(LEN=*), INTENT(IN)  :: parameter_name
      CHARACTER(LEN=*), INTENT(IN)  :: filename
      CHARACTER(LEN=256), INTENT(OUT) :: value_str ! Output as a string
      LOGICAL, INTENT(OUT)         :: found_status  ! TRUE if parameter found

      CHARACTER(LEN=512) :: line       ! Buffer for reading lines
      CHARACTER(LEN=100) :: key        ! Parameter name from file
      CHARACTER(LEN=256) :: val        ! Parameter value from file
      INTEGER            :: file_unit = 10 ! File unit number
      INTEGER            :: io_stat    ! I/O status
      INTEGER            :: comment_pos, eq_pos ! Position markers
      INTEGER            :: i, line_len ! Loop counter and line length

      CHARACTER(LEN=LEN(parameter_name)) :: search_name_upper
      CHARACTER(LEN=100)                 :: key_upper

      found_status = .FALSE.
      value_str = ""
      search_name_upper = to_upper(TRIM(parameter_name))

      ! Open the file
      OPEN(UNIT=file_unit, FILE=TRIM(filename), STATUS='OLD', ACTION='READ', IOSTAT=io_stat)

      IF (io_stat /= 0) THEN
         WRITE(*, '(A, A, I0)') 'Error: Could not open file: ', TRIM(filename), ', IOSTAT=', io_stat
         RETURN
      END IF

      ! Read line by line
      DO
         READ(file_unit, '(A)', IOSTAT=io_stat) line
         IF (io_stat /= 0) EXIT ! Exit on End-Of-File or error

         ! *** NEW: Replace TABS (ASCII 9) with spaces (ASCII 32) ***
         line_len = LEN_TRIM(line)
         DO i = 1, line_len
               IF (IACHAR(line(i:i)) == 9) line(i:i) = ' '
         END DO

         ! 1. Handle comments
         comment_pos = INDEX(line, '#')
         IF (comment_pos > 0) THEN
               line = line(:comment_pos-1)
         END IF

         ! 2. Trim and skip blank lines
         line = TRIM(ADJUSTL(line))
         IF (LEN_TRIM(line) == 0) CYCLE

         ! 3. Find the '=' sign
         eq_pos = INDEX(line, '=')
         IF (eq_pos > 0) THEN
               ! *** MODIFIED: Use TRIM on both sides ***
               key = TRIM(line(:eq_pos-1))          ! Trim spaces/tabs before '='
               val = TRIM(ADJUSTL(line(eq_pos+1:))) ! Trim spaces/tabs after '='

               ! 4. Compare (case-insensitive)
               key_upper = to_upper(TRIM(key))

               ! DEBUG (Optional: Uncomment to see what is being compared)
               ! WRITE(*, '("Comparing: [", A, "] with [", A, "]")') &
               !      TRIM(key_upper), TRIM(search_name_upper)

               IF (TRIM(key_upper) == TRIM(search_name_upper)) THEN
                  value_str = TRIM(val)
                  found_status = .TRUE.
                  EXIT ! Found it, exit the loop
               END IF
         END IF
      END DO

      ! Close the file
      CLOSE(file_unit)

      ! Report if not found (Only if it wasn't found)
      IF (.NOT. found_status) THEN
         WRITE(*, '(A, A, A)') 'Warning: Parameter "', & 
                TRIM(parameter_name), '" not found in file.'
      END IF

   END SUBROUTINE get_param_string

    ! --------------------------------------------------------------------------
    ! Function: read_integer_param
    ! Reads an integer parameter from the file.
    ! --------------------------------------------------------------------------
    FUNCTION read_integer_param(parameter_name, filename) RESULT(result_value)
        CHARACTER(LEN=*), INTENT(IN) :: parameter_name, filename
        INTEGER                      :: result_value
        CHARACTER(LEN=256)           :: value_str
        LOGICAL                      :: found
        INTEGER                      :: io_stat

        CALL get_param_string(parameter_name, filename, value_str, found)

        IF (found) THEN
            READ(value_str, *, IOSTAT=io_stat) result_value
            IF (io_stat /= 0) THEN
                WRITE(*, '(A, A, A, A)') 'Error: Could not convert value "', &
                                         TRIM(value_str), '" to INTEGER for parameter "', &
                                         TRIM(parameter_name), '".'
                STOP 'Parameter conversion error.'
            END IF
        ELSE
            WRITE(*, '(A, A)') 'Error: Required parameter "', TRIM(parameter_name), '" not found.'
            STOP 'Missing parameter error.'
        END IF

    END FUNCTION read_integer_param

    ! --------------------------------------------------------------------------
    ! Function: read_real_param
    ! Reads a real (floating-point) parameter from the file.
    ! --------------------------------------------------------------------------
    FUNCTION read_real_param(parameter_name, filename) RESULT(result_value)
        CHARACTER(LEN=*), INTENT(IN) :: parameter_name, filename
        REAL                         :: result_value
        CHARACTER(LEN=256)           :: value_str
        LOGICAL                      :: found
        INTEGER                      :: io_stat

        CALL get_param_string(parameter_name, filename, value_str, found)

        IF (found) THEN
            READ(value_str, *, IOSTAT=io_stat) result_value
            IF (io_stat /= 0) THEN
                WRITE(*, '(A, A, A, A)') 'Error: Could not convert value "', &
                                         TRIM(value_str), '" to REAL for parameter "', &
                                         TRIM(parameter_name), '".'
                STOP 'Parameter conversion error.'
            END IF
        ELSE
            WRITE(*, '(A, A)') 'Error: Required parameter "', TRIM(parameter_name), '" not found.'
            STOP 'Missing parameter error.'
        END IF

    END FUNCTION read_real_param

    ! --------------------------------------------------------------------------
    ! Subroutine: read_char_param
    ! Reads a character parameter from the file.
    ! --------------------------------------------------------------------------
    SUBROUTINE read_char_param(parameter_name, filename, result_value)
        CHARACTER(LEN=*), INTENT(IN)  :: parameter_name, filename
        CHARACTER(LEN=*), INTENT(OUT) :: result_value
        CHARACTER(LEN=256)            :: value_str
        LOGICAL                       :: found

        CALL get_param_string(parameter_name, filename, value_str, found)

        IF (found) THEN
            result_value = value_str ! Assign trimmed value
        ELSE
            WRITE(*, '(A, A)') 'Error: Required parameter "', TRIM(parameter_name), '" not found.'
            STOP 'Missing parameter error.'
        END IF

    END SUBROUTINE read_char_param

    ! --------------------------------------------------------------------------
    ! Function: read_logical_param (MODIFIED)
    ! Reads a logical parameter from the file. Handles only yes/no (case-insensitive).
    ! --------------------------------------------------------------------------
    FUNCTION read_logical_param(parameter_name, filename) RESULT(result_value)
        CHARACTER(LEN=*), INTENT(IN) :: parameter_name, filename
        LOGICAL                      :: result_value
        CHARACTER(LEN=256)           :: value_str
        CHARACTER(LEN=10)            :: check_str
        LOGICAL                      :: found

        CALL get_param_string(parameter_name, filename, value_str, found)

        IF (found) THEN
            check_str = to_upper(TRIM(value_str)) ! Convert to upper case
            SELECT CASE (TRIM(check_str))
                CASE ('YES') ! Only check for YES
                    result_value = .TRUE.
                CASE ('NO')  ! Only check for NO
                    result_value = .FALSE.
                CASE DEFAULT ! Any other value is an error
                    WRITE(*, '(A, A, A, A)') 'Error: Value "', TRIM(value_str), &
                                             '" is not valid for logical parameter "', &
                                             TRIM(parameter_name), '". Use only YES or NO.'
                    STOP 'Parameter conversion error.'
            END SELECT
        ELSE
            WRITE(*, '(A, A)') 'Error: Required parameter "', TRIM(parameter_name), '" not found.'
            STOP 'Missing parameter error.'
        END IF

    END FUNCTION read_logical_param

END MODULE PrjFile_reader


