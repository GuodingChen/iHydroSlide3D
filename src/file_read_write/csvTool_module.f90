
module csv_ReadWrite_utils

    use csv_module
    
    implicit none
    
    private 
    public :: csvRead
contains
    subroutine csvRead(csvFilePath, N_HeaderRow, NColumnInCSV, TargetCol_Data)
        
        ! N_HeaderRow: Header line to skip, [1,2,3,...]
        ! NColumnInCSV: Number of columns in the table, [1,2,3,...]
        ! Array element index starts from 1 
        implicit none

        character(len=*), intent(in)  :: csvFilePath
        integer, intent(in) :: N_HeaderRow, NColumnInCSV
        type(csv_file) :: f
        character(len=30), allocatable :: header(:)
        double precision, intent(out), allocatable :: TargetCol_Data(:)
        
        logical :: status_ok
        integer,dimension(:),allocatable :: itypes
        
        ! read the file
        call f%read(csvFilePath, header_row = N_HeaderRow, status_ok=status_ok)
        
        ! get the header and type info
        call f%get_header(header,status_ok)
        call f%variable_types(itypes,status_ok)
        
        ! get date from csv file based on its Column number 
        call f%get(NColumnInCSV, TargetCol_Data, status_ok)

        call f%destroy()



    end subroutine csvRead


end module csv_ReadWrite_utils


