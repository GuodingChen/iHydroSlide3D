
!	Main Program of CREST
program CREST_Main

    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use Landslide_Basic
    use LandslideModel_parameters
    use OMP_LIB
    use TimeProcess
    

    implicit none

    
    logical :: bIsError, ControlFile_Exist
    integer :: LenPrjFileName
    
    integer(kind=8) :: StartTick, EndTick, TickRate
    
    
    
    ! 获取系统的频率(TickRate)以及初始时间(StartTick)
    call system_clock(count_rate=TickRate)
    call system_clock(count=StartTick)

    
    ! Get the number of command-line arguments
    CommandLine_NumArgs = command_argument_count()

    ! Check if the project file name is provided
    if (CommandLine_NumArgs < 1) then
        print *, 'Please specify the project control file-->Usage: ./iHydroSlide3D <inputfile.prj>'
        stop 1
    end if
    
    ! Read the first command-line argument
    call get_command_argument(1, PrjFileName)
    
    g_PrjNP = trim(PrjFileName)
    
    inquire(file = g_PrjNP, exist=ControlFile_Exist)

    if (.not. ControlFile_Exist) then
        print *, 'Error: project control file does not exist -', g_PrjNP
        stop 1
    end if

    
    
    ! Trim and check the extension
    LenPrjFileName = len_trim(PrjFileName)

    if (LenPrjFileName < 4 .or. trim(adjustl(PrjFileName(LenPrjFileName-3:LenPrjFileName))) /= '.prj') then
        print *, 'Error: Input file must have a .prj extension.'
        stop 1
    end if

    write(*,'(A)') '------------------------------------------------------------'
    write(*,'(A)') '                   << iHydroSlide3D >>'
    write(*,'(A)') '  A Integrated Hydrological & 3D Slope Stability Framework'
    write(*,'(A)') '                   Version V1.1 (2025)'
    write(*,*)

    
    !Write to the log file
    
    call XXWGetFreeFile(g_CREST_LogFileID)

    open(g_CREST_LogFileID,file &
            = "logs//"//"iHydroSlide3D-"//trim(g_strD)//".log",form='formatted')

    write(g_CREST_LogFileID,"(25X,A)") "iHydroSlide3D"
    write(g_CREST_LogFileID,"(1X,A)") "A integrated hydrological processes and &
            & 3D slope stability modeling framework"
    write(g_CREST_LogFileID,"(25X,'Version ',A)") g_CREST_Version

    

    call CREST_Main_Pre(bIsError)
    


    if(bIsError .eqv. .true.)then
        stop
    end if


    
    write(*,'(A)') '-------------------- Parallel Setup -------------------------'
    if (g_ModelCore == 1) then ! only for hydrological modeling
        write(*,'(A,I5)') 'Subbasin number:        ', N_Subbasin
        write(*,'(A,I5)') 'Hydrology threads:      ', Nthread_hydro

    elseif (g_ModelCore == 3) then ! for both hydrology and landslide
        write(*,'(A,I5)') 'Subbasin number:        ', N_Subbasin
        write(*,'(A,I5)') 'Hydrology threads:      ', Nthread_hydro
        write(*,*)
        write(*,"(2X, A, g0)") "Total tile number for landslide: ", total_tile_number
        write(*,"(2X, A, g0)") "Thread for landslide: ", Nthread_Land
        write(*,"(2X, A, g0)") "ellipse density: ", ellipse_density

    end if
    ! save to the log file
    write(g_CREST_LogFileID,"(2X,A)") "--------All necessary data for the model has been read---------"
    write(g_CREST_LogFileID,*)
    write(g_CREST_LogFileID,"(2X,A)") "------parallel setup----------"
    if (g_ModelCore == 1) then ! only for hydrological modeling
        write(g_CREST_LogFileID,*) "Subbasin number: ", N_Subbasin
        write(g_CREST_LogFileID,*) "Thread for hydrology: ", Nthread_hydro

    elseif (g_ModelCore == 3) then ! for both hydrology and landslide
        write(g_CREST_LogFileID,"(2X, A, g0)") "Subbasin number: ", N_Subbasin
        write(g_CREST_LogFileID,"(2X, A, g0)") "Thread for hydrology: ", Nthread_hydro
        write(g_CREST_LogFileID,"(2X, A, g0)") "Total tile numbber for landslide: ", total_tile_number
        write(g_CREST_LogFileID,"(2X, A, g0)") "Thread for landslide: ", Nthread_Land
        write(g_CREST_LogFileID,"(2X, A, g0)") "ellipse density: ", ellipse_density

    end if

    ! the parallel preprocessing is prepared based on g_ModelCore
    select case(g_ModelCore)
        case(1) ! only simulate the hydrological processes
            call Parallel_hydro_pre()
        case(3) ! coupled hydrological-stability simulation
            call Parallel_hydro_pre()
            call Parallel_land_pre()
        case default ! default only for hydrology model
            call Parallel_hydro_pre()
    end select

    if(bIsError .eqv. .true.)then
        stop
    end if
    
    write(*,*)
    write(g_CREST_LogFileID,*)

    write(*,'(A)') '---------------------- Simulation info -------------------'
    
    select case (trim(g_sRunStyle))
    case ("SIMU")
        write(*,'(A,A)') 'Run Style:              ' // trim(g_sRunStyle)
        ! Output the simulation time step
        select case (g_TimeMark)
            case ('d')
                write(*,'(A,I0,A)') 'Time step:              ', g_TimeStep, ' day(s)'
            case ('h')
                write(*,'(A,I0,A)') 'Time step:              ', g_TimeStep, ' hour(s)'
            case ('u')
                write(*,'(A,I0,A)') 'Time step:              ', g_TimeStep, ' minute(s)'
            case ('s')
                write(*,'(A,I0,A)') 'Time step:              ', g_TimeStep, ' second(s)'
        end select        

        write(*,'(A,A)') 'Simulation start time:  '//  &
                GetPrintDatetime(SimuDateTimeList(1))
        write(*,'(A,A)') 'Simulation end time:    ' //  &
                GetPrintDatetime(SimuDateTimeList(size(SimuDateTimeList)))

        if ( g_LoadState .eqv. .true. ) then 
            write(*,'(A,A)') 'Load state:             ', GetPrintDatetime(WarmupDateTime_simu)
        else
            write(*,'(A,A)') 'Load state:             ', 'Initial condition'
        end if         
       
    end select

    

    !  ----------------  run CREST core ---------------------
    print *           ! add line
    write(*,'(A)') '---------------------- Simulation Progress -------------------'
    call CREST_Simu()

    write(*,*)
    write(g_CREST_LogFileID,*)

    ! ========================= end simu ===================
    call system_clock(count=EndTick)

    write(*,'(A)') '----------------------- Summary ------------------------------'
    
    call PrintModelRunTime(StartTick, EndTick, TickRate, "Model Total Run Time: ")
    
    write(*,*)

    write(*,'(A)') '>> Project: "' // trim(g_PrjNP) // '" is finished.'
    write(*,'(A)') '=============================================================='
    

    close(g_CREST_LogFileID)


    stop
end program CREST_Main




