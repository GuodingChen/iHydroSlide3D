subroutine CREST_Simu()
    use CREST_Project
    use CREST_Basic
    use CREST_Param
    use CREST_Calib_SCEUA
    use OMP_LIB
    use SoilDownscale_Basic
    use Landslide_Basic
    use LandslideModel_parameters
    use csv_ReadWrite_utils
    use TimeProcess
    use linearRoute
    use iso_fortran_env, only: real64
    
    implicit none
    
    integer :: NsimuStepTot
    integer :: i,j,k, i_basin, HDF5_WriteCount
    integer, parameter :: dp = real64
    real(dp), parameter :: EPS_STORAGE = 1.0d-12
    double precision :: HydroTime_start, P_monitor
    double precision :: riverWater_routing
    
    logical :: bIsError, FileExist
    character(len=200):: RainSeriesFileName
    character(len=200):: PetSeriesFileName

    character(len = 19) :: OutputDatetime
    character(len = 3):: i_basin_str
    character(len = 19) :: WarmupFileDate
    double precision, allocatable :: RainSeries(:), PETSeries(:)
    integer :: CurrentDayInYear

    ! basical variable for hydrological module
    double precision, allocatable :: Rain(:,:),PET(:,:),EPot(:,:)
    double precision, allocatable :: W0(:,:),SS0(:,:),SI0(:,:), sRiver0(:,:)
    double precision, allocatable :: g_WU0(:,:), g_SS0(:,:), g_SI0(:,:), g_sRiver0(:,:)
    double precision, allocatable :: SS0_track(:,:), SI0_track(:,:), sRiver0_track(:,:)
    double precision, allocatable :: W(:,:),ExcS(:,:),ExcI(:,:), g_SM(:,:)
    double precision, allocatable :: RI(:,:),RS(:,:)
    double precision, allocatable :: EAct(:,:), Runoff(:,:), runoffDischarge(:,:)
    double precision, allocatable :: passRS(:,:), passRI(:,:), passRiver(:,:)

    integer,allocatable :: g_SubMask(:,:)

    ! Computation progress monitoring
    integer :: bar_length, filled_length
    character(len=30) :: bar
    character(len=10) :: P_monitor_str

    ! Color System
    character(len=*), parameter :: reset = CHAR(27)//"[0m"
    character(len=*), parameter :: green = CHAR(27)//"[32m"
    character(len=*), parameter :: yellow = CHAR(27)//"[33m"
    character(len=*), parameter :: cyan = CHAR(27)//"[36m"


    !---------------------------------------------------
    allocate(Rain(0:g_NCols-1,0:g_NRows-1))
    allocate(PET(0:g_NCols-1,0:g_NRows-1))
    allocate(EPot(0:g_NCols-1,0:g_NRows-1))

    allocate(W0(0:g_NCols-1,0:g_NRows-1))
    allocate(SS0(0:g_NCols-1,0:g_NRows-1))
    allocate(SI0(0:g_NCols-1,0:g_NRows-1))
    allocate(sRiver0(0:g_NCols-1,0:g_NRows-1))
    
    allocate(g_WU0(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SS0(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SI0(0:g_NCols-1,0:g_NRows-1))
    allocate(g_sRiver0(0:g_NCols-1,0:g_NRows-1))
    
    allocate(W(0:g_NCols-1,0:g_NRows-1))
    allocate(g_SM(0:g_NCols-1,0:g_NRows-1))
    allocate(ExcS(0:g_NCols-1,0:g_NRows-1))
    allocate(ExcI(0:g_NCols-1,0:g_NRows-1))
    allocate(EAct(0:g_NCols-1,0:g_NRows-1))
    allocate(Runoff(0:g_NCols-1,0:g_NRows-1))
    allocate(runoffDischarge(0:g_NCols-1,0:g_NRows-1))

    allocate(RS(0:g_NCols-1,0:g_NRows-1))
    allocate(RI(0:g_NCols-1,0:g_NRows-1))
    allocate(passRS(0:g_NCols-1,0:g_NRows-1))
    allocate(passRI(0:g_NCols-1,0:g_NRows-1))
    allocate(passRiver(0:g_NCols-1,0:g_NRows-1))
    allocate(SS0_track(0:g_NCols-1,0:g_NRows-1))
    allocate(SI0_track(0:g_NCols-1,0:g_NRows-1))
    allocate(sRiver0_track(0:g_NCols-1,0:g_NRows-1))

    allocate(g_SubMask(0:g_NCols-1,0:g_NRows-1))

    Rain = g_NoData_Value
    PET = g_NoData_Value
    EPot = g_NoData_Value 
    EAct =g_NoData_Value
    W = g_NoData_Value
    g_SM = g_NoData_Value
    Runoff=g_NoData_Value
    runoffDischarge = g_NoData_Value

    ExcS=g_NoData_Value
    ExcI=g_NoData_Value
    RS=g_NoData_Value
    RI=g_NoData_Value

    W0 = g_NoData_Value
    SS0 = g_NoData_Value
    SI0 = g_NoData_Value
    sRiver0 = g_NoData_Value

    g_WU0 = g_NoData_Value
    g_SS0 = g_NoData_Value
    g_SI0 = g_NoData_Value
    g_sRiver0 = g_NoData_Value


    bar_length = 30
    
    ! initial the HDF5_write count

    HDF5_WriteCount = 0

    ! initial the time tic
    HydroRunTime = 0
    LandRunTime = 0


    NsimuStepTot = size(SimuDateTimeList)
    
    
    
    if(g_RunStyle/=2)then
        if(g_tOutlet%HasOutlet .eqv. .true.)then
            allocate(g_tOutlet%Rain(1 : NsimuStepTot))
            allocate(g_tOutlet%PET(1 : NsimuStepTot))
            allocate(g_tOutlet%EPot(1 : NsimuStepTot))
            allocate(g_tOutlet%EAct(1 : NsimuStepTot))
            allocate(g_tOutlet%W(1 : NsimuStepTot))
            allocate(g_tOutlet%SM(1 : NsimuStepTot))
            allocate(g_tOutlet%R(1 : NsimuStepTot))
            allocate(g_tOutlet%ExcS(1 : NsimuStepTot))
            allocate(g_tOutlet%ExcI(1 : NsimuStepTot))
            allocate(g_tOutlet%RS(1 : NsimuStepTot))
            allocate(g_tOutlet%RI(1 : NsimuStepTot))
        end if
        
        do k = 1, g_NOutPixs
            allocate(g_tOutPix(k)%Rain(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%PET(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%EPot(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%EAct(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%W(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%SM(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%R(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%ExcS(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%ExcI(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%RS(1 : NsimuStepTot))
            allocate(g_tOutPix(k)%RI(1 : NsimuStepTot))
        end do
    end if

    

    ! Load the State Data
    if (g_LoadState .eqv. .true.) then 

    
        ! transfer the WarmupDateTime_simu to standard TimeFile Format
        ! like: 2024-11-02_12-00-00
        
        WarmupFileDate = GetFileUseDatetime(WarmupDateTime_simu)
        call LoadStates(WarmupFileDate, W0, SS0, SI0, bIsError) 
        
    else 
        !  the state is calculated as initial conditions
        !  load initial conditions from ICS
        call ReadICSFile(g_WU0, g_SS0, g_SI0, g_sRiver0)
        
        where(g_Mask/=g_NoData_Value)
            W0 = g_WU0*g_tParams%WM/100.0
        elsewhere
            W0 = g_NoData_Value
        end where
        
        SS0 = g_SS0
        SI0 = g_SI0
        sRiver0 = g_sRiver0
    end if

    deallocate(g_WU0)
    deallocate(g_SS0)
    deallocate(g_SI0)
    deallocate(g_sRiver0)

    
    do k = 1, NsimuStepTot

        P_monitor = real(k) / real(NsimuStepTot) * 100.0
        
        ! write(*,"('Step:',I5,' | Time:',A,' | Progress:',F5.1,'%')") k+1, SimuDateTimeList(k+1), P_monitor
        filled_length = int(P_monitor / 100.0 * bar_length)
        bar = repeat('#', filled_length) // repeat('-', bar_length - filled_length)
        
        ! write(*,'(A,A,F5.1,A,A)', advance='no') &
        !     CHAR(13)//'Progress: [', bar, P_monitor, '%   Time: ', trim(adjustl(SimuDateTimeList(k+1)))
        write(P_monitor_str, '(F5.1)') P_monitor
        
        write(*,'(A)', advance='no') CHAR(13) // &
            'Progress: [' // green // bar // reset // '] ' // &
            yellow // trim(adjustl(P_monitor_str)) // '% ' // reset // &
            'Time: ' // cyan // trim(GetPrintDatetime(SimuDateTimeList(k))) // reset

        call flush(6)  ! clean stdout
        
        write(g_CREST_LogFileID,*) k," ", SimuDateTimeList(k)
        

        ! Reading Rain File
        
        select case (trim(RainDisType))
            case ('HOMO')  
                RainSeriesFileName = trim(g_RainPath) // '.csv'
                inquire(file = RainSeriesFileName, exist = FileExist)
                           
                if (.not. FileExist) then
                    print *, 'Error: can not find the rainfall input as: ', RainSeriesFileName 
                    stop 1
                end if
                ! Array element index starts from 1    
                call csvRead(RainSeriesFileName, 1, 2, RainSeries)
                Rain = RainSeries(k) / g_TimeStep

                
            case ('HETE')  
                OutputDatetime = GetFileUseDatetime(SimuDateTimeList(k))
                call ReadMatrixFile(TRIM(g_RainPath), Rain,  &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_RainFormat, &
                TRIM(OutputDatetime))
                
                
                if(bIsError .eqv. .true.)then                 
                    print *, 'Rain Data File Missed.'
                    stop 1
                else 
                    where(Rain > 0) 
                        Rain = Rain / g_TimeStep
                    end where
                end if   
                
            case default
                print *, "Invalid rainfall type"
                STOP 'Input RainType is invalid (required "homo" or "hete" in .prj file).'
        end select

            

        ! Reading PET File
        select case (trim(PETDisType))
            case ('HOMO')  

                PetSeriesFileName = trim(g_PETPath) // '.csv'
                inquire(file = PetSeriesFileName, exist = FileExist)
                           
                if (.not. FileExist) then
                    print *, 'Error: can not find the pet input as: ', PetSeriesFileName 
                    stop 1
                end if

                call csvRead(PetSeriesFileName, 1, 2, PETSeries)
                ! The following code is only effective in zhufengxi project
                ! 读取洙凤溪流域平均每日蒸散发值 (mm)
                ! 根据当前日期的day决定ET强度
                ! 计算得到当前时间在一年中的day DOY
                CurrentDayInYear = GetDayInYear(SimuDateTimeList(k))

                ! 此时PET的单位：mm / day,根据时间步长转化为相应值
                ! mm / day ----> mm / g_TimeMark
                select case (g_TimeMark)
                    case ('d')  !  mm / day
                        PET = PETSeries(CurrentDayInYear) 
                    case ('h')  ! mm / hour
                        PET = PETSeries(CurrentDayInYear) / 24.0
                    case ('u')  ! mm / minute
                        PET = PETSeries(CurrentDayInYear) / 1440.0
                    case ('s')  ! mm / second
                        PET = PETSeries(CurrentDayInYear) / 86400.0
                end select


            case ('HETE') 
                OutputDatetime = GetFileUseDatetime(SimuDateTimeList(k))
                call ReadMatrixFile(TRIM(g_PETPath), PET, &
                g_NCols, g_NRows,g_XLLCorner,g_YLLCorner, &
                g_CellSize,g_NoData_Value,bIsError,g_PETFormat, &
                TRIM(OutputDatetime))

                if(bIsError .eqv. .true.)then

                    print *, 'PET Data File Missed.'
                    stop 1        

                else 
                    where(PET > 0)  
                        PET = PET / g_TimeStep
                    end where
                end if

            case default
                print *, "Invalid PET type"
                STOP 'Input PETType is invalid (required "homo" or "hete" in .prj file).'
        end select
        
        ! Initialize temporary variables 
        passRS = 0.0d0
        passRI = 0.0d0
        passRiver = 0.0d0
        SS0_track = 0.0d0
        SI0_track = 0.0d0
        sRiver0_track = 0.0d0
        
        ! activate the hydrological threads
        call omp_set_num_threads(Nthread_hydro)
        !call omp_set_num_threads(2)
        !write (*,*) OMP_GET_MAX_THREADS()

        HydroTime_start = OMP_get_wtime()
        
        !-------------hydrological parallel region start-----------------
        !$OMP PARALLEL PRIVATE(g_SubMask, i_basin_str) DEFAULT(SHARED)
        !$OMP DO
        do i_basin = 1, N_Subbasin
            write(i_basin_str , '(i3)') i_basin
            ! read the mask of all sub-basins for each computing thread
            g_SubMask = Subbasin_assemble(i_basin, :, :)
            ! print '("Thread: ", i0)', omp_get_thread_num()
            ! calculate the hillslope process based on sub-basins
            ! DO NOT redo the calculation for channel pixels
            do i=0, g_NRows-1
                do j=0, g_NCols-1
                    ! for all cells in subbasin
                    if ( g_SubMask(j,i) == g_NoData_Value &
                        .or. g_Stream(j,i) ==1 ) then
                        cycle
                    end if
                    
                    !Convert unite of Rain and PET from mm/dt to mm
                    if(Rain(j,i)>0.0)then
                        Rain(j,i)=Rain(j,i) * g_TimeStep
                    else
                        Rain(j,i)=0.0
                    end if
                    
                    if(PET(j,i)>0.0)then
                        PET(j,i)=PET(j,i) * g_TimeStep
                    else
                        PET(j,i)=0.0
                    end if

                    call CREST_PrecipInt(Rain(j,i), g_tParams%RainFact(j,i),Rain(j,i))
                    
                    call CREST_EPotential(PET(j,i), g_tParams%KE(j,i),EPot(j,i))
                    
                    call CREST_RunoffGen(W0(j,i), Rain(j,i),EPot(j,i),  &
                            g_tParams%WM(j,i),g_tParams%IM(j,i),  &
                            g_tParams%B(j,i),g_tParams%Ksat(j,i)*g_TimeStep,  &
                            W(j,i),ExcS(j,i),ExcI(j,i))
                    
                    call CREST_EAct(W0(j,i), Rain(j,i),EPot(j,i),W(j,i),EAct(j,i))
                    
                    ! update the soil water amount 
                    W0(j,i) = W(j,i)

                   ! Calculate the routing water: overland flow
                    SS0(j,i) = SS0(j,i)+ExcS(j,i)
                    if (SS0(j,i) > EPS_STORAGE) then
                        RS(j,i) = SS0(j,i) * g_tParams%KS(j,i)
                    else
                        SS0(j,i) = 0.0_dp
                        RS(j,i) = 0.0_dp
                    end if
                    
                    ! Calculate the routing water: subsurface  flow
                    SI0(j,i) = SI0(j,i)+ExcI(j,i)


                    if (SI0(j,i) > EPS_STORAGE) then
                        RI(j,i) = SI0(j,i) * g_tParams%KI(j,i)
                    else
                        SI0(j,i) = 0.0_dp
                        RI(j,i) = 0.0_dp
                    end if

                    
                    ! calculate the local runoff 
                    Runoff(j,i) = RS(j,i) + RI(j,i)
                    
                    ! overland flow routing
                    call hillslopeRoute(j, i, RS(j,i), passRS, SS0_track, sRiver0_track)
                    
                    ! subsurface flow routing
                    call subflowRoute(j, i, RI(j,i), passRI, SI0_track, sRiver0_track)

                    ! update reservoir state 
                    SS0(j,i) = SS0(j,i) - RS(j,i)
                    SI0(j,i) = SI0(j,i) - RI(j,i)
                    
                    if (SS0(j,i) < EPS_STORAGE) SS0(j,i) = 0.0_dp
                    if (SI0(j,i) < EPS_STORAGE) SI0(j,i) = 0.0_dp
                    

                end do
            end do
        end do
        !$OMP END DO
        !$OMP END PARALLEL
        !------------- parallel end -----------------

        ! update the river water storage
        ! by accepting surface and subsurface flow
        sRiver0 = sRiver0 + sRiver0_track
       
        ! reset the river state track 
        sRiver0_track = 0.0d0
        
    
        ! Channel Routing (stream only)
        do i=0, g_NRows-1
            do j=0, g_NCols-1
                ! only for stream cell 
                if(g_Stream(j,i) == g_NoData_Value &
                    .or. g_Mask(j,i) == g_NoData_Value )then
                
                    cycle

                end if
                
                if(Rain(j,i)>0.0)then
                        Rain(j,i)=Rain(j,i) * g_TimeStep
                else
                    Rain(j,i)=0.0
                end if
                if(PET(j,i)>0.0)then
                    PET(j,i)=PET(j,i) * g_TimeStep
                else
                    PET(j,i)=0.0
                end if

                call CREST_PrecipInt(Rain(j,i), g_tParams%RainFact(j,i),Rain(j,i))
                
                call CREST_EPotential(PET(j,i), g_tParams%KE(j,i),EPot(j,i))
                
                sRiver0(j,i) = sRiver0(j,i) + Rain(j,i) - EPot(j,i)
                
                Runoff(j,i) = sRiver0(j,i)
                

                if ( sRiver0(j,i) <= 0 ) then 
                    sRiver0(j,i) = 0
                    Runoff(j,i) = 0
                else 
                    ! Channel Routing
                    riverWater_routing = sRiver0(j,i) * g_tParams%KS(j,i)
                    ! riverWater_routing = sRiver0(j,i) 
                    call streamRouting(j, i, riverWater_routing, passRiver, sRiver0_track)
                    ! update river water storage
                    sRiver0(j,i) = sRiver0(j,i) - riverWater_routing
                end if 
                
            end do 
        end do 
        
        !update hydrological state 
        SS0 = SS0 + SS0_track
        SI0 = SI0 + SI0_track
        sRiver0 = sRiver0 + sRiver0_track

        
        ! update the runoff map 
        Runoff = Runoff + passRS + passRI + passRiver
        
        
        ! Runoff should be tansfered into m^3 / s:
        ! ---------------------------------------
        ! RS, RI, and Runoff are in mm
        ! g_GridArea is in km^2
        ! g_TimeStepToSeconds is in s

        
        where(g_Mask /= g_NoData_Value)
            runoffDischarge = (Runoff / 1000.0) * (g_GridArea * 1.0e6) / g_TimeStepToSeconds
        end where        
        
        
        ! calculate the soil moisture (SM) of hydrological model, unit: %
        where (g_Mask/=g_NoData_Value .and. g_Stream /= 1)
            g_SM = W*100.0/(g_tParams%WM)
        end where

        
        
        ! do the soil downscaling process
        if (g_ModelCore == 3) then

            ! activate the landslide module threads
            call omp_set_num_threads(Nthread_Land)

            g_SM_fine = g_NoData_Value
            g_FS_3D = -g_NoData_Value
            g_failure_volume = g_NoData_Value
            g_failure_area = g_NoData_Value
            g_probability = g_NoData_Value
            g_cal_count = 0
            g_unstable_count = 0
            if (g_NOutDTs == 0) then
                call SoilDownscale_pre(g_SM)
                call Landslide_module()

            else
                ! only activate the landslide model in specific timestep
                if (g_NOutDTs >= 1) then 
                    do i = 1, g_NOutDTs 

                        if (g_OutDTIndex(i) == k) then
                            call SoilDownscale_pre(g_SM)
                            call Landslide_module()
                        end if

                    end do
                end if 

            end if
        end if



        ! 
        if(g_RunStyle == 1) then

            !Output the state of outlet
            if(g_tOutlet%HasOutlet)then
                call CalculateOutletData(k, Rain /g_tParams%RainFact, PET / g_tParams%KE, EPot,EAct, &
                        W,runoffDischarge,ExcS,ExcI,RS,RI)    
            end if

            
            
            !Output the state of the Pixes
            call CalculateOutPixData(k, Rain /g_tParams%RainFact,PET / g_tParams%KE,EPot,EAct,  &
                    W,runoffDischarge,ExcS,ExcI,RS,RI)

            OutputDatetime = GetFileUseDatetime(SimuDateTimeList(k))

            
            
            
            !output the continuous time series
            if (g_NOutDTs == 0) then
                select case (trim(g_ResultFormat))
                    case("HDF5")
                        HDF5_WriteCount = HDF5_WriteCount + 1
                        call Export_HDF5(OutputDatetime,Rain /g_tParams%RainFact,PET / g_tParams%KE,EPot,EAct,  &
                                W,runoffDischarge,ExcS,ExcI,RS,RI, g_SM, HDF5_WriteCount)
                    case("ASC")
                        call ExportGridVar(OutputDatetime,Rain /g_tParams%RainFact,PET / g_tParams%KE,EPot,EAct,  &
                                W,runoffDischarge,ExcS,ExcI,RS,RI, g_SM)
                end select
            end if



            !Output specific Date
            if (g_NOutDTs >= 1) then 

                Do i = 1, g_NOutDTs 

                    if (g_OutDTIndex(i) == k) then
                    
                        select case (trim(g_ResultFormat))
                            case("HDF5")
                                HDF5_WriteCount = HDF5_WriteCount + 1
                                call Export_HDF5(OutputDatetime,Rain /g_tParams%RainFact,PET / g_tParams%KE,EPot,EAct,  &
                                        W,runoffDischarge,ExcS,ExcI,RS,RI, g_SM, HDF5_WriteCount)
                            case("ASC")
                                call ExportGridVar(OutputDatetime,Rain /g_tParams%RainFact,PET / g_tParams%KE,EPot,EAct,  &
                                        W,runoffDischarge,ExcS,ExcI,RS,RI, g_SM)
                        end select
                    end if

                end do

            end if 
        end if


    

    end do !End time loop of NsimuStepTot
    ! ---------------------- End time loop of NsimuStepTot -------------------




    if(g_RunStyle == 1) then
        
        call OutputPixValueToCSV()
        
        
        ! Save the State Data
        ! default moment: OutputDatetime (the last timestep)
        if (g_SaveState .eqv. .true.) then 
            call SaveStates(OutputDatetime, W0, SS0, SI0)
        end if 
        
        
        if (g_tOutlet%HasOutlet) then
            call OutputOutletValueToCSV()
        end if

    end if

    
    write(g_CREST_LogFileID,*)"Hydrological runtime (s): ", HydroRunTime
    write(g_CREST_LogFileID,*)"Landslide runtime (s): ", LandRunTime
    ! write(*,*)"Hydrological runtime (s): ", HydroRunTime
    ! write(*,*)"Landslide runtime (s): ", LandRunTime
    
    ! deallocate
    

    ! record the passing cell for runoff calculation (Guoding Chen in TUDelft)


    return
end subroutine


