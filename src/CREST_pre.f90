
subroutine CREST_Main_Pre(bIsError)

  use CREST_Project
  use CREST_Basic
  use CREST_Param
  use SoilDownscale_Basic
  use Landslide_Basic

  implicit none

  integer :: k, i, j
  logical :: bIsError
  
  bIsError = .false.

  write(*,'(A)') '-------------------- Data Loading --------------------------'
  write(*,'(A)') 'Reading Project Data...'

  call ReadProjectFile(bIsError)


  
  if (bIsError .eqv. .true.) then
    write (*, "(1X,A)") &
      "*** Something wrong in your project file!"
    write (g_CREST_LogFileID, "(1X,A)") &
      "*** Something wrong in your project file!"
    return
  end if

  write(*,'(A)') 'Reading Basic Data:'

  call ReadBasicFile(bIsError)
  
    


  if (bIsError .eqv. .true.) then
    write (*, "(1X,A)") &
      "*** Something wrong in your basics file!"
    write (g_CREST_LogFileID, "(1X,A)") &
      "*** Something wrong in your basics file!"
    return
  end if


  
  do k = 1, g_NOutPixs
    g_tOutPix(k) % Mask = g_NoData_Value
    i = g_tOutPix(k) % Row
    j = g_tOutPix(k) % Col
    write(*,'(A,I0,A)') '    -> Getting Mask Map of OutPix Num ', k, '...'
    
    call GetMask(g_NCols, g_NRows, j, i, g_NoData_Value, &
                 g_FDR, g_NextC, g_NextR, g_tOutPix(k) % Mask, InBasin)
    write(*,'(A,I0,A)') '    -> Writing Mask Map of OutPix Num ', k, ' to File...'

   

    call WriteMatrixFile_Int(trim(g_ResultPath)//"OutPix_" &
                             //trim(g_tOutPix(k) % Name)//"_Mask", &
                             g_tOutPix(k) % Mask, g_NCols, g_NRows, &
                             g_XLLCorner, g_YLLCorner, g_CellSize, g_NoData_Value, &
                             bIsError, g_BasicFormat)

  end do
    


  write(*,'(A)') 'Reading Parameters and Initial Conditions...'
  call ReadParamFile()
  write(*,'(A)') '    -> Parameters read...done'
  
  write (*, *)
  return
end subroutine CREST_Main_Pre





! read the basic information of the project control file
subroutine ReadProjectFile(bIsError)
  use CREST_Project
  use PrjFile_reader
  use CREST_Basic
  use SoilDownscale_Basic
  use Landslide_Basic
  use TimeProcess
  
  implicit none

  integer :: i, j
  character(len=200):: sTemp
  character(len=200):: sTemp_ForNum
  character(len=15) :: PixOutDate_str
  ! Variables for the PixOutDate finding location loop
  integer :: PixOutDate_index 
  
  logical :: bOutPixColRow, bOutletColRow
  double precision:: dblTemp
  logical :: bIsError

  bIsError = .false.

  g_NCols = read_integer_param("NCols_Hydro", g_PrjNP)
  g_NRows = read_integer_param("NRows_Hydro", g_PrjNP)

  g_XLLCorner = read_real_param("XLLCorner_Hydro", g_PrjNP)
  g_YLLCorner = read_real_param("YLLCorner_Hydro", g_PrjNP)

  g_CellSize = read_real_param("CellSize_Hydro", g_PrjNP)

  g_NCols_Land = read_integer_param("NCols_Land", g_PrjNP)
  g_NRows_Land = read_integer_param("NRows_Land", g_PrjNP)

  g_XLLCorner_Land = read_real_param("XLLCorner_Land", g_PrjNP)
  g_YLLCorner_Land = read_real_param("YLLCorner_Land", g_PrjNP)

  g_CellSize_Land = read_real_param("CellSize_Land", g_PrjNP)

  g_NoData_Value = read_real_param("NoData_Value", g_PrjNP)

  ! get the Model map Coordinate System
  call read_char_param("CoordinateSystem", g_PrjNP, g_CS)
  call UPCASE(g_CS)

  ! run style
  call read_char_param("RunStyle", g_PrjNP, g_sRunStyle)
  call UPCASE(g_sRunStyle)

  call read_char_param("ModelCore", g_PrjNP, g_sModelCore)
  call UPCASE(g_sModelCore)

  select case (trim(g_sModelCore))
    case ("HYDRO")
      g_ModelCore = 1
    case ("HYDROSLIDE")
      g_ModelCore = 2
    case ("HYDROSLIDE3D")
      g_ModelCore = 3
    case default
      g_ModelCore = 1
  end select

  select case (g_sRunStyle(1:4))
    case ("SIMU")
      g_RunStyle = 1
    case ("CALI")
      g_RunStyle = 2
    case default
      g_RunStyle = 1
  end select

  call read_char_param("HydroBasicFormat", g_PrjNP, g_BasicFormat)
  call UPCASE(g_BasicFormat)

  call read_char_param("HydroBasicPath", g_PrjNP, g_BasicPath)
  
  call read_char_param("SoilDownscaleFormat", g_PrjNP, g_SoilDownscaleFormat)
  call UPCASE(g_SoilDownscaleFormat)

  call read_char_param("SoilDownscalePath", g_PrjNP, g_SoilDownscalePath)

  call read_char_param("LandslideFormat", g_PrjNP, g_LandslideFormat)
  call UPCASE(g_LandslideFormat)

  call read_char_param("LandslidePath", g_PrjNP, g_LandslidePath)

  call read_char_param("ParamFormat", g_PrjNP, g_ParamFormat)
  call UPCASE(g_ParamFormat)

  call read_char_param("ParamPath", g_PrjNP, g_ParamPath)

  call read_char_param("StateFormat", g_PrjNP, g_StateFormat)
  call UPCASE(g_StateFormat)

  call read_char_param("StatePath", g_PrjNP, g_StatePath)

  call read_char_param("ICSFormat", g_PrjNP, g_ICSFormat)
  call UPCASE(g_ICSFormat)

  call read_char_param("ICSPath", g_PrjNP, g_ICSPath)

  call read_char_param("OBSFormat", g_PrjNP, g_OBSFormat)
  call UPCASE(g_OBSFormat)

  call read_char_param("OBSPath", g_PrjNP, g_OBSPath)

  call read_char_param("RainFormat", g_PrjNP, g_RainFormat)
  call UPCASE(g_RainFormat)
  

  call read_char_param("RainType", g_PrjNP, RainDisType)
  call UPCASE(RainDisType)
  call read_char_param("RainPath", g_PrjNP, g_RainPath)

  call read_char_param("PETType", g_PrjNP, PETDisType)
  call read_char_param("PETFormat", g_PrjNP, g_PETFormat)

  call UPCASE(g_PETFormat)
  call UPCASE(PETDisType)
  

  call read_char_param("PETPath", g_PrjNP, g_PETPath)

  call read_char_param("ResultFormat", g_PrjNP, g_ResultFormat)
  call UPCASE(g_ResultFormat)

  call read_char_param("ResultPath", g_PrjNP, g_ResultPath)

  
  ! Model Run numerical setup
  call read_char_param("TimeMark", g_PrjNP, g_TimeMark)
  g_TimeStep = read_integer_param("TimeStep", g_PrjNP)

  ! simulate time
  call read_char_param("StartDate", g_PrjNP, StartDateTime_simu)
  
  call read_char_param("EndDate", g_PrjNP, EndDateTime_simu)

  ! warmup info
  g_LoadState = read_logical_param("LoadState", g_PrjNP)
  g_SaveState = read_logical_param("SaveState", g_PrjNP)
  call read_char_param("WarmupDate", g_PrjNP, WarmupDateTime_simu)

  ! get the simulation duration list
  call SimuTimeRangeGet(StartDateTime_simu, EndDateTime_simu, SimuDateTimeList, &
                       g_TimeMark, g_TimeStep, g_TimeStepToSeconds)
  
  
  

  
  call read_char_param("OutletName", g_PrjNP, g_tOutlet % Name)
  g_tOutlet%HasOutlet = read_logical_param("HasOutlet", g_PrjNP)
  
  bOutletColRow = read_logical_param("OutletColRow", g_PrjNP)

  if (bOutletColRow .eqv. .true.) then
    g_tOutlet % Col = read_integer_param("OutletCol", g_PrjNP)
    g_tOutlet % Row = read_integer_param("OutletRow", g_PrjNP)
  else
    dblTemp = read_real_param("OutletLong", g_PrjNP)
    ! get the matirx Col and Row for given Long and Lati
    g_tOutlet % Col = int((dblTemp - g_XLLCorner) / g_CellSize)

    dblTemp = read_real_param("OutletLati", g_PrjNP)

    g_tOutlet % Row = int((g_YLLCorner + g_NRows * g_CellSize - dblTemp) / g_CellSize)
    
    if (g_tOutlet % Col < 0 .or. g_tOutlet % Col >= g_NCols &
              .or. g_tOutlet % Row < 0 .or. g_tOutlet % Row >= g_NRows) then
      g_tOutlet % bIsOut = .true.
      print *, 'Error: Your outlet is out of the basin! Please check it!'
      stop

    else
      g_tOutlet % bIsOut = .false.
    end if
  end if



  ! ---------- output pixcel location read --------------
  g_NOutPixs = read_integer_param("NOutPixs", g_PrjNP)
  
  bOutPixColRow = read_logical_param("OutPixColRow", g_PrjNP)

  
  

  ! check outpit pixcel or not
  if (g_NOutPixs >= 1) then

    allocate (g_tOutPix(1 : g_NOutPixs))
    ! loop all OutPixs point
    do i = 1, g_NOutPixs
      write (sTemp_ForNum, "(I2)") i 
      sTemp_ForNum = adjustl(sTemp_ForNum)

      call read_char_param("OutPixName"//trim(sTemp_ForNum), g_PrjNP, g_tOutPix(i) % Name)
      
      if (bOutPixColRow .eqv. .true.) then
        g_tOutPix(i) % Col = read_integer_param("OutPixCol"//sTemp_ForNum, g_PrjNP)
        g_tOutPix(i) % Row = read_integer_param("OutPixRow"//sTemp_ForNum, g_PrjNP)
        
      else
        dblTemp = read_real_param("OutPixLong"//sTemp_ForNum, g_PrjNP)
        g_tOutPix(i) % Col = int((dblTemp - g_XLLCorner) / g_CellSize)

        dblTemp = read_real_param("OutPixLati"//sTemp_ForNum, g_PrjNP)
        g_tOutPix(i) % Row = int((g_YLLCorner + g_NRows * g_CellSize - dblTemp) / g_CellSize)
      end if

      if (g_tOutPix(i) % Col < 0 &
          .or. g_tOutPix(i) % Col >= g_NCols &
          .or. g_tOutPix(i) % Row < 0 &
          .or. g_tOutPix(i) % Row >= g_NRows) then
        
        g_tOutPix(i) % bIsOut = .true.
      else
        g_tOutPix(i) % bIsOut = .false.
      end if

      allocate (g_tOutPix(i) % Mask(0:g_NCols - 1, 0:g_NRows - 1))

    end do

  end if 


  

  !Grid Outputs Information
  allocate (g_bGOVar(lbound(g_sGOVarName, 1):ubound(g_sGOVarName, 1)))
  do i = lbound(g_sGOVarName, 1), ubound(g_sGOVarName, 1)
    g_bGOVar(i) = read_logical_param("GOVar_"//trim(g_sGOVarName(i)), g_PrjNP)
  end do


  
  ! Output the specified date
  g_NOutDTs = read_integer_param("NumOfOutputDates", g_PrjNP)

  if (g_NOutDTs >= 1) then 

    allocate (g_OutDTIndex(1:g_NOutDTs))

    Do i = 1, g_NOutDTs 
      write (sTemp_ForNum, "(I2)") i

      sTemp_ForNum = adjustl(sTemp_ForNum)

      call read_char_param("OutputDate_"//sTemp_ForNum, g_PrjNP, sTemp)

      call is_valid_datetime_format(sTemp)

      PixOutDate_str = trim(sTemp)

      ! --- Search using a DO-loop ---
      PixOutDate_index = 0 ! Initialize to 0 (not found)
      do j = 1, size(SimuDateTimeList)
        ! Use TRIM to avoid issues with trailing whitespace
        if (trim(SimuDateTimeList(j)) == trim(PixOutDate_str)) then
          PixOutDate_index = j
          exit ! Found it, no need to search further
        end if
      end do

      if (PixOutDate_index == 0) then
        write(*, '(A)') "ERROR: PixOutDate (" // trim(PixOutDate_str) // ") not found in SimuDateTimeList."
        error stop 1
      end if

      
      g_OutDTIndex(i) = PixOutDate_index
    end do
  end if 




  return
end subroutine ReadProjectFile

! read the basic files, e.g., DEM, FDR, FAC....
subroutine ReadBasicFile(bIsError)
  use CREST_Project
  use CREST_Basic
  use SoilDownscale_Basic
  use Landslide_Basic

  implicit none
  logical :: bIsError
  double precision, allocatable :: AreaFact(:, :)



  ! read the hydrological DEM data
  allocate (g_DEM(0:g_NCols - 1, 0:g_NRows - 1))
  write(*,'(A)') '    --- Reading Hydro-DEM File'
  call ReadMatrixFile(trim(g_BasicPath)//"DEM", g_DEM, &
                      g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                      g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")


  if (bIsError .eqv. .true.) then
    return
  end if

  allocate (g_FDR(0:g_NCols - 1, 0:g_NRows - 1))
  write(*,'(A)') '    --- Reading FDR File'
  call ReadMatrixFile_Int(trim(g_BasicPath)//"FDR", g_FDR, &
                          g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                          g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")
  if (bIsError .eqv. .true.) then
    return
  end if




  !Convert to Standard Flow Direction Map
  if (count(g_FDR == 3) /= 0 &
      .or. count(g_FDR == 5) /= 0 &
      .or. count(g_FDR == 7) /= 0) then
    write (*, *) "     ---Converting DDM to FDR!"
    call ConvDDMToFDR()

    call WriteMatrixFile_Int(trim(g_ResultPath)//"FDR", &
                             g_FDR, g_NCols, g_NRows, &
                             g_XLLCorner, g_YLLCorner, g_CellSize, g_NoData_Value, &
                             bIsError, g_BasicFormat)
  end if

  allocate (g_FAC(0:g_NCols - 1, 0:g_NRows - 1))
  write(*,'(A)') '    --- Reading FAC File'
  call ReadMatrixFile_Int(trim(g_BasicPath)//"FAC", g_FAC, &
                          g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                          g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")


  allocate (g_GridArea(0:g_NCols - 1, 0:g_NRows - 1))
  allocate (g_NextLen(0:g_NCols - 1, 0:g_NRows - 1))
  allocate (g_NextC(0:g_NCols - 1, 0:g_NRows - 1))
  allocate (g_NextR(0:g_NCols - 1, 0:g_NRows - 1))

  write(*,'(A)') '    --- Reading GridArea File'
  call ReadMatrixFile(trim(g_BasicPath)//"GridArea", g_GridArea, &
                      g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                      g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")
  call AssignNextGroup(g_FDR, bIsError)

  write(*,'(A)') '    --- Reading AreaFact File'
  allocate (AreaFact(0:g_NCols - 1, 0:g_NRows - 1))
  call ReadMatrixFile(trim(g_BasicPath)//"AreaFact", AreaFact, &
                      g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                      g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")
  if (bIsError .eqv. .false.) then
    where (g_Mask /= g_NoData_Value)
      g_GridArea = AreaFact * g_GridArea
    end where
  end if

  !Read Stream Map
  allocate (g_Stream(0:g_NCols - 1, 0:g_NRows - 1))
  write(*,'(A)') '    --- Reading Stream File'
  call ReadMatrixFile_Int(trim(g_BasicPath)//"Stream", g_Stream, &
                          g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                          g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")
  if (bIsError .eqv. .true.) then
    call GetStream()

    call WriteMatrixFile_Int(trim(g_ResultPath)//"Stream", &
                             g_Stream, g_NCols, g_NRows, &
                             g_XLLCorner, g_YLLCorner, g_CellSize, g_NoData_Value, &
                             bIsError, g_BasicFormat)
  end if


  
  !Read Mask Map
  allocate (g_Mask(0:g_NCols - 1, 0:g_NRows - 1))
  allocate (g_tOutlet%Mask(0:g_NCols - 1, 0:g_NRows - 1))
  write(*,'(A)') '    --- Trying to read the MASK file from Basics folder'
  call ReadMatrixFile_Int(trim(g_BasicPath)//"Mask", g_Mask, &
                          g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                          g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")      
  if (bIsError .eqv. .true.) then
    if (g_tOutlet % HasOutlet) then
      !Get Mask Map by Outlet
      write(*,'(A)') '>> WARNING: Can''t find the MASK file in Basics folder'
      write(*,'(A)') '    -> Get Mask Map by Outlet'

      call GetMask(g_NCols, g_NRows, g_tOutlet % Col, g_tOutlet % Row, &
                   g_NoData_Value, g_FDR, g_NextC, g_NextR, &
                   g_Mask, InBasin)
      g_tOutlet%Mask = g_Mask
    else
      write (*, *) "Get Mask Map by DEM in Basics folder" ! by cgd
      call GetMaskByNoData(g_NCols, g_NRows, &
                           g_NoData_Value, g_DEM, g_Mask)
    end if
    ! generate the mask based on the outlet, and write it to the folder
    call WriteMatrixFile_Int(trim(g_ResultPath)//"Mask_Outlet", &
                             g_Mask, g_NCols, g_NRows, &
                             g_XLLCorner, g_YLLCorner, g_CellSize, g_NoData_Value, &
                             bIsError, g_BasicFormat)
  else 
    write(*,'(A)') '>> Find MASK file in Basics folder'
    g_tOutlet%Mask = g_Mask
  end if




  !Read Slope Map
  allocate (g_Slope(0:g_NCols - 1, 0:g_NRows - 1))
  allocate (g_Slope_angle(0:g_NCols - 1, 0:g_NRows - 1))
  write(*,'(A)') '    --- Reading Slope File'
  call ReadMatrixFile(trim(g_BasicPath)//"Slope", g_Slope, &
                      g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                      g_CellSize, g_NoData_Value, bIsError, g_BasicFormat, "")
  if (bIsError .eqv. .true.) then

    call CalSlope_Tiger(bIsError)

    if (bIsError .eqv. .true.) then
      call CalSlope()
    end if

  end if

  where (g_Mask == 1)
    g_Slope_angle = ATAND(g_Slope)
  elsewhere
    g_Slope_angle = g_Slope
  end where
  ! output the slope file with unit of angle
  call WriteMatrixFile(trim(g_ResultPath)//"Slope_angle", &
                       g_Slope_angle, g_NCols, g_NRows, &
                       g_XLLCorner, g_YLLCorner, g_CellSize, g_NoData_Value, &
                       bIsError, g_BasicFormat)

  ! Limit the matrix to the mask area
  where (g_Mask == g_NoData_Value)
    g_FDR = g_NoData_Value
    g_FAC = g_NoData_Value
    g_Stream = g_NoData_Value
  end where

  if (g_ModelCore == 3) then
    ! read the landslide mask data
    allocate (g_mask_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))
    write (*, *) "Reading Landslide File!"
    call ReadMatrixFile_Int(trim(g_LandslidePath)//"mask_fine", g_mask_fine, &
                            g_NCols_Land, g_NRows_Land, g_xllCorner_Land, g_yllCorner_Land, &
                            g_CellSize_Land, g_NoData_Value, bIsError, g_LandslideFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    ! read the soil map with coarse resolution
    allocate (g_soil(0:g_NCols - 1, 0:g_NRows - 1))

    call ReadMatrixFile_Int(trim(g_LandslidePath)//"Soil", g_soil, &
                            g_NCols, g_NRows, g_xllCorner, g_yllCorner, &
                            g_CellSize, g_NoData_Value, bIsError, g_LandslideFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    ! read the landslide DEM data
    allocate (g_DEM_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

    call ReadMatrixFile(trim(g_LandslidePath)//"DEM_fine", g_DEM_fine, &
                        g_NCols_Land, g_NRows_Land, g_xllCorner_Land, g_yllCorner_Land, &
                        g_CellSize_Land, g_NoData_Value, bIsError, g_LandslideFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    where (g_DEM_fine < 0)
      g_DEM_fine = 0
    end where

    ! read the landslide slope data
    allocate (g_slope_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

    call ReadMatrixFile(trim(g_LandslidePath)//"slope_fine", g_slope_fine, &
                        g_NCols_Land, g_NRows_Land, g_xllCorner_Land, g_yllCorner_Land, &
                        g_CellSize_Land, g_NoData_Value, bIsError, g_LandslideFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    where (g_slope_fine == 0)
      g_slope_fine = 0.1
    end where

    ! read the landslide slope data
    allocate (g_aspect_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

    call ReadMatrixFile(trim(g_LandslidePath)//"aspect_fine", g_aspect_fine, &
                        g_NCols_Land, g_NRows_Land, g_xllCorner_Land, g_yllCorner_Land, &
                        g_CellSize_Land, g_NoData_Value, bIsError, g_LandslideFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    where (g_aspect_fine < 0)
      g_aspect_fine = 1
    end where

    allocate (g_FS_3D(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))
    allocate (g_failure_volume(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))
    allocate (g_failure_area(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))
    allocate (g_probability(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))
    allocate (g_cal_count(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))
    allocate (g_unstable_count(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

    !----------------- read the soil downscaling data------------
    allocate (g_aspect_coarse(0:g_NCols - 1, 0:g_NRows - 1))
    write (*, *) "  Reading soil downscaling File!"
    call ReadMatrixFile(trim(g_SoilDownscalePath)//"aspect_coarse", g_aspect_coarse, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_SoilDownscaleFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    
    allocate (g_curvature_coarse(0:g_NCols - 1, 0:g_NRows - 1))

    call ReadMatrixFile(trim(g_SoilDownscalePath)//"curvature_coarse", g_curvature_coarse, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_SoilDownscaleFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    allocate (g_TWI_coarse(0:g_NCols - 1, 0:g_NRows - 1))

    call ReadMatrixFile(trim(g_SoilDownscalePath)//"TWI_coarse", g_TWI_coarse, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_SoilDownscaleFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    allocate (g_TWI_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

    call ReadMatrixFile(trim(g_SoilDownscalePath)//"TWI_fine", g_TWI_fine, &
                        g_NCols_Land, g_NRows_Land, g_xllCorner_Land, g_yllCorner_Land, &
                        g_CellSize_Land, g_NoData_Value, bIsError, g_SoilDownscaleFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    allocate (g_curvature_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

    call ReadMatrixFile(trim(g_SoilDownscalePath)//"curvature_fine", g_curvature_fine, &
                        g_NCols_Land, g_NRows_Land, g_xllCorner_Land, g_yllCorner_Land, &
                        g_CellSize_Land, g_NoData_Value, bIsError, g_SoilDownscaleFormat, "")
    if (bIsError .eqv. .true.) then
      return
    end if

    ! declare the soil moisture in fine map
    allocate (g_SM_fine(0:g_NCols_Land - 1, 0:g_NRows_Land - 1))

  end if

  return
end subroutine ReadBasicFile



subroutine ReadParamFile()
  use CREST_Project
  use PrjFile_reader
  use CREST_Basic
  use CREST_Param
  use LandslideModel_parameters
  use OMP_LIB
  implicit none

  

  logical :: bIsError
  double precision :: dblTemp

  character(len=20) :: param_type

  ! # # # # Memory allocation # # # # 
  call InitParamsType(g_tParams)
  

  ! --------------------- read RainFact ------------------------
  
  call read_char_param("RainFactType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("RainFact", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % RainFact = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"RainFact", g_tParams % RainFact, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % RainFact = g_NoData_Value
        end where
        
      case default
        print *, "Invalid RainFactType"
        STOP 'Input RainFactType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read Ksat ------------------------
  
  call read_char_param("KsatType", g_PrjNP, param_type)
  

  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("Ksat", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % Ksat = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"Ksat", g_tParams % Ksat, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % Ksat = g_NoData_Value
        end where

      case default
        print *, "Invalid KsatType"
        STOP 'Input KsatType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 
  
  ! The default reading unit of ksat is mm / h,
  !  which needs to be converted to mm / g_TimeMark

  ! mm/h ----> mm/g_TimeMark
  select case (g_TimeMark)
      case ('d')  !  mm / day
          g_tParams % Ksat = g_tParams % Ksat * 24.0
      case ('h')  ! mm / hour
          g_tParams % Ksat = g_tParams % Ksat 
      case ('u')  ! mm / minute
          g_tParams % Ksat = g_tParams % Ksat / 60.0
      case ('s')  ! mm / second
          g_tParams % Ksat = g_tParams % Ksat / 3600.0
  end select

  ! --------------------- read WM ------------------------
  
  call read_char_param("WMType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("WM", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % WM = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"WM", g_tParams % WM, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % WM = g_NoData_Value
        end where

      case default
        print *, "Invalid WMType"
        STOP 'Input WMType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  
  ! --------------------- read B ------------------------

  call read_char_param("BType", g_PrjNP, param_type)
  
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("B", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % B = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"B", g_tParams % B, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % B = g_NoData_Value
        end where

      case default
        print *, "Invalid BType"
        STOP 'Input BType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read IM ------------------------
  call read_char_param("IMType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("IM", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % IM = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"IM", g_tParams % IM, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % IM = g_NoData_Value
        end where

      case default
        print *, "Invalid IMType"
        STOP 'Input IMType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read KE ------------------------

  call read_char_param("KEType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("KE", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % KE = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"KE", g_tParams % KE, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % KE = g_NoData_Value
        end where

      case default
        print *, "Invalid KEType"
        STOP 'Input KEType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read coeM ------------------------

  call read_char_param("coeMType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("coeM", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % coeM = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"coeM", g_tParams % coeM, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % coeM = g_NoData_Value
        end where

      case default
        print *, "Invalid coeMType"
        STOP 'Input coeMType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read expM ------------------------

  call read_char_param("expMType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("expM", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % expM = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"expM", g_tParams % expM, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % expM = g_NoData_Value
        end where

      case default
        print *, "Invalid expMType"
        STOP 'Input expMType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read coeR ------------------------

  call read_char_param("coeRType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("coeR", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % coeR = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"coeR", g_tParams % coeR, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % coeR = g_NoData_Value
        end where

      case default
        print *, "Invalid coeRType"
        STOP 'Input coeRType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read coeSType ------------------------

  call read_char_param("coeSType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("coeS", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % coeS = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"coeS", g_tParams % coeS, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % coeS = g_NoData_Value
        end where

      case default
        print *, "Invalid coeSType"
        STOP 'Input coeSType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read KS ------------------------
  
  call read_char_param("KSType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("KS", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % KS = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"KS", g_tParams % KS, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % KS = g_NoData_Value
        end where

      case default
        print *, "Invalid KSType"
        STOP 'Input KSType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  ! --------------------- read KI ------------------------
  
  call read_char_param("KIType", g_PrjNP, param_type)
  
  select case (trim(param_type))
      case ('uniform')  
        ! read the RainFact as single value 
        dblTemp = read_real_param("KI", g_PrjNP)
        where (g_Mask /= g_NoData_Value)
          g_tParams % KI = dblTemp
        end where
        
      case ('distributed')  
        call ReadMatrixFile(trim(g_ParamPath)//"KI", g_tParams % KI, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ParamFormat, "")
        where (g_Mask == g_NoData_Value)
          g_tParams % KI = g_NoData_Value
        end where

      case default
        print *, "Invalid KIType"
        STOP 'Input KIType is invalid (required "uniform" or "distributed" in .prj file).'
  end select 

  
  ! landslide module is open. Read the landslide parameters
  ! ------------------------- read landslide module parameters ----------------
  if (g_ModelCore == 3) then
    ! landslide module is open. Read the landslide parameters

    
    ellipse_density = read_integer_param("ellipse_density", g_PrjNP)

    min_ae = read_real_param("min_ae", g_PrjNP)
    max_ae = read_real_param("max_ae", g_PrjNP)

    min_be = read_real_param("min_be", g_PrjNP)
    max_be = read_real_param("max_be", g_PrjNP)

    min_ce = read_real_param("min_ce", g_PrjNP)
    max_ce = read_real_param("max_ce", g_PrjNP)

    ! here cell_size is the resolution of the landslide map in unit of m
    CellSize_LandInM  = read_real_param("CellSize_LandInM", g_PrjNP)
    
  end if

  
  ! ------------------------- read parallel computation set ----------------

  
  if (g_ModelCore == 1) then ! only for hydrological modeling

    N_Subbasin = read_integer_param("N_Subbasin", g_PrjNP)
    Nthread_hydro = read_integer_param("NHydroThread", g_PrjNP)

  elseif (g_ModelCore == 3) then ! for both hydrology and landslide

    N_Subbasin = read_integer_param("N_Subbasin", g_PrjNP)
    Nthread_hydro = read_integer_param("NHydroThread", g_PrjNP)
    total_tile_number = read_integer_param("Tot_tile", g_PrjNP)
    Nthread_Land = read_integer_param("NLandThread", g_PrjNP)

  end if
  ! check the validation of the parallel setup
  if (Nthread_hydro > OMP_GET_MAX_THREADS() .or. &
      Nthread_Land > OMP_GET_MAX_THREADS()) then
    write (*, *) "Error: Parallel startup failed! &
&                You are requesting the threads more than maximum number"
    write (*, *) "Available maximum threads are: ", OMP_GET_MAX_THREADS()
    stop
  end if

  return

end subroutine ReadParamFile


! read the initial conditions
subroutine ReadICSFile(g_WU0_read, g_SS0_read, g_SI0_read, g_sRiver0_read)
  use CREST_Project
  use CREST_Basic
  use PrjFile_reader
  
  implicit none
  logical :: bIsError, FileExist
  character(len=200):: FileNameIC, ICValueType
  double precision :: ValueIC
  double precision, intent(out) :: g_WU0_read(0:g_NCols - 1, 0:g_NRows - 1)
  double precision, intent(out) :: g_SS0_read(0:g_NCols - 1, 0:g_NRows - 1)
  double precision, intent(out) :: g_SI0_read(0:g_NCols - 1, 0:g_NRows - 1)
  double precision, intent(out) :: g_sRiver0_read(0:g_NCols - 1, 0:g_NRows - 1)
  
  
  FileNameIC = trim(g_ICSPath)//"InitialConditions.txt"
  !  check initial condition setup file 
  inquire(file = FileNameIC, exist = FileExist)
                           
  if (.not. FileExist) then
      print *, 'Error: can not find the initial condition file: ', FileNameIC
      stop 1
  end if
  

  g_WU0_read = g_NoData_Value
  g_SS0_read = g_NoData_Value
  g_SI0_read = g_NoData_Value
  g_sRiver0_read = g_NoData_Value
  ! --------------------- read WU0 ---------------------------
  call read_char_param("WU0Type", FileNameIC, ICValueType)

  ! check the value type: uniform or distributed
  if (trim(ICValueType) == 'uniform') then 
    ! read the value 
    ValueIC = read_real_param("WU0", FileNameIC)
  
    where (g_Mask /= g_NoData_Value .and. g_Stream /= 1)
      g_WU0_read = ValueIC
    end where
    
  else if (trim(ICValueType) == 'distributed') then 
    !  read the g_WU0 from the path 
    call ReadMatrixFile(trim(g_ICSPath)//'WU0', g_WU0_read, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ICSFormat, "")
  else

    print *, " Error: value type for IC is only accepted as 'uniform' or 'distributed' "
    stop 1

  end if 


  ! --------------------- read SS0 ---------------------------
  call read_char_param("SS0Type", FileNameIC, ICValueType)

  ! check the value type: uniform or distributed
  if (trim(ICValueType) == 'uniform') then 
    ! read the value 
    ValueIC = read_real_param("SS0", FileNameIC)

    where (g_Mask /= g_NoData_Value .and. g_Stream /= 1)
      g_SS0_read = ValueIC
    end where
    
  else if (trim(ICValueType) == 'distributed') then 
    !  read the g_WU0 from the path 
    call ReadMatrixFile(trim(g_ICSPath)//'SS0', g_SS0_read, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ICSFormat, "")
  else

    print *, " Error: value type for IC is only accepted as 'uniform' or 'distributed' "
    stop 1
  end if   


  ! --------------------- read SI0 ---------------------------
  call read_char_param("SI0Type", FileNameIC, ICValueType)

  ! check the value type: uniform or distributed
  if (trim(ICValueType) == 'uniform') then 
    ! read the value 
    ValueIC = read_real_param("SI0", FileNameIC)

    where (g_Mask /= g_NoData_Value .and. g_Stream /= 1)
      g_SI0_read = ValueIC
    end where
    
  else if (trim(ICValueType) == 'distributed') then 
    !  read the g_WU0 from the path 
    call ReadMatrixFile(trim(g_ICSPath)//'SI0', g_SI0_read, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ICSFormat, "")
  else

    print *, " Error: value type for IC is only accepted as 'uniform' or 'distributed' "
    stop 1
  end if   
  
  ! --------------------- read sRiver0 ---------------------------
  call read_char_param("sRiver0Type", FileNameIC, ICValueType)

  ! check the value type: uniform or distributed
  if (trim(ICValueType) == 'uniform') then 
    ! read the value 
    ValueIC = read_real_param("sRiver0", FileNameIC)
    
    where (g_Mask /= g_NoData_Value .and. g_Stream == 1)
      g_sRiver0_read = ValueIC
    end where
    
  else if (trim(ICValueType) == 'distributed') then 
    !  read the g_WU0 from the path 
    call ReadMatrixFile(trim(g_ICSPath)//'sRiver0', g_sRiver0_read, &
                        g_NCols, g_NRows, g_XLLCorner, g_YLLCorner, &
                        g_CellSize, g_NoData_Value, bIsError, g_ICSFormat, "")
  else
    print *, " Error: value type for IC is only accepted as 'uniform' or 'distributed' "
    stop 1
  end if   


  return
end subroutine ReadICSFile


subroutine AssignNextGroup(FDR, bIsNotExist)
  use CREST_Project
  use CREST_Basic
  implicit none
  ! character(len = 20) :: g_CS
  logical :: bGCS, bIsNotExist ! Examine whether the GridArea.asc exist

  integer :: FDR(0:g_NCols - 1, 0:g_NRows - 1)
  integer :: j, i
  double precision :: LenSN, LenEW, LenCross

  !Identify whether it is a Geographic Coordinate System (GCS)
  !   or Projected Coordinate System (PCS)

  if (g_XLLCorner >= -180.0 .and. g_XLLCorner <= 180 &
      .and. g_YLLcorner >= -90 .and. g_YLLCorner <= 90) then
    bGCS = .true.
  else
    bGCS = .false.
  end if

  if (trim(g_CS) == "GCS") then
    bGCS = .true.
  else
    bGCS = .false.
  end if

  if (bGCS .eqv. .true.) then
    LenSN = g_CellSize * 110574.0
  else
    LenSN = g_CellSize
  end if
  
  
  do i = 0, g_NRows - 1
    do j = 0, g_NCols - 1
      if (FDR(j, i) /= g_NoData_Value) then
        if (bGCS .eqv. .true.) then
          LenEW = g_YLLCorner + (g_NRows - i - 0.5) * g_CellSize
          LenEW = LenSN * cos(LenEW * 4.0 * atan(1.0) / 180.0)
        else
          LenEW = g_CellSize
        end if
        LenCross = sqrt(LenEW**2 + LenSN**2)

        if (bIsNotExist .eqv. .true.) then
          g_GridArea(j, i) = LenSN * LenEW / 1.0e6 ! Convert to km^2
        end if

        select case (FDR(j, i))
        case (64)
          g_NextR(j, i) = i - 1
          g_NextC(j, i) = j
          g_NextLen(j, i) = LenSN
        case (128)
          g_NextR(j, i) = i - 1
          g_NextC(j, i) = j + 1
          g_NextLen(j, i) = LenCross
        case (1)
          g_NextR(j, i) = i
          g_NextC(j, i) = j + 1
          g_NextLen(j, i) = LenEW
        case (2)
          g_NextR(j, i) = i + 1
          g_NextC(j, i) = j + 1
          g_NextLen(j, i) = LenCross
        case (4)
          g_NextR(j, i) = i + 1
          g_NextC(j, i) = j
          g_NextLen(j, i) = LenSN
        case (8)
          g_NextR(j, i) = i + 1
          g_NextC(j, i) = j - 1
          g_NextLen(j, i) = LenCross
        case (16)
          g_NextR(j, i) = i
          g_NextC(j, i) = j - 1
          g_NextLen(j, i) = LenEW
        case (32)
          g_NextR(j, i) = i - 1
          g_NextC(j, i) = j - 1
          g_NextLen(j, i) = LenCross
        case (256) !Outlet
          g_NextR(j, i) = g_NRows
          g_NextC(j, i) = g_NCols
          g_NextLen(j, i) = LenCross
        case default
          write (*, *) "Something is wrong " &
            //"in your Flow Direction Map!"
        end select


        if (InBasin(g_NextC(j, i), &
                    g_NextR(j, i)) .eqv. .false.) then
          !                                                 g_NextC(j,i)=g_NoData_Value
          !                                                 g_NextR(j,i)=g_NoData_Value
          g_NextLen(j, i) = LenSN
        end if
      else
        g_NextC(j, i) = g_NoData_Value
        g_NextR(j, i) = g_NoData_Value
        g_NextLen(j, i) = g_NoData_Value
        if (bIsNotExist .eqv. .true.) then
          g_GridArea(j, i) = g_NoData_Value
        end if
      end if
    end do
  end do

  return
end subroutine AssignNextGroup


subroutine GetMask(NCols, NRows, jCol_outlet, iRow_outlet, NoData_Value, FDR, &
                   NextC, NextR, MaskOut, InBasin)
  implicit none
  integer :: iRow_outlet, jCol_outlet !Objective Row/Col
  integer :: NCols, NRows
  double precision :: NoData_Value
  integer :: NextC(0:NCols - 1, 0:NRows - 1)
  integer :: NextR(0:NCols - 1, 0:NRows - 1)
  integer :: FDR(0:NCols - 1, 0:NRows - 1)
  integer, intent(out) :: MaskOut(0:NCols - 1, 0:NRows - 1)
  integer :: i, j, iiA, iiB, jjA, jjB
  logical, external :: InBasin

  MaskOut = NoData_Value
  MaskOut(jCol_outlet, iRow_outlet) = 1
  do i = 0, NRows - 1

    do j = 0, NCols - 1
      !    write(*,*) i, j
      !if (i == 338 .and. j == 338) then
      !    pause
      !end if
      if (FDR(j, i) == NoData_Value) then
        cycle
      end if
      iiA = NextR(j, i)
      jjA = NextC(j, i)
      do while (InBasin(jjA, iiA) .eqv. .true.)
        if (FDR(jjA, iiA) == NoData_Value) then
          exit
        end if
        if (iiA == iRow_outlet .and. jjA == jCol_outlet) then
          MaskOut(j, i) = 1
          exit
        end if
        iiB = iiA
        jjB = jjA
        iiA = NextR(jjB, iiB)
        jjA = NextC(jjB, iiB)
      end do
    end do
  end do
  return
end subroutine GetMask

subroutine GetStream()
  use CREST_Project
  use CREST_Basic

  implicit none
  double precision :: TH
  logical :: fExist

  integer :: i, j
  integer :: fileid
  character(len=200):: fileName
  double precision :: dblTemp
  fileName = trim(g_BasicPath)//"Stream.def"

  inquire (file=trim(Filename), exist=fExist)
  if (fExist .eqv. .false.) then
    write (*, *) "You should give a Stream Map or 'Stream.def' file!"
    write (g_CREST_LogFileID, *) &
      "You should give a Stream Map or 'Stream.def' file!"
    stop
  end if
  call XXWGetFreeFile(fileid)
  open (fileid, file=TRIM(Filename), form="formatted")
  read (fileid, *) TH
  close (fileid)

  do i = 0, g_NRows - 1
    do j = 0, g_NCols - 1
      if (g_FAC(j, i) > TH) then
        g_Stream(j, i) = 1
      else
        g_Stream(j, i) = g_NoData_Value
      end if
    end do
  end do

  return
end subroutine GetStream




