module TimeProcess
    implicit none

    private 
    
    public :: SimuTimeRangeGet, GetDayInYear
    public :: is_valid_datetime_format
    public :: GetFileUseDatetime, GetPrintDatetime
    public :: PrintModelRunTime
contains
    
    !========================================================================
    ! 时间范围生成子程序
    !========================================================================
    subroutine SimuTimeRangeGet(StartTime_readIn, EndTime_readIn, SimuTimeList, &
                                TimeMark, TimeStep, StepSec)
        character(len=*), intent(in)  :: StartTime_readIn
        character(len=*), intent(in)  :: EndTime_readIn
        character(len=1), intent(in)  :: TimeMark
        integer,          intent(in)  :: TimeStep
        character(len=19), allocatable, intent(out) :: SimuTimeList(:)
        integer :: TotalSeconds ! 返回总时间（秒），使用8字节防止溢出
        integer(kind=8), intent(out) :: StepSec ! 返回总时间
        ! 内部变量
        integer(kind=8) :: StartSec, EndSec, CurrentSec
        integer :: num_steps, i
        integer :: y, m, d, h, min, s

        ! check the datetime str format that read from the .prj file 
        call is_valid_datetime_format(StartTime_readIn)
        call is_valid_datetime_format(EndTime_readIn)

        
        ! 1. 解析初始时间并计算总秒数
        call ParseTimeStr(StartTime_readIn, y, m, d, h, min, s)
        StartSec = DateToTotalSeconds(y, m, d, h, min, s)

        ! 2. 解析结束时间并计算总秒数
        call ParseTimeStr(EndTime_readIn, y, m, d, h, min, s)
        EndSec = DateToTotalSeconds(y, m, d, h, min, s)

        ! 3. 计算时间差 (总秒数)
        TotalSeconds = EndSec - StartSec
        
        if (TotalSeconds < 0) then
            print *, "Error: End time is earlier than Start time!"
            stop
        end if

        ! 4. 计算步长对应的秒数
        select case (TimeMark)
            case ('d', 'D')
                StepSec = TimeStep * 86400_8
            case ('h', 'H')
                StepSec = TimeStep * 3600_8
            case ('u', 'U')  ! 'u' 代表分钟
                StepSec = TimeStep * 60_8
            case ('s', 'S')
                StepSec = TimeStep * 1_8
            case default
                print *, "Error: Invalid TimeMark! Use d, h, u, or s."
                stop
        end select
        
        ! 5. 计算时间序列长度并分配数组
        num_steps = int(TotalSeconds / StepSec) + 1
        allocate(SimuTimeList(num_steps))
        
        ! 6. 生成时间序列
        do i = 1, num_steps
            CurrentSec = StartSec + (i - 1) * StepSec
            
            ! 将秒数转换回 日期
            call TotalSecondsToDate(CurrentSec, y, m, d, h, min, s)
            
            ! 格式化写入数组 (输出如 20250507_000000)
            write(SimuTimeList(i), '(I4.4, I2.2, I2.2, "_", I2.2, I2.2, I2.2)') &
                  y, m, d, h, min, s
        end do

    end subroutine SimuTimeRangeGet

    !========================================================================
    ! 辅助功能：将时间字符串解析为年月日时分秒
    !========================================================================
    subroutine ParseTimeStr(TimeStr, y, m, d, h, min, s)
        character(len=*), intent(in)  :: TimeStr
        integer, intent(out) :: y, m, d, h, min, s
        read(TimeStr(1:4),   *) y
        read(TimeStr(5:6),   *) m
        read(TimeStr(7:8),   *) d
        read(TimeStr(10:11), *) h
        read(TimeStr(12:13), *) min
        read(TimeStr(14:15), *) s
    end subroutine ParseTimeStr

    !========================================================================
    ! 辅助功能：年月日时分秒转为自公元以来的总秒数
    !========================================================================
    function DateToTotalSeconds(y, m, d, h, min, s) result(total_sec)
        integer, intent(in) :: y, m, d, h, min, s
        integer(kind=8) :: total_sec
        integer(kind=8) :: jdn
        
        ! 计算儒略日 (Fliegel & Van Flandern 算法)
        integer :: a, y2, m2
        a = (14 - m) / 12
        y2 = y + 4800 - a
        m2 = m + 12 * a - 3
        jdn = d + (153 * m2 + 2)/5 + 365 * y2 + y2/4 - y2/100 + y2/400 - 32045
        
        ! 换算为总秒数
        total_sec = jdn * 86400_8 + int(h, 8) * 3600_8 + int(min, 8) * 60_8 + int(s, 8)
    end function DateToTotalSeconds

    !========================================================================
    ! 辅助功能：总秒数转回年月日时分秒
    !========================================================================
    subroutine TotalSecondsToDate(total_sec, y, m, d, h, min, s)
        integer(kind=8), intent(in)  :: total_sec
        integer, intent(out) :: y, m, d, h, min, s
        integer(kind=8) :: jdn, rem
        integer :: l, n, i, j
        
        jdn = total_sec / 86400_8
        rem = mod(total_sec, 86400_8)
        
        ! 计算时分秒
        h   = int(rem / 3600_8)
        rem = mod(rem, 3600_8)
        min = int(rem / 60_8)
        s   = int(mod(rem, 60_8))
        
        ! 儒略日转公历日期 (Fliegel & Van Flandern 算法)
        l = int(jdn) + 68569
        n = (4 * l) / 146097
        l = l - (146097 * n + 3) / 4
        i = (4000 * (l + 1)) / 1461001
        l = l - (1461 * i) / 4 + 31
        j = (80 * l) / 2447
        d = l - (2447 * j) / 80
        l = j / 11
        m = j + 2 - (12 * l)
        y = 100 * (n - 49) + i + l
    end subroutine TotalSecondsToDate




    subroutine is_valid_datetime_format(datetime_str)

      implicit none
      
      character(len=*), intent(in) :: datetime_str
      integer :: trimmed_length, underscore_pos
      logical :: is_valid

      ! Get trimmed length (no trailing spaces)
      trimmed_length = len_trim(datetime_str)

      ! Find the position of the underscore
      underscore_pos = index(datetime_str(1:trimmed_length), "_")

      ! Check: must be 15 characters long with underscore at position 9
      is_valid = (trimmed_length == 15 .and. underscore_pos == 9)
      
      if (.not. is_valid) then 
         print *, "ERROR: Invalid datetime format. Expected 'YYYYMMDD_hhmmss'"
         stop 1 
      end if 
      
    end subroutine is_valid_datetime_format

    !计算输入时刻在当年是第几天 (1~366)
    !========================================================================
    function GetDayInYear(CurrentSimuTime_Str) result(DayInYearSimu)
        character(len=*), intent(in) :: CurrentSimuTime_Str
        integer :: DayInYearSimu
        integer :: y, m, d, h, min, s, i
        ! 定义平年每个月的天数
        integer, dimension(12) :: days_in_month = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
        
        ! 解析出年月日
        call ParseTimeStr(CurrentSimuTime_Str, y, m, d, h, min, s)
        
        ! 判断是否为闰年 (能被4整除且不能被100整除，或者能被400整除)
        if ((mod(y, 4) == 0 .and. mod(y, 100) /= 0) .or. (mod(y, 400) == 0)) then
            days_in_month(2) = 29
        end if
        
        ! 累加当前月之前的所有月的天数
        DayInYearSimu = 0
        do i = 1, m - 1
            DayInYearSimu = DayInYearSimu + days_in_month(i)
        end do
        
        ! 加上当前的日数
        DayInYearSimu = DayInYearSimu + d
    end function GetDayInYear

    !========================================================================
    !转换时间格式为 2025-05-07_00-00-00 (纯函数)
    !========================================================================
    pure function GetFileUseDatetime(SimuTime) result(OutputTime)
        character(len=*), intent(in) :: SimuTime
        character(len=19) :: OutputTime
        
        ! 利用字符串切片与 // 拼接符重组格式
        OutputTime = SimuTime(1:4)   // '-' // SimuTime(5:6)   // '-' // SimuTime(7:8) // &
                     '_'             // &
                     SimuTime(10:11) // '-' // SimuTime(12:13) // '-' // SimuTime(14:15)
    end function GetFileUseDatetime

    !========================================================================
    !转换时间格式为 2025-05-07 00:00:00 (纯函数)
    !========================================================================
    pure function GetPrintDatetime(SimuTime) result(OutputTime)
        character(len=*), intent(in) :: SimuTime
        character(len=19) :: OutputTime
        
        ! 与上个函数类似，仅替换连接符号
        OutputTime = SimuTime(1:4)   // '-' // SimuTime(5:6)   // '-' // SimuTime(7:8) // &
                     ' '             // &
                     SimuTime(10:11) // ':' // SimuTime(12:13) // ':' // SimuTime(14:15)
    end function GetPrintDatetime

    !========================================================================
    ! 新增：计算并打印模型真实运行时间的子程序
    !========================================================================
    subroutine PrintModelRunTime(StartTick, EndTick, TickRate, OutputMsg)
        integer(kind=8),  intent(in) :: StartTick, EndTick, TickRate
        character(len=*), intent(in) :: OutputMsg
        
        real(kind=8) :: totalSeconds, rem
        real(kind=8) :: secondsWithMillis
        integer      :: RunTimeUse_d, RunTimeUse_h, RunTimeUse_m

        ! 计算总耗时的秒数 (EndTick - StartTick) / TickRate
        totalSeconds = real(EndTick - StartTick, kind=8) / real(TickRate, kind=8)

        ! 转换为日、时、分、秒
        RunTimeUse_d = int(totalSeconds / 86400.0_8)
        rem = mod(totalSeconds, 86400.0_8)
        
        RunTimeUse_h = int(rem / 3600.0_8)
        rem = mod(rem, 3600.0_8)
        
        RunTimeUse_m = int(rem / 60.0_8)
        secondsWithMillis = mod(rem, 60.0_8)

        ! 按照你提供的逻辑进行打印
        if (totalSeconds < 1.0_8) then
            write(*,'(A, F6.3, " seconds")') trim(OutputMsg), totalSeconds
        else
            if (RunTimeUse_d > 0) then
                write(*,'(A, I0, " days ", I0, " hours ", I0, " minutes ", F6.3, " seconds")') &
                      trim(OutputMsg), RunTimeUse_d, RunTimeUse_h, RunTimeUse_m, secondsWithMillis
            else if (RunTimeUse_h > 0) then
                write(*,'(A, I0, " hours ", I0, " minutes ", F6.3, " seconds")') &
                      trim(OutputMsg), RunTimeUse_h, RunTimeUse_m, secondsWithMillis
            else if (RunTimeUse_m > 0) then
                write(*,'(A, I0, " minutes ", F6.3, " seconds")') &
                      trim(OutputMsg), RunTimeUse_m, secondsWithMillis
            else
                write(*,'(A, F6.3, " seconds")') trim(OutputMsg), secondsWithMillis
            end if
        end if
    end subroutine PrintModelRunTime


end module TimeProcess


