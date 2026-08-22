module linearRoute

    use CREST_Basic
    use CREST_Project
    use CREST_Param
    implicit none



    private 
    
    public :: hillslopeRoute, subflowRoute, streamRouting
   
    
    
contains


    !---------------------------< overland routing >--------------------
    subroutine hillslopeRoute(j, i, RS, routPassRS, SS0_state, sRiver0_state)
        
        
        integer, intent(in)    :: j, i
        integer :: iiA, jjA,  iiB, jjB
        double precision, intent(in) :: RS
        
        double precision, intent(inout) :: routPassRS(0:, 0:) 
        double precision, intent(inout) :: SS0_state(0:, 0:) 
        double precision, intent(inout) :: sRiver0_state(0:, 0:) 

        double precision :: speedFlowLoacl, speedHillslope
        double precision :: timeHillslope, timeHillslope_tot
        double precision :: excessRatio
        
        iiA = g_NextR(j,i)
        jjA = g_NextC(j,i)

        iiB = i
        jjB = j


        timeHillslope_tot = 0
        
        excessRatio = 0

        do while(timeHillslope_tot < g_TimeStepToSeconds)
            
            
            
            

            speedFlowLoacl = 0.2
            ! m/s 
            speedHillslope = g_tParams%coeM(jjB,iiB) * speedFlowLoacl &
                        *g_Slope(jjB,iiB)**g_tParams%expM(jjB,iiB)
            
            
            ! unit: s
            timeHillslope = g_NextLen(j,i) / speedHillslope
            
            
            timeHillslope_tot = timeHillslope_tot + timeHillslope
            
            if( g_Mask(jjA,iiA) /= g_NoData_Value .and.  g_Stream(jjA,iiA) /= 1) then

                 ! next cell still in basin, do routing 
                
                ! calculate the water allocation to next cell
                ! This means that within the time step, the water from cell B & 
                ! did not fully flow to cell A.
                if (timeHillslope_tot >= g_TimeStepToSeconds) then 
                    !
                    excessRatio = (timeHillslope_tot - g_TimeStepToSeconds) / timeHillslope
                    

                    if (excessRatio > 1) then 
                        print *, 'error with overland flow excessRatio'
                        stop 1
                    end if  

                    !$OMP ATOMIC
                    routPassRS(jjA, iiA) = routPassRS(jjA, iiA) + RS * (1 - excessRatio)
                          
                else 
                    ! fully passing route 

                    !$OMP ATOMIC
                    routPassRS(jjA, iiA) = routPassRS(jjA, iiA) + RS 
                end if 
                
                
            else if (g_Stream(jjA,iiA) == 1) then 

                ! routing to stream channel

                !$OMP ATOMIC
                sRiver0_state(jjA, iiA) = sRiver0_state(jjA, iiA) + RS 
                ! leave the remaining routing task to stream 
                exit 
            else 
                ! (jjA, iiA) is outside of the basin
                ! (jjB, iiB) is in the basin; (jjB, iiB) is (g_tOutlet % Col, g_tOutlet % Row)
                ! stop routing
                
                ! In fact, this code won't happen here because &
                ! it's always blocked by the stream.
                exit 
            end if
            ! index for next cell 
            iiB = iiA
            jjB = jjA
            iiA = g_NextR(jjB, iiB)
            jjA = g_NextC(jjB, iiB)
            
        end do 
        
        ! update the hydrological state 
        if (g_Mask(jjA,iiA) /= g_NoData_Value .and. g_Stream(jjA,iiA) /= 1) then 
            ! the downstream cell is still in basin after loop 
            ! thus, (jjA,iiA) is the destination
            
            if (excessRatio > 0) then 
                ! water did not fully flow to cell A.
                !$OMP ATOMIC
                SS0_state(jjA,iiA) = SS0_state(jjA,iiA) + RS * (1 - excessRatio)
                ! part of water still in cell B
                !$OMP ATOMIC
                SS0_state(jjB,iiB) = SS0_state(jjB,iiB) + RS * excessRatio
            else 
                ! fully passing route 
                !$OMP ATOMIC
                SS0_state(jjA,iiA) = SS0_state(jjA,iiA) + RS
            end if 
            
        end if 



        return
    end subroutine hillslopeRoute



    !---------------------------< subsurface routing >--------------------
    subroutine subflowRoute(j, i, RI, routPassRI, SI0_state, sRiver0_state)
        
        
        integer, intent(in)    :: j, i
        integer :: iiA, jjA,  iiB, jjB
        double precision, intent(in) :: RI
        
        double precision, intent(inout) :: routPassRI(0:, 0:) 
        double precision, intent(inout) :: SI0_state(0:, 0:) 
        double precision, intent(inout) :: sRiver0_state(0:, 0:) 
        
        double precision :: speedFlowLoacl, speedHillslope, speedSubflow
        double precision :: timeSubflow, timeSubflow_tot
        double precision :: excessRatio
        
        iiA = g_NextR(j,i)
        jjA = g_NextC(j,i)
        iiB = i
        jjB = j
        
        timeSubflow_tot = 0
        excessRatio = 0

        do while(timeSubflow_tot < g_TimeStepToSeconds)
            
            
            speedFlowLoacl = 0.2
            ! m/s 
            speedHillslope = g_tParams%coeM(jjB,iiB) * speedFlowLoacl &
                        *g_Slope(jjB,iiB)**g_tParams%expM(jjB,iiB)
            
            speedSubflow = speedHillslope * g_tParams%coeS(j,i)
            
            ! unit: s
            timeSubflow = g_NextLen(j,i) / speedSubflow
            
            timeSubflow_tot = timeSubflow_tot + timeSubflow
            
            
            if( g_Mask(jjA,iiA) /= g_NoData_Value .and.  g_Stream(jjA,iiA) /= 1) then

                 ! next cell still in basin, do routing 
                
                ! calculate the water allocation to next cell
                ! This means that within the time step, the water from cell B & 
                ! did not fully flow to cell A.
                if (timeSubflow_tot >= g_TimeStepToSeconds) then 
                    !
                    excessRatio = (timeSubflow_tot - g_TimeStepToSeconds) / timeSubflow
                    

                    if (excessRatio > 1) then 
                        print *, 'error with subsurface flow excessRatio'
                        stop 1
                    end if  
                    !$OMP ATOMIC
                    routPassRI(jjA, iiA) = routPassRI(jjA, iiA) + RI * (1 - excessRatio)
                          
                else 
                    ! fully passing route 
                    !$OMP ATOMIC
                    routPassRI(jjA, iiA) = routPassRI(jjA, iiA) + RI
                end if 
                
                
                
                
            else if (g_Stream(jjA,iiA) == 1) then 
       
                ! routing to stream channel
                !$OMP ATOMIC
                sRiver0_state(jjA, iiA) = sRiver0_state(jjA, iiA) + RI
                 
                exit ! leave the routing task to stream 
            else 
                ! (jjA, iiA) is outside of the basin
                ! (jjB, iiB) is in the basin; (jjB, iiB) is (g_tOutlet % Col, g_tOutlet % Row)
                exit ! stop routing; out of basin
            end if
            ! index for next cell 
            iiB = iiA
            jjB = jjA
            iiA = g_NextR(jjB, iiB)
            jjA = g_NextC(jjB, iiB)

        end do 
        
        ! update the hydrological state 
        if (g_Mask(jjA,iiA) /= g_NoData_Value .and. g_Stream(jjA,iiA) /= 1) then 
            ! the downstream cell is still in basin after loop 
            ! thus, (jjA,iiA) is the destination

            ! if g_Stream(jjA,iiA) == 1, (jjA,iiA) is not the destination
            if (excessRatio > 0) then 
                ! water did not fully flow to cell A.
                !$OMP ATOMIC
                SI0_state(jjA,iiA) = SI0_state(jjA,iiA) + RI * (1 - excessRatio)
                ! part of water still in cell B
                !$OMP ATOMIC
                SI0_state(jjB,iiB) = SI0_state(jjB,iiB) + RI * excessRatio
            else 
                ! fully passing route 
                !$OMP ATOMIC
                SI0_state(jjA,iiA) = SI0_state(jjA,iiA) + RI
            end if 
            
        end if 

        
        return
    end subroutine subflowRoute

    
    !---------------------------< subsurface routing >--------------------
    subroutine streamRouting(j, i, RRiver,  routPassRiver, sRiver0_state)
        
        
        integer, intent(in)    :: j, i
        integer :: iiA, jjA,  iiB, jjB
        double precision, intent(in) :: RRiver
        double precision, intent(inout) :: routPassRiver(0:, 0:)  
        double precision, intent(inout) :: sRiver0_state(0:, 0:)  

        double precision :: speedFlowLoacl, speedHillslope, speedStream
        double precision :: timeStream, timeStream_tot
        double precision :: excessRatio
        
        iiA = g_NextR(j,i)
        jjA = g_NextC(j,i)

        iiB = i
        jjB = j
        
        timeStream_tot = 0
        excessRatio = 0

        do while(timeStream_tot < g_TimeStepToSeconds)
            
            
            
            speedFlowLoacl = 0.2
            ! m/s 
            speedHillslope = g_tParams%coeM(jjB,iiB) * speedFlowLoacl &
                        *g_Slope(jjB,iiB)**g_tParams%expM(jjB,iiB)
            
            speedStream = speedHillslope * g_tParams%coeR(j,i) 
            
            
            ! unit: s
            timeStream = g_NextLen(j,i) / speedStream
            
            timeStream_tot = timeStream_tot + timeStream
            
            if( g_Mask(jjA,iiA) /= g_NoData_Value) then
                
                 ! next cell still in basin, do routing 
                
                if (g_Stream(jjA,iiA) /= 1) then 
                    print *, 'error: River routing outside the stream grid.'
                    write(g_CREST_LogFileID,*) 'error: River routing outside the stream grid.'
                    stop 1
                end if 
                
                ! calculate the water allocation to next cell
                ! This means that within the time step, the water from cell B & 
                ! did not fully flow to cell A.
                if (timeStream_tot >= g_TimeStepToSeconds) then 
                    !
                    
                    excessRatio = (timeStream_tot - g_TimeStepToSeconds) / timeStream
                    
                    
                    if (excessRatio > 1) then 
                        print *, 'error with stream flow excessRatio'
                        stop 1
                    end if  
                    
                    routPassRiver(jjA, iiA) = routPassRiver(jjA, iiA) + RRiver * (1 - excessRatio)
                    
                    
                else 
                    ! fully passing route 
                    
                    routPassRiver(jjA, iiA) = routPassRiver(jjA, iiA) + RRiver
                end if 
                
            else 
                ! (jjA, iiA) is outside of the basin
                ! (jjB, iiB) is in the basin; (jjB, iiB) is (g_tOutlet % Col, g_tOutlet % Row)
                exit ! stop routing; out of basin
            end if
            
            ! index for next cell 
            iiB = iiA
            jjB = jjA
            iiA = g_NextR(jjB, iiB)
            jjA = g_NextC(jjB, iiB)


        end do 
        
        
        ! update the river state 
        if (g_Mask(jjA,iiA) /= g_NoData_Value) then 
            ! the downstream cell is still in basin after loop 
            ! thus, (jjA,iiA) is the destination

            ! if g_Stream(jjA,iiA) == 1, (jjA,iiA) is not the destination
            if (excessRatio > 0) then 
                ! water did not fully flow to cell A.
                !$OMP ATOMIC
                sRiver0_state(jjA,iiA) = sRiver0_state(jjA,iiA) + RRiver * (1 - excessRatio)
                ! part of water still in cell B
                !$OMP ATOMIC
                sRiver0_state(jjB,iiB) = sRiver0_state(jjB,iiB) + RRiver * excessRatio
            else 
                ! fully passing route 
                !$OMP ATOMIC
                sRiver0_state(jjA,iiA) = sRiver0_state(jjA,iiA) + RRiver
            end if 
            
        end if 

        return
    end subroutine streamRouting

    


end module linearRoute