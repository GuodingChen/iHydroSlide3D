subroutine CalSlope_Tiger(bIsError)
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none
    integer :: i,j,ii,jj
    double precision :: GM(0:g_NCols-1,0:g_NRows-1)
    double precision :: GMValue,GMValueIn

    logical :: fExist,bIsError
    integer :: fileid
    character(len=200):: fileName
    double precision :: dblTemp
    fileName=trim(g_BasicPath) // "Slope.def"

    bIsError=.false.
    inquire(file=trim(Filename), exist=fExist)

    if(fExist .eqv. .false.)then
        bIsError=.true.
        return
    end if
    call XXWGetFreeFile(fileid)
    open(fileid,file=TRIM(Filename), form="formatted")
    read(fileid,*)GMValue
    close(fileid)

    ! 		GMValue=1.185728
    GM=GMValue

    do i=0, g_NRows-1
        do j=0, g_NCols-1
            ii=g_NextR(j,i)
            jj=g_NextC(j,i)

            if(g_Mask(j,i)/=g_NoData_Value)then
                if(InBasin(jj,ii) .eqv. .false.)then
                    g_Slope(j,i)=GM(j,i)/g_NextLen(j,i)
                else
                    if(g_DEM(j,i)>g_DEM(jj,ii))then
                        g_Slope(j,i)=(g_DEM(j,i)-g_DEM(jj,ii))  &
                                /g_NextLen(j,i)
                    else
                        g_Slope(j,i) &
                                =GM(j,i)/g_NextLen(j,i)
                    end if
                end if
            else
                g_Slope(j,i)=g_NoData_Value
            end if
        end do
    end do

    return
end subroutine

!------------------------------------------------------------
subroutine CalSlope()
    use CREST_Project
    use CREST_Basic
    use CREST_Param

    implicit none
    integer :: i,j,ii,jj
    double precision,external :: XXWCell_i_j_Slope_All

    do i=0, g_NRows-1
        do j=0, g_NCols-1

            if(g_Mask(j,i)/=g_NoData_Value)then
                ii=g_NextR(j,i)
                jj=g_NextC(j,i)
                if(InBasin(jj,ii) .eqv. .false.)then
                    g_Slope(j,i)=XXWCell_i_j_Slope_All(i,j)

                else
                    if(g_DEM(j,i)>g_DEM(jj,ii))then
                        g_Slope(j,i)=(g_DEM(j,i)-g_DEM(jj,ii)) &
                                /g_NextLen(j,i)
                    else ! If the direction map is correct, this condition will not happen
                        g_Slope(j,i)=XXWCell_i_j_Slope_All(i,j)
                    end if
                end if
                if(g_Slope(j,i)<0)then
                    g_Slope(j,i)=-g_Slope(j,i)
                end if
                if(abs(g_Slope(j,i)-0.0)<0.000001)then
                    g_Slope(j,i)=0.000001
                end if
            else
                g_Slope(j,i)=g_NoData_Value
            end if

        end do
    end do

    return
end subroutine CalSlope
!------------------------------------------------------------

double precision Function XXWCell_i_j_Slope_All(i,j)
    use CREST_Project
    use CREST_Basic
    implicit none

    integer :: i,j
    double precision :: LenSN, LenEW
    double precision, external :: XXWCell_i_j_Slope
    logical :: bGCS

    if(g_XLLCorner>=-180.0 .and. g_XLLCorner<=180 &
            .and. g_YLLcorner>=-90 .and. g_YLLCorner<=90)then
        bGCS=.true.
    else
        bGCS=.false.
    end if
    if (trim(g_CS) == "GCS") then
        bGCS=.true.
    else
        bGCS = .false.
    end if



    if(bGCS .eqv. .true.)then
        LenSN=g_CellSize*110574.0
    else
        LenSN=g_CellSize
    end if

    if(bGCS .eqv. .true.)then
        LenEW=g_YLLCorner+(g_NRows-i-0.5)*g_CellSize
        LenEW=LenSN*cos(LenEW*4.0*atan(1.0)/180.0)
    else
        LenEW=g_CellSize
    end if

    XXWCell_i_j_Slope_All=XXWCell_i_j_Slope(g_NCols,g_NRows,i,j, &
            g_CellSize, g_NoData_Value,g_DEM,LenSN, LenEW)
end function XXWCell_i_j_Slope_All


double precision Function XXWCell_i_j_Slope(NCols,NRows,i,j, &
        CellSize, NoData_Value,dblDEMMat,LenSN, LenEW)
    implicit none

    integer :: NCols,NRows,i,j
    double precision :: CellSize, NoData_Value
    double precision :: dblDEMMat(0:NCols-1,0:NRows-1)
    double precision :: aa,bb,cc,dd,ee,ff,gg,hh,ii
    double precision :: LenSN, LenEW,dzdx,dzdy

    if(dblDEMMat(j,i)==NoData_Value)then
        XXWCell_i_j_Slope=NoData_Value
        return
    end if

    ee=dblDEMMat(j,i)

    if(i==0)then
        if(j==0)then
            aa=ee
            bb=ee
            cc=ee
            dd=ee

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        elseif(j==NCols-1)then
            aa=ee
            bb=ee
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=ee
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=ee
        else
            aa=ee
            bb=ee
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=dblDEMMat(j+1,i)
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        end if

    elseif(i==NRows-1)then
        if(j==0)then
            aa=ee
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=ee

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=ee
            ii=ee
        elseif(j==NCols-1)then
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=ee
            gg=ee
            hh=ee
            ii=ee
        else
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=dblDEMMat(j-1,i)

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=ee
            ii=ee
        end if
    else
        if(j==0)then
            aa=ee
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=ee

            ff=dblDEMMat(j+1,i)
            gg=ee
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        elseif(j==NCols-1)then
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=ee
            dd=dblDEMMat(j-1,i)

            ff=ee
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=ee
        else
            aa=dblDEMMat(j-1,i-1)
            bb=dblDEMMat(j,i-1)
            cc=dblDEMMat(j+1,i-1)
            dd=dblDEMMat(j-1,i)

            ff=dblDEMMat(j+1,i)
            gg=dblDEMMat(j-1,i+1)
            hh=dblDEMMat(j,i+1)
            ii=dblDEMMat(j+1,i+1)
        end if
    end if

    if(aa==NoData_Value)then
        aa=ee
    end if

    if(bb==NoData_Value)then
        bb=ee
    end if

    if(cc==NoData_Value)then
        cc=ee
    end if

    if(dd==NoData_Value)then
        dd=ee
    end if

    if(ee==NoData_Value)then
        ee=ee
    end if

    if(ff==NoData_Value)then
        ff=ee
    end if

    if(gg==NoData_Value)then
        gg=ee
    end if

    if(hh==NoData_Value)then
        hh=ee
    end if

    if(ii==NoData_Value)then
        ii=ee
    end if

    dzdx=(cc+2.0*ff+ii)-(aa+2.0*dd+gg)
    dzdx=dzdx/(8.0*LenEW)

    dzdy=(gg+2.0*hh+ii)-(aa+2.0*bb+cc)
    dzdy=dzdy/(8.0*LenSN)

    XXWCell_i_j_Slope=sqrt(dzdx**2+dzdy**2)
    ! 		XXWCell_i_j_Slope=XXWCell_i_j_Slope*4.0*atan(1.0)/180.0
end function XXWCell_i_j_Slope



!########################################################

subroutine ConvDDMToFDR()
    use CREST_Project
    use CREST_Basic
    implicit none

    integer :: i,j,ii,jj
    integer :: numNoData,iRowNoData,iColNoData

    do i=0,g_NRows-1
        do j=0,g_NCols-1
            if(g_FDR(j,i)==g_NoData_Value)then
                cycle
            end if
            select case(g_FDR(j,i))
            case(1)
                g_FDR(j,i)=64
            case(2)
                g_FDR(j,i)=128
            case(8)
                g_FDR(j,i)=32
            case(3)
                g_FDR(j,i)=1
            case(4)
                g_FDR(j,i)=2
            case(5)
                g_FDR(j,i)=4
            case(6)
                g_FDR(j,i)=8
            case(7)
                g_FDR(j,i)=16
            case(0) !Outlet location
                numNoData=0
                iRowNoData=g_NoData_Value
                iColNoData=g_NoData_Value
                do ii=-1,1
                    do jj=-1,1
                        if((ii==0).and.(jj==0))then
                            cycle
                        end if
                        if(InBasin(j+jj,i+ii))then
                            if(g_FDR(j+jj,i+ii)/=g_NoData_Value)then
                            else
                                numNoData=numNoData+1
                                iRowNoData=ii
                                iColNoData=jj
                            end if
                        else
                            numNoData=numNoData+1
                            iRowNoData=ii
                            iColNoData=jj
                        end if
                    end do
                end do
                if(numNoData==0)then
                    g_FDR(j,i)=256 ! This cell is a sink
                elseif(numNoData==8)then
                    g_FDR(j,i)=1 ! This cell is an island
                else
                    if(iRowNoData==-1)then
                        if(iColNoData==-1)then
                            g_FDR(j,i)=32
                        elseif(iColNoData==0)then
                            g_FDR(j,i)=64
                        else
                            g_FDR(j,i)=128
                        end if
                    elseif(iRowNoData==0)then
                        if(iColNoData==-1)then
                            g_FDR(j,i)=16
                        elseif(iColNoData==0)then
                            write(*,*)"Wrong!!!!!!!!!!"
                        else
                            g_FDR(j,i)=1
                        end if
                    else
                        if(iColNoData==-1)then
                            g_FDR(j,i)=8
                        elseif(iColNoData==0)then
                            g_FDR(j,i)=4
                        else
                            g_FDR(j,i)=2
                        end if
                    end if
                end if
            end select
        end do
    end do
    return
end subroutine
!########################################################


subroutine InitParamsType(tParams)
    use CREST_Basic
    use CREST_Param
    implicit none
    type(CREST_Params) tParams

    allocate(tParams%RainFact(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%Ksat(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%WM(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%B(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%IM(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%KE(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%coeM(0:g_NCols-1,0:g_NRows-1))

    allocate(tParams%expM(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%coeR(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%coeS(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%KS(0:g_NCols-1,0:g_NRows-1))
    allocate(tParams%KI(0:g_NCols-1,0:g_NRows-1))

    tParams%RainFact=g_NoData_Value
    tParams%Ksat=g_NoData_Value
    tParams%WM=g_NoData_Value
    tParams%B=g_NoData_Value
    tParams%IM=g_NoData_Value
    tParams%KE=g_NoData_Value
    tParams%coeM=g_NoData_Value

    tParams%expM=g_NoData_Value
    tParams%coeR=g_NoData_Value
    tParams%coeS=g_NoData_Value
    tParams%KS=g_NoData_Value
    tParams%KI=g_NoData_Value

    return
end subroutine





SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,IOUT,IN)

    CHARACTER*(*) LINE
    CHARACTER*20 STRING
    CHARACTER*30 RW
    CHARACTER*1 TAB
    integer :: N,IOUT,IN
    !     ------------------------------------------------------------------
    TAB=CHAR(9)
    !

    LINLEN=LEN(LINE)
    LINE(LINLEN:LINLEN)=' '
    ISTART=LINLEN
    ISTOP=LINLEN
    LINLEN=LINLEN-1
    IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
    !
    !2------Find start of word, which is indicated by first character that
    !2------is not a blank, a comma, or a tab.
    DO 10 I=ICOL,LINLEN
        IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' &
                .AND. LINE(I:I).NE.TAB) GO TO 20
    10 CONTINUE
    ICOL=LINLEN+1
    GO TO 100
    !
    !3------Found start of word.  Look for end.
    !3A-----When word is quoted, only a quote can terminate it.
    20 IF(LINE(I:I).EQ.'''') THEN
        I=I+1
        IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
                IF(LINE(J:J).EQ.'''') GO TO 40
            25    CONTINUE
        END IF
        !
        !3B-----When word is not quoted, space, comma, or tab will terminate.
    ELSE
        DO 30 J=I,LINLEN
            IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' &
                    .OR. LINE(J:J).EQ.TAB) GO TO 40
        30  CONTINUE
    END IF
    !
    !3C-----End of line without finding end of word; set end of word to
    !3C-----end of line.
    J=LINLEN+1
    !
    !4------Found end of word; set J to point to last character in WORD and
    !-------set ICOL to point to location for scanning for another word.
    40 ICOL=J+1
    J=J-1
    IF(J.LT.I) GO TO 100
    ISTART=I
    ISTOP=J
    !
    !5------Convert word to upper case and RETURN if NCODE is 1.
    IF(NCODE.EQ.1) THEN
        IDIFF=ICHAR('a')-ICHAR('A')
        DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z') &
                    LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
        50  CONTINUE
        RETURN
    END IF
    !
    !6------Convert word to a number if requested.
    100 IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
        RW=' '
        L=30-ISTOP+ISTART
        IF(L.LT.1) GO TO 200
        RW(L:30)=LINE(ISTART:ISTOP)
        IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
        IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
    END IF
    RETURN
    !
    !7------Number conversion error.
    200 IF(NCODE.EQ.3) THEN
        STRING= 'A REAL NUMBER'
        L=13
    ELSE
        STRING= 'AN INTEGER'
        L=10
    END IF
    !
    !7A-----If output unit is negative, set last character of string to 'E'.
    IF(IOUT.LT.0) THEN
        N=0
        R=0.
        LINE(LINLEN+1:LINLEN+1)='E'
        RETURN
        !
        !7B-----If output unit is positive; write a message to output unit.
    ELSE IF(IOUT.GT.0) THEN
        IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
        ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
        END IF
        201 FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A, &
                '" TO ',A,' IN LINE:',/1X,A)
        202 FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A, &
                '" TO ',A,' IN LINE:',/1X,A)
        !
        !7C-----If output unit is 0; write a message to default output.
    ELSE
        IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
        ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
        END IF
    END IF
    !
END
SUBROUTINE UPCASE(WORD)
    !     ******************************************************************
    !     CONVERT A CHARACTER STRING TO ALL UPPER CASE
    !     ******************************************************************
    !       SPECIFICATIONS:
    !     ------------------------------------------------------------------
    CHARACTER WORD*(*)
    !
    !1------Compute the difference between lowercase and uppercase.
    L = LEN(WORD)
    IDIFF=ICHAR('a')-ICHAR('A')
    !
    !2------Loop through the string and convert any lowercase characters.
    DO 10 K=1,L
        IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z') &
                WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
    10 CONTINUE
    !
    !3------return.
    RETURN
END
!########################################################


subroutine GetMaskByNoData(NCols,NRows,NoData_Value, &
        DEM,MaskOut)
    implicit none
    integer :: NCols,NRows
    double precision :: NoData_Value
    double precision :: DEM(0:NCols-1,0:NRows-1)
    integer, intent(out) :: MaskOut(0:NCols-1,0:NRows-1)
    integer :: i,j

    do i=0, NRows-1
        do j=0, NCols-1
            if(DEM(j,i)   ==  NoData_Value)then
                MaskOut(j,i)    =   NoData_Value
            else
                MaskOut(j,i)=1
            end if
        end do
    end do
    return
end subroutine
!########################################################


!########################################################
logical function InBasin(jCol,iRow)
    use CREST_Basic
    implicit none
    integer :: iRow,jCol
    if(jCol<0 .or. jCol>=g_NCols .or. iRow<0 .or. iRow>=g_NRows)then
        InBasin=.false.
    else
        ! 			if(g_Mask(jCol,iRow)==g_NoData_Value)then
        ! 				InBasin=.false.
        ! 			else
        InBasin=.true.
        ! 			end if
    end if
    return
end function
