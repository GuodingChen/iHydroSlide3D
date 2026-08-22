
subroutine CREST_RunoffGen(W0,P,EPot,WM,IM,B,Ksat,W,ExcS,ExcI)

    implicit none
    double precision,intent(in) :: W0,P,EPot,WM,B,Ksat
    double precision,intent(out) :: W,ExcS,ExcI
    double precision :: IM
    double precision :: PSoil,R,temX,WMM,A
    
    if (abs(IM - 1.0) < 1.0e-6) then
        IM = 1.0d0 - 1.0d-3
    end if 

    if(P>EPot)then
        ! Calculate part of precip that goes into soil
        PSoil=(P-EPot)*(1.0-IM)

        if(W0<WM)then
            WMM=WM*(1.0+B)
            A=WMM*(1.0-(1.0-W0/WM)**(1.0/(1.0+B)))
            if(PSoil+A>=WMM)then
                R=PSoil-(WM-W0)
                W=WM
            else
                R=PSoil-WM*((1.0-A/WMM)**(1.0+B) &
                        -(1.0-(A+PSoil)/WMM)**(1.0+B))
                if(R<0.0)then
                    R=0.0
                end if
                W=W0+PSoil-R
            end if
        else ! W0>WM   the soil is full
            R=PSoil
            W=W0
        end if
        ! Calculate how much water can infiltrate
        temX=((W0+W)/2.0)*Ksat/WM
        if(R<=temX)then
            ExcI=R
        else
            ExcI=temX
        end if
        ExcS=R-ExcI+(P-EPot)*IM
        if (ExcS<0) then !Added by Dr.Xianwu Xue 2011.5.13
            ExcS=0.0
        end if
    else !P<=EPot
        ExcS = 0.0
        ExcI = 0.0
        temX = (EPot - P) * W0 / WM
        ! temX = EPot - P
        if(temX < W0) then
            W = W0 - temX
        else
            W = 0.0
        end if
    end if



    return
end subroutine CREST_RunoffGen
