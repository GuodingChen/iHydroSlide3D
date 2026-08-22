

!########################################################
subroutine CREST_EAct(W0,P,EPot,W,EAct)

    implicit none

    double precision,intent(in) :: W0,P,EPot,W
    double precision,intent(out) :: EAct


    if(P>EPot)then
        EAct=EPot
    else !P<=EPot
        EAct=W0-W
    end if



    return
end subroutine


subroutine CREST_EPotential(E,EFact,EPot)

    implicit none

    double precision :: E,EFact
    double precision :: EPot

    EPot=E*EFact

    return
end subroutine


!########################################################
subroutine CREST_PrecipInt(P,PFact,PInt)

    implicit none

    double precision :: P,PFact
    double precision :: PInt

    PInt=P*PFact

    return
end subroutine CREST_PrecipInt


