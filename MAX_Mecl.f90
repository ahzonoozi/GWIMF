SUBROUTINE Max_Mecl(nx,SFR,FeH,Mecl_max)

  INTEGER :: f
  INTEGER :: nx
  REAL, ALLOCATABLE, DIMENSION(:) :: Mecl
  REAL :: Bt,Mecl_u,Mecl_l,Kecl,SFR,Mecl_max,FeH,Mtot,M_t


  CALL INPUT(nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2,TYPE_IGIMF) 
  ALLOCATE(Mecl(1:nx+1))
           Mecl=0.0
 
!-----------------------------------------------------
  
  Bt =-0.106*log10(SFR)+2. 
  Mecl(1)=5.0      
  M_t=0.
  Mtot=SFR*10.**7.  !Mtot=SFR*dt
  Mecl_u=10.**9.
  Mecl_l=5.
       
   

  DO f=2,900
     M_t=0.
     Mecl(f)=Mecl_l+10**(0.01*f)
     IF (Bt==2.)THEN
        M_t=(log(Mecl(f))-log(Mecl_l))/(Mecl(f)**(-1.)-Mecl_u**(-1.))
     ELSE  
        M_t=((1.-Bt)/(2.-Bt))*(Mecl(f)**(2.-Bt)-Mecl_l**(2.-Bt))/(Mecl_u**(1.-Bt)-Mecl(f)**(1.-Bt))
     ENDIF

     IF (M_t.GE.Mtot) GOTO 10

  ENDDO

10   Mecl_max=Mecl(f)
     IF(M_t.LT.Mtot)THEN
        Mecl_max=1.e9
     ENDIF

     Kecl=Mtot*(-Bt+2.)/(Mecl_max**(-Bt+2.)-Mecl_L**(-Bt+2.0))
     
     !WRITE(800,*)Mecl_max,Kecl,SFR
     !CLOSE(800)
 
  RETURN
END

