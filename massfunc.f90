SUBROUTINE massfunc(Mecl_max,nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2)

  INTEGER :: i,j,f
  INTEGER :: nx, mx
  REAL, ALLOCATABLE, DIMENSION(:) :: m,dm,logM
  REAL, ALLOCATABLE, DIMENSION(:) :: Mecl,dMecl,Meclave,m_max
  REAL, ALLOCATABLE, DIMENSION(:) :: IMF,phi,IGIMF,xx
  REAL :: SFR,FeH,Mecl_max,M_t,Nt_IGIMF,Nt_IMF,Mt_IMF,Mt_IGIMF
  REAL :: M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,k1,k2,k3,k_1,k_2,alpha_3,a_1,a_2
  REAL :: Bt,delta,Kecl,Mecl1,m_max1

  CALL input(nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2,TYPE_IGIMF)  
  ALLOCATE(m(1:nx+1),dm(1:nx+1),logM(1:nx+1)) 
           m=0.0; dm=0.0; logM=0.0
  ALLOCATE(Mecl(1:nx+1),dMecl(1:nx+1),Meclave(1:nx+1),m_max(1:nx+1),xx(1:nx+1))
           Mecl=0.0; dMecl=0.0; Meclave=0.0; m_max=0.0; xx=0.0
  ALLOCATE(IMF(1:nx+1),phi(1:nx+1),IGIMF(1:nx+1))
           IMF=0.0; phi=0.0; IGIMF=0.0

  WRITE(500,*) '#','M_ecl','   ','m_max' 

   Nt_IGIMF=0.0
   Nt_IMF=0.0
   logM(1)=-1.08
   m(1)=10.0**(logM(1)) 
   Bt =-0.106*log10(SFR)+2. 
   Mecl(1)=5.0 
   Mt_IGIMF=0.0
   Mt_IMF =0.0      
   
   
!==================================================================

  delta=(log10(Mecl_max)-log10(5.))/mx  
             
  DO f=2,mx+2
 
     Mecl(f)=10.0**(delta*f+log10(5.))
     Mecl1=Mecl(f)

     CALL max_mstar(Mecl1,nx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,m_max1)
     m_max(f)=m_max1
     !WRITE(*,*)Mecl(f),m_max(f)
  ENDDo
!====================================================================
   
  DO i=2, 400 
         
     IGIMF(i)=0.0
     phi(i)=0.0
     IMF(i)=0.0
     logM(i)=-1.08+0.01*(i-1)  !-1.08+0.02*i
     m(i)=10.0**(logM(i))
     dm(i)=m(i)-m(i-1)

     k_2= 1./( 2.*(M_turn1**(-a_1+2.)-M_L**(-a_1+2.))/(-a_1+2.)+(M_U**(-a_2+2.)-M_turn1**(-a_2+2.))/(-a_2+2.)  )
     K_1=k_2*M_turn1**(a_1-a_2)

     IF (m(i).GE.0.08.AND.m(i).LT.0.5) THEN
        IMF(i)=k_1*m(i)**(-a_1)
     ENDIF                                       
     IF (m(i).GE.0.5.AND.m(i).LE.150.0) THEN
        IMF(i)= k_2*m(i)**(-a_2)    
     ENDIF 
             
                   
         Nt_IMF=IMF(i)*dm(i)+Nt_IMF
         Mt_IMF =IMF(i)*dm(i)*m(i)+Mt_IMF  
               
         delta=(log10(Mecl_max)-log10(5.))/mx   

           
     Do f=2,mx+2       
        Mecl(f)=10.0**(delta*f+log10(5.))    
        IF (Mecl(f).LE.Mecl_max)THEN
           Meclave(f)=(Mecl(f)+Mecl(f-1))/2.0
           xx(f)=0.99*(0.61*log10(Mecl(f))+2.85-6.0)-0.14*FeH
           dMecl(f)=Mecl(f)-Mecl(f-1)      
                     
           IF (xx(f).GE.-0.87 )THEN
              alpha_3=(1.0*(1.94-0.41*xx(f)))
           ELSEIF(xx(f).LT.-0.87)THEN
              alpha_3=2.3
           ENDIF

           IF(TYPE_IGIMF.EQ.1)then
              alpha_3=1.30
           ENDIF

           K1=Mecl(f)/((M_turn1**(-alpha_1+2.)-M_L**(-alpha_1+2.))/(-alpha_1+2.)+(M_turn2**(-alpha_2+2.)-&
              &M_turn1**(-alpha_2+2.))*(M_turn1**(alpha_2-alpha_1))/(-alpha_2+2.)+(m_max(f)**(-alpha_3+2.)&
              &-M_turn2**(-alpha_3+2.))*(M_turn2**(alpha_3-alpha_2))*(M_turn1**(alpha_2-alpha_1))/(-alpha_3+2.)) 
           K2=K1*(M_turn1**(alpha_2-alpha_1))
           K3=K2*(M_turn2**(alpha_3-alpha_2))
                  
           IF (m(i).GE.0.08 .and. m(i).LT.0.5) THEN
              phi(i)=K1*m(i)**(-alpha_1) 
           ENDIF                                       
           IF (m(i).GE.0.5 .and. m(i).LT.1.0) THEN
              phi(i)=K2*m(i)**(-alpha_2) 
           ENDIF 
           IF (m(i).GE.1.0 .and. m(i).LE.150.0) THEN 
              phi(i)=K3*m(i)**(-alpha_3)    
           ENDIF   
       

           IF (m(i).LE. m_max(f)) THEN
              IGIMF(i)=phi(i)*((Meclave(f))**(-Bt))*dMecl(f)+IGIMF(i)
           ELSEIF(m(i).GT.m_max(f)) THEN     
              IGIMF(i)=0.0     
           ENDIF
            
        ENDIF
     ENDDO
           
    
     Nt_IGIMF=IGIMF(i)*dm(i)+Nt_IGIMF
     Mt_IGIMF=IGIMF(i)*dm(i)*m(i)+Mt_IGIMF
             

  ENDDO

  WRITE(200,*) '#','masses[M_{sun}]', '       ','IGIMF-M'
  WRITE(240,*) '#','masses[M_{sun}]', '       ','IMF-M'  

  DO i=2,400

     IF (m(i).LE.150.0)THEN
        WRITE(200,*) m(i),IGIMF(i)/Mt_IGIMF 
        WRITE(240,*) m(i),IMF(i)/Mt_IMF 
     ENDIF
  ENDDO

  CLOSE(200)
  CLOSE(500)
  CLOSE(240)
   
  RETURN
END


