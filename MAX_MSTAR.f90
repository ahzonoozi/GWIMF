SUBROUTINE MAX_MSTAR(Mecl1,nx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,m_max1)

  INTEGER :: j
  INTEGER :: nx
  REAL, ALLOCATABLE, DIMENSION(:) :: m,dm,logM
  REAL :: SFR,FeH,M_C
  REAL :: Bt,delta,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,alpha_3,k1,k2,k3
  REAL :: m_max1,xx1,Mecl1

  CALL INPUT(nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2,TYPE_IGIMF)  
  ALLOCATE(m(1:nx+1),dm(1:nx+1),logM(1:nx+1)) 
           m=0.0; dm=0.0; logM=0.0
     

  logM(1)=-1.08
  m(1)=10.0**(logM(1))  
             
  xx1=0.99*(0.61*log10(Mecl1)+2.85-6.0)-0.14*FeH
  IF (xx1.GE.-0.87 )THEN
     alpha_3=(1.0*(1.94-0.41*xx1))
  ELSEIF(xx1.LT.-0.87)THEN
     alpha_3=2.3
  ENDIF

  IF(TYPE_IGIMF.EQ.1)then
     alpha_3=1.30
  ENDIF

  M_C=0.0     
  DO j=1,400

     logM(j)=-1.08+0.01*(j-1)  
     m(j)=10.0**(logM(j))
     dm(j)=m(j)-m(j-1)
     M_C=0.0                  


     IF (m(j).GE.0.08.AND.m(j).LT.0.5) THEN
        K1=1./((M_turn1**(1.-alpha_1)-m(j)**(1.-alpha_1))/(1.-alpha_1)+(M_turn2**(1.-alpha_2)-&
           &M_turn1**(1.-alpha_2))*(M_turn1**(alpha_2-alpha_1))/(1.-alpha_2)+(M_U**(1.-alpha_3)-&
           &M_turn2**(1.-alpha_3))*(M_turn1**(alpha_2-alpha_1))*(M_turn2**(alpha_3-alpha_2))/(1.-alpha_3)) 
                   
        M_C=K1*(m(j)**(2.-alpha_1)-M_L**(2.-alpha_1))/(2.-alpha_1)              
     ENDIF 
     IF (m(j).GE.0.5.AND.m(j).LT.1.0) THEN
        K1=1./( (M_turn2**(1.-alpha_2)-m(j)**(1.-alpha_2))*(M_turn1**(alpha_2-alpha_1))/(1.-alpha_2)+&
            &(M_U**(1.-alpha_3)-M_turn2**(1.-alpha_3))*(M_turn1**(alpha_2-alpha_1))*&
            &(M_turn2**(alpha_3-alpha_2))/(1.-alpha_3))
                   
        M_C=K1*((M_turn1**(2.-alpha_1)-M_L**(2.-alpha_1))/(2.-alpha_1)+(m(j)**(2.-alpha_2)-&
            &M_turn1**(2.-alpha_2))*(M_turn1**(alpha_2-alpha_1))/(2.-alpha_2))             
     ENDIF 
     IF (m(j).GE.1..AND.m(j).LT.150.) THEN
        K1=(M_turn1**(alpha_1-alpha_2))*(M_turn2**(alpha_2-alpha_3))*(1.-alpha_3)/&
            &(M_U**(1.-alpha_3)-m(j)**(1.-alpha_3))
                   
        M_C=K1*((M_turn1**(2.-alpha_1)-M_L**(2.-alpha_1))/(2.-alpha_1)+(M_turn2**(2.-alpha_2)-&
            &M_turn1**(2.-alpha_2))*(M_turn1**(alpha_2-alpha_1))/(2.-alpha_2)+(M(j)**(2.-alpha_3)-&
            &M_turn2**(2.-alpha_3))*(M_turn2**(alpha_3-alpha_2))*(M_turn1**(alpha_2-alpha_1))/(2.-alpha_3))             
     ENDIF 
      
 
                   
     IF(M_C.GE.Mecl1) GOTO 20 
       
    
  ENDDO

20 m_max1=m(j)
   

  IF (M_C.LT.Mecl1)THEN
     m_max1=150.
  ENDIF

  WRITE(500,*)Mecl1,m_max1 
  RETURN   
END  



