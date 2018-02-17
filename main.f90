PROGRAM GWIMF

   IMPLICIT NONE
   INTEGER :: i,j,f,k
   INTEGER :: nx,mx,TYPE_IGIMF
   REAL, ALLOCATABLE, DIMENSION(:) :: m,dm
   REAL, ALLOCATABLE, DIMENSION(:) :: Mecl,dMecl,Meclave,m_max,IMF,logM
   REAL, ALLOCATABLE, DIMENSION(:) :: phi,IGIMF,N,NN,xx
   REAL :: N_IGIMF,N_IMF,SFR,Mecl_max,FeH,const,M_C,Mtot,M_t,xx1
   REAL :: Bt,delta,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,k1,k2,k3,k_1,k_2,a3,a_1,a_2,Mecl_u,Mecl_l,Mecl1,m_max1

  CALL input(nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2,TYPE_IGIMF)
  CALL output(FeH,SFR)

   ALLOCATE(m(1:nx+1),dm(1:nx+1),logM(1:nx+1)) 
      m=0.0; dm=0.0; logM=0.0
   ALLOCATE(Mecl(1:nx+1),dMecl(1:nx+1),Meclave(1:nx+1),m_max(1:nx+1),IMF(1:nx+1))
      Mecl=0.0; dMecl=0.0; Meclave=0.0; m_max=0.0; IMF=0.0
   ALLOCATE(phi(1:nx+1),IGIMF(1:nx+1),N(1:nx+1),NN(1:nx+1),xx(1:nx+1))
      phi=0.0; IGIMF=0.0; N=0.0; NN=0.0; xx=0.0

  CALL max_mecl(nx,SFR,FeH,Mecl_max)
  CALL massfunc(Mecl_max,nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2)
  
 
END PROGRAM GWIMF
!###################################################################################################
SUBROUTINE output(FeH,SFR)
  REAL SFR,FeH

CHARACTER(LEN=45) :: OUTPUT_FILE
CHARACTER(LEN=7)  :: n2        !SFR
CHARACTER(LEN=4)  :: n4        !Fe/H 


                   WRITE(n2,'(ES7.1)')SFR
		   WRITE(n4,'(F4.1)')FeH
		   OUTPUT_FILE='SFR'//n2//'FeH'//n4//'.txt' 
   


  OPEN(200,FILE=OUTPUT_FILE) 
  OPEN(240,file='canonic-IMF.txt')
  OPEN(500,FILE='mmax-Mecl.txt')
   
RETURN
END


!###################################################################################################
subroutine input(nx,mx,SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2,TYPE_IGIMF)
  IMPLICIT NONE

     REAL SFR,FeH,M_L,M_turn1,M_turn2,M_U,alpha_1,alpha_2,a_1,a_2
     INTEGER i,nx,mx,TYPE_IGIMF
     open(10,file='input')

      READ(10,*)SFR,FeH
      READ(10,*)nx,mx
      READ(10,*)M_L,M_turn1,M_turn2,M_U
      READ(10,*)a_1,a_2
      READ(10,*)TYPE_IGIMF
      
      IF(TYPE_IGIMF.EQ.3)then
        alpha_1=1.30+0.5*FeH
        alpha_2=2.30+0.5*FeH
      ENDIF
      IF(TYPE_IGIMF.EQ.2)then
        alpha_1=1.30
        alpha_2=2.30
      ENDIF
      IF(TYPE_IGIMF.EQ.1)then
        alpha_1=1.30
        alpha_2=2.30
      ENDIF


    CLOSE(10)
RETURN
END







