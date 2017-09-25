      SUBROUTINE CROSS (A,B,AXB)
C=======================================================================
C  Returns vector cross product AXB between vectors A, B
C-----------------------------------------------------------------------
      REAL   A(3),B(3),AXB(3)
      AXB(1)=A(2)*B(3)-A(3)*B(2)
      AXB(2)=A(3)*B(1)-A(1)*B(3)
      AXB(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END
