C Fortran program to simulate Game of Life
      PROGRAM Gameoflife
        integer fdin, fdout, gen, row, col, stillgen, i, ios
        character*90 name
        character*80 pattern1(100), pattern2(100)
        logical still
        fdin = 1
        fdout = 2
        call infopen(fdin)
        read(fdin,'(A)') name
        read(fdin,*) gen
        read(fdin,*) row, col

C       read pattern from input file
        i = 1
   10   if ( i .GT. row ) GO TO 20
          read(fdin,'(A)') pattern2(i)
          i = i + 1
          GO TO 10

C       simulate the generations
   20   i = 1
   30   if ( i .GT. (gen+1) ) GO TO 40
          still = .TRUE.
          call copy(row, col, pattern2, pattern1)
          call generate(row,col,pattern1,pattern2,still,stillgen)
          if ( still ) GO TO 40
          i = i + 1
          GO TO 30

C       write result pattern into file
   40   stillgen = i - 1
        name = name(1:(length(name)-1)) // 'for.txt'
        open(unit=fdout, file=name, iostat=ios)
        if ( ios .NE. 0 ) stop "Error opening file"

        i = 1
   50   if ( i .GT. row ) GO TO 60
          write(fdout, '(A,A)')
     +    pattern1(i)(1:length(pattern1(i))), char(13)
          i = i + 1
          GO TO 50

C       write result statement into file
   60   if ( .NOT. still .AND. (gen .EQ. 1) ) GO TO 70
        if ( .NOT. still ) GO TO 80
        if ( stillgen .EQ. 0 ) GO TO 90
        if ( stillgen .EQ. 1 ) GO TO 100
        write(fdout, '(A,X,I0,X,A,A)')
     +  'It is a still life after',stillgen,'steps.',char(13)
        GO TO 110
   70   write(fdout, '(A,X,I0,X,A,A)')
     +  'It is still not a still life even after',1,'step.',char(13)
        GO TO 110
   80   write(fdout, '(A,X,I0,X,A,A)')
     +  'It is still not a still life even after',gen,'steps.',char(13)
        GO TO 110
   90   write(fdout, '(A,A)')
     +  'It is a still life initially.',char(13)
        GO TO 110
  100   write(fdout, '(A,X,I0,X,A,A)')
     +  'It is a still life after',1,'step.',char(13)

  110   i = 1
        close(fdin)
        close(fdout)
        STOP
      END

C subroutine for file open and exception handling
      SUBROUTINE infopen(u)
        integer u, nargs, ios
        character*50 filename
        nargs = iargc()
        if ( nargs .EQ. 0 )
     +  STOP 'Error: wrong number of arguments'
        call getarg(1,filename)
        open(unit=u, file=filename, iostat=ios, status='old')
        if ( ios .NE. 0 ) stop "Error opening file"
        RETURN
      END

C subroutine for producing next generation
      SUBROUTINE generate(row,col,pattern1,pattern2,still,stillgen)
        integer row, col, stillgen, i, j, lives
        character*80 pattern1(*), pattern2(*)
        logical still

C       a nested loop for scanning the whole pattern
        i = 1
  120   if ( i .GT. row ) GO TO 170
          j = 1
  130     if ( j .GT. col ) GO TO 160
            call countlives(row, col, i, j, pattern1, lives)
            if ( (pattern1(i)(j:j) .EQ. '0') .AND.
     +       (lives .EQ. 3) ) GO TO 140
            if ( (pattern1(i)(j:j) .EQ. '*') .AND.
     +       ((lives .EQ. 2) .OR. (lives .EQ. 3)) ) GO TO 150
            pattern2(i)(j:j) = '0'
            if ( pattern1(i)(j:j) .EQ. '*' ) still = .FALSE.
            GO TO 150
  140       still = .FALSE.
            pattern2(i)(j:j) = '*'

  150       j = j + 1
            GO TO 130
  160     i = i + 1
          GO TO 120
  170 END

C subroutine for counting live neighbours of the given position
      SUBROUTINE countlives(row, col, posr, posc, pattern, lives)
        integer row, col, posr, posc, lives, i, j
        character*80 pattern(*)
        lives = 0
        i = -1
  180   if ( i .GT. 1 ) GO TO 220
          j = -1
  190     if ( j .GT. 1 ) GO TO 210
            if ( (posr+i) .LT. 1 .OR. (posr+i) .GT. row .OR.
     +      (posc+j) .LT. 1 .OR. (posc+j) .GT. col ) GO TO 200
            if ( i .EQ. 0 .AND. j .EQ. 0 ) GO TO 200
            if ( pattern(posr+i)((posc+j):(posc+j)) .EQ. '*' )
     +      lives = lives + 1
  200       j = j + 1
            GO TO 190
  210     i = i + 1
          GO TO 180
  220 END

C subroutine for copy from one pattern to another
      SUBROUTINE copy(row, col, pattern1, pattern2)
        integer row, col, i
        character*80 pattern1(*), pattern2(*)
        i = 1
  230   if ( i .GT. row ) GO TO 240
          pattern2(i) = pattern1(i)
          i = i + 1
          GO TO 230
  240   i = 1
      END

C function that returns the length of a string
      integer FUNCTION length(str)
        character*(*) str
        integer i
        i = LEN(str)
  250   if ( i .LT. 1 ) GO TO 260
          if ( str(i:i) .NE. ' ' ) GO TO 260
          i = i - 1
          GO TO 250
  260   length = i
      END
