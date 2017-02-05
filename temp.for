C my first FORTRAN program
      PROGRAM Gameoflife
        integer fdin, fdout, gen, row, col, stillgen, i, ios
        character*90 name
        character*80 pattern1(100), pattern2(100)
        logical still
        call infopen(fdin)
        read(fdin,'(A)') name
        read(fdin,*) gen
        read(fdin,*) row, col

C read pattern from input file
        i = 1
   10   if ( i .GT. row ) GO TO 20
          read(fdin,'(A)') pattern2(i)
          i = i + 1
          GO TO 10

C simulate the generations
   20   i = 1
   30   if ( i .GT. (gen+1) ) GO TO 40
          still = .TRUE.
          call copy(row, col, pattern2, pattern1)
          call generate(row,col,pattern1,pattern2,still,stillgen)
          if ( still ) then
            stillgen = i - 1
            GO TO 40
          endif
          i = i + 1
          GO TO 30

C write result into file
   40   name = name(1:length(name)) // 'for.txt'
        open(unit=fdout, file=name, iostat=ios, status='new')
        if ( ios .NE. 0 ) stop "Error opening file"

        i = 1
   50   if ( i .GT. row ) GO TO 60
          write(fdout, '(A)') pattern1(i)(1:length(pattern1(i)))
          i = i + 1
          GO TO 50

   60   if ( .NOT. still .AND. (gen .EQ. 1) ) GO TO 70
        if ( .NOT. still ) GO TO 80
        if ( stillgen .EQ. 0 ) GO TO 90
        if ( stillgen .EQ. 1 ) GO TO 100
        write(fdout, '(A,X,I0,X,A)')
     +  'It is a still life after',stillgen,'steps.'
        GO TO 300
   70   write(fdout, '(A,X,I0,X,A)')
     +  'It is still not a still life even after',1,'step.'
        GO TO 300
   80   write(fdout, '(A,X,I0,X,A)')
     +  'It is still not a still life even after',gen,'steps.'
        GO TO 300
   90   write(fdout, '(A)')
     +  'It is a still life initially.'
        GO TO 300
  100   write(fdout, '(A,X,I0,X,A)')
     +  'It is a still life after',1,'step.'

  300   close(fdin)
        close(fdout)
        STOP
      END

C subroutine for file open and error print
      SUBROUTINE infopen(u)
        integer u, nargs, ios
        character*50 filename
        nargs = iargc()
        if ( nargs .EQ. 0 ) then
          write(*,*) 'Error: wrong number of arguments'
          write(*,*) 'Please enter input filename at command line'
          STOP
        end if
        call getarg(1,filename)
        open(unit=u, file=filename, iostat=ios, status='old')
        if ( ios .NE. 0 ) stop "Error opening file"
        RETURN
      END

C subroutine for produce next generation
      SUBROUTINE generate(row,col,pattern1,pattern2,still,stillgen)
        integer row, col, stillgen, i, j, lives
        character*80 pattern1(*), pattern2(*)
        logical still

C nested loop
        i = 1
  110   if ( i .GT. row ) GO TO 120
          j = 1
  150     if ( j .GT. col ) GO TO 160
            call countlives(row, col, i, j, pattern1, lives)
            if ( (pattern1(i)(j:j) .EQ. '0') .AND.
     +       (lives .EQ. 3) ) GO TO 130
            if ( (pattern1(i)(j:j) .EQ. '*') .AND.
     +       ((lives .EQ. 2) .OR. (lives .EQ. 3)) ) GO TO 140

            pattern2(i)(j:j) = '0'
            if ( pattern1(i)(j:j) .EQ. '*' ) still = .FALSE.

            GO TO 140
  130       still = .FALSE.
            pattern2(i)(j:j) = '*'

  140       j = j + 1
            GO TO 150
  160     i = i + 1
          GO TO 110

  120 END

C subroutine for counting live neighbours of the given position
      SUBROUTINE countlives(row, col, posr, posc, pattern, lives)
        integer row, col, posr, posc, lives
        character*80 pattern(*)
        lives = 0
        if ( (posr-1) .GT. 0 ) then
          if ( pattern(posr-1)(posc:posc) .EQ. '*' )
     +    lives = lives + 1
          if ( (posc-1) .GT. 0 ) then
            if ( pattern(posr-1)((posc-1):(posc-1)) .EQ. '*' )
     +      lives = lives + 1
          end if
          if ( (posc+1) .LE. col ) then
            if ( pattern(posr-1)((posc+1):(posc+1)) .EQ. '*' )
     +      lives = lives + 1
          end if
        end if

        if ( (posc-1) .GT. 0 ) then
          if ( pattern(posr)((posc-1):(posc-1)) .EQ. '*' )
     +    lives = lives + 1
        end if
        if ( (posc+1) .LE. col ) then
          if ( pattern(posr)((posc+1):(posc+1)) .EQ. '*' )
     +    lives = lives + 1
        end if

        if ( (posr+1) .LE. row ) then
          if ( pattern(posr+1)(posc:posc) .EQ. '*' )
     +    lives = lives + 1
          if ( (posc-1) .GT. 0 ) then
            if ( pattern(posr+1)((posc-1):(posc-1)) .EQ. '*' )
     +      lives = lives + 1
          end if
          if ( (posc+1) .LE. col ) then
            if ( pattern(posr+1)((posc+1):(posc+1)) .EQ. '*' )
     +      lives = lives + 1
          end if
        end if
      END

C copy from one pattern to another
      SUBROUTINE copy(row, col, pattern1, pattern2)
        integer row, col, i
        character*80 pattern1(*), pattern2(*)
        i = 1
  210   if ( i .GT. row ) GO TO 220
          pattern2(i) = pattern1(i)
          i = i + 1
          GO TO 210
  220   i = 1
      END

      integer FUNCTION length(str)
        character*(*) str
        integer i
        i = LEN(str)
  400   if ( i .LT. 1 ) GO TO 410
          if ( str(i:i) .NE. ' ' ) GO TO 410
          i = i - 1
  410   length = i
      END
