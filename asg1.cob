      *Cobol program to simulate Game of Life
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAME-OF-LIFE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO NAME
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 DATA-INPUT PIC X(80).
       FD OUTPUT-FILE.
       01 DATA-OUTPUT.
           03 CELL PIC X OCCURS 1 TO 80 TIMES
               DEPENDING ON WIDTH.

       WORKING-STORAGE SECTION.
      *Global variables
       01 NAME PIC X(90) VALUE SPACES.
       01 GEN PIC 9(5) VALUE ZERO.
       01 ROW PIC 9(3) VALUE ZERO.
       01 COLM PIC 9(2) VALUE ZERO.
       01 STILL-ASGN PIC X.
           88 STILL-EVAL VALUE 'Y'.
       01 STILLGEN PIC 9(5) VALUE ZERO.
       01 PATTERN1.
           03 P1 PIC X(80) OCCURS 100 TIMES.
       01 PATTERN2.
           03 P2 PIC X(80) OCCURS 100 TIMES.
       01 LIVES PIC 9 VALUE ZERO.
       01 WIDTH PIC 9(2) VALUE 1.

      *Temporary variables
       01 I PIC 9(5) VALUE 1.
       01 J PIC 9(5) VALUE 1.
       01 K PIC 9(5) VALUE 1.
       01 TEMP-A PIC 9(5) VALUE ZERO.
       01 TEMP-B PIC 9(5) VALUE ZERO.
       01 TEMP-C PIC 9(5) VALUE ZERO.
       01 TEMP-D PIC X(5) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INFOPEN
           PERFORM SIMULATE
           STRING NAME DELIMITED BY SPACE
               'cob.txt' DELIMITED BY SIZE
               INTO NAME
           END-STRING
           OPEN OUTPUT OUTPUT-FILE
           MOVE COLM TO WIDTH
           PERFORM WRITE-FILE

           CLOSE INPUT-FILE OUTPUT-FILE
           STOP RUN.

      *Read from input file
       INFOPEN.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
           MOVE DATA-INPUT TO NAME
           READ INPUT-FILE
           UNSTRING DATA-INPUT DELIMITED BY SPACE
               INTO GEN
           END-UNSTRING
           READ INPUT-FILE
           UNSTRING DATA-INPUT DELIMITED BY SPACE
               INTO ROW, COLM
           END-UNSTRING
           PERFORM READ-PATTERN.

      *Read pattern from input file
       READ-PATTERN.
           READ INPUT-FILE
           MOVE DATA-INPUT TO P2(I)
           ADD 1 TO I
           IF I <= ROW
               GO TO READ-PATTERN
           END-IF
           MOVE 1 TO I.

      *Simulate the generations
       SIMULATE.
           IF I <= (GEN + 1)
               MOVE 'Y' TO STILL-ASGN
               PERFORM COPY-PATTERN
               PERFORM GENERATION
               IF NOT STILL-EVAL
                   ADD 1 TO I
                   GO TO SIMULATE
               END-IF
               SUBTRACT 1 FROM I GIVING STILLGEN
           END-IF
           MOVE 1 TO I.

      *Copy from one pattern to another
       COPY-PATTERN.
           MOVE P2(J) TO P1(J)
           ADD 1 TO J
           IF J <= ROW
               GO TO COPY-PATTERN
           END-IF
           MOVE 1 TO J.

      *Produce next generation
       GENERATION.
           IF J <= ROW
               IF K <= COLM
                   PERFORM COUNT-LIVES
                   IF P1(J)(K:1) = '0' AND LIVES = 3
                       MOVE 'N' TO STILL-ASGN
                       MOVE '*' TO P2(J)(K:1)
                   END-IF
                   IF P1(J)(K:1) = '*' AND LIVES NOT = 2 AND LIVES NOT = 3
                       MOVE 'N' TO STILL-ASGN
                       MOVE '0' TO P2(J)(K:1)
                   END-IF
                   ADD 1 TO K
                   GO TO GENERATION
               END-IF
               ADD 1 TO J
               MOVE 1 TO K
               GO TO GENERATION
           END-IF
           MOVE 1 TO J.

      *Count live neighbours of the given position
       COUNT-LIVES.
           MOVE ZERO TO LIVES
           SUBTRACT 1 FROM J GIVING TEMP-A
           SUBTRACT 1 FROM K GIVING TEMP-B
           MOVE ZERO TO TEMP-C
           INSPECT P1(TEMP-A)(TEMP-B:3) TALLYING TEMP-C FOR ALL '*'
           ADD TEMP-C TO LIVES
           MOVE ZERO TO TEMP-C
           INSPECT P1(J)(TEMP-B:3) TALLYING TEMP-C FOR ALL '*'
           ADD TEMP-C TO LIVES
           MOVE ZERO TO TEMP-C
           ADD 1 TO J GIVING TEMP-A
           INSPECT P1(TEMP-A)(TEMP-B:3) TALLYING TEMP-C FOR ALL '*'
           ADD TEMP-C TO LIVES
           MOVE ZERO TO TEMP-A
           MOVE ZERO TO TEMP-B
           MOVE ZERO TO TEMP-C
           IF P1(J)(K:1) = '*'
               SUBTRACT 1 FROM LIVES
           END-IF.

      *Write result into file
       WRITE-FILE.
           IF I <= ROW
               MOVE P1(I) TO DATA-OUTPUT
               WRITE DATA-OUTPUT
               ADD 1 TO I
               GO TO WRITE-FILE
           END-IF
           MOVE 1 TO I
           IF STILL-EVAL
               IF STILLGEN = 0
                   MOVE 29 TO WIDTH
                   MOVE 'It is a still life initially.' TO DATA-OUTPUT
               END-IF
               IF STILLGEN = 1
                   MOVE 32 TO WIDTH
                   MOVE 'It is a still life after 1 step.' TO DATA-OUTPUT
               END-IF
               IF STILLGEN NOT = 0 AND STILLGEN NOT = 1
                   MOVE STILLGEN TO TEMP-D
                   MOVE ZERO TO TEMP-A
                   INSPECT TEMP-D TALLYING TEMP-A FOR LEADING ZEROS
                   SUBTRACT TEMP-A FROM 5 GIVING TEMP-B
                   ADD 1 TO TEMP-A
                   ADD 32 TO TEMP-B GIVING WIDTH
                   STRING 'It is a still life after ' DELIMITED BY SIZE
                       TEMP-D(TEMP-A:TEMP-B) DELIMITED BY SIZE
                       ' steps.' DELIMITED BY SIZE
                       INTO DATA-OUTPUT
                   MOVE ZERO TO TEMP-A
                   MOVE ZERO TO TEMP-B
                   MOVE SPACES TO TEMP-D
               END-IF
               WRITE DATA-OUTPUT
           END-IF
           IF NOT STILL-EVAL
               IF GEN = 1
                   MOVE 47 TO WIDTH
                   MOVE 'It is still not a still life even after 1 step.'
                       TO DATA-OUTPUT
               END-IF
               IF GEN NOT = 1
                   MOVE GEN TO TEMP-D
                   MOVE ZERO TO TEMP-A
                   INSPECT TEMP-D TALLYING TEMP-A FOR LEADING ZEROS
                   SUBTRACT TEMP-A FROM 5 GIVING TEMP-B
                   ADD 1 TO TEMP-A
                   ADD 47 TO TEMP-B GIVING WIDTH
                   STRING 'It is still not a still life even after '
                       DELIMITED BY SIZE
                       TEMP-D(TEMP-A:TEMP-B) DELIMITED BY SIZE
                       ' steps.' DELIMITED BY SIZE
                       INTO DATA-OUTPUT
                   MOVE ZERO TO TEMP-A
                   MOVE ZERO TO TEMP-B
                   MOVE SPACES TO TEMP-D
               END-IF
               WRITE DATA-OUTPUT
           END-IF.
