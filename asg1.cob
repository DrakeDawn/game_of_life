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
       01 DATA-OUTPUT PIC X(80).

       WORKING-STORAGE SECTION.
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
           PERFORM WRITE-FILE

           CLOSE INPUT-FILE OUTPUT-FILE
           STOP RUN.

       INFOPEN.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
           MOVE DATA-INPUT TO NAME
           READ INPUT-FILE
           UNSTRING DATA-INPUT DELIMITED BY SPACE
               INTO GEN
           READ INPUT-FILE
           UNSTRING DATA-INPUT DELIMITED BY SPACE
               INTO ROW, COLM
           END-UNSTRING
           PERFORM READ-PATTERN.

       READ-PATTERN.
           READ INPUT-FILE
           MOVE DATA-INPUT TO P2(I)
           ADD 1 TO I
           IF I <= ROW THEN
               GO TO READ-PATTERN
           END-IF
           MOVE 1 TO I.

       SIMULATE.
           IF I <= (GEN + 1) THEN
               MOVE 'Y' TO STILL-ASGN
               PERFORM COPY-PATTERN
               PERFORM GENERATION
               IF NOT STILL-EVAL THEN
                   ADD 1 TO I
                   GO TO SIMULATE
               END-IF
               SUBTRACT 1 FROM I GIVING STILLGEN
           END-IF
           MOVE 1 TO I.

       COPY-PATTERN.
           MOVE P2(J) TO P1(J)
           ADD 1 TO J
           IF J <= ROW THEN
               GO TO COPY-PATTERN
           END-IF
           MOVE 1 TO J.

       GENERATION.
           IF J <= ROW THEN
               IF K <= COLM THEN
                   PERFORM COUNT-LIVES
                   IF P1(J)(K:1) = '0' AND LIVES = 3 THEN
                       MOVE 'N' TO STILL-ASGN
                       MOVE '*' TO P2(J)(K:1)
                   END-IF
                   IF P1(J)(K:1) = '*' AND LIVES NOT = 2 AND LIVES NOT = 3 THEN
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
           IF P1(J)(K:1) = '*' THEN
               SUBTRACT 1 FROM LIVES
           END-IF.

       WRITE-FILE.
           IF I <= ROW THEN
               MOVE P1(I) TO DATA-OUTPUT
               WRITE DATA-OUTPUT
               ADD 1 TO I
               GO TO WRITE-FILE
           END-IF
           MOVE 1 TO I
           IF STILL-EVAL THEN
               IF STILLGEN = 0 THEN
                   MOVE 'It is a still life initially.' TO DATA-OUTPUT
               END-IF
               IF STILLGEN = 1 THEN
                   MOVE 'It is a still life after 1 step.' TO DATA-OUTPUT
               END-IF
               IF STILLGEN NOT = 0 AND STILLGEN NOT = 1 THEN
                   MOVE STILLGEN TO TEMP-D
                   MOVE ZERO TO TEMP-A
                   INSPECT TEMP-D TALLYING TEMP-A FOR LEADING ZEROS
                   SUBTRACT TEMP-A FROM 5 GIVING TEMP-B
                   ADD 1 TO TEMP-A
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
           IF NOT STILL-EVAL THEN
               IF GEN = 1 THEN
                   MOVE 'It is still not a still life even after 1 step.'
                       TO DATA-OUTPUT
               END-IF
               IF GEN NOT = 1 THEN
                   MOVE GEN TO TEMP-D
                   MOVE ZERO TO TEMP-A
                   INSPECT TEMP-D TALLYING TEMP-A FOR LEADING ZEROS
                   SUBTRACT TEMP-A FROM 5 GIVING TEMP-B
                   ADD 1 TO TEMP-A
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
