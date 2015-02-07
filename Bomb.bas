DECLARE SUB welcome ()
DECLARE SUB introduc ()
s: CLS : SCREEN 2: CALL welcome: CALL introduc: CLS : RANDOMIZE VAL(RIGHT$(TIME$, 2)): win = 0: lose = 0: INPUT "g=?(2--20)", g
CIRCLE (10, 10), 7
LINE (50, 199)-(80, 199): LINE -(89, 194): LINE -(50, 194): LINE -(50, 199)
LINE (56, 194)-(56, 187): LINE -(70, 187): LINE -(70, 194): LINE -(85, 187)
DIM picb(46), pics(355)
GET (3, 7)-(17, 13), picb
GET (19, 187)-(90, 199), pics
DO
CLS : vb = INT(RND * 90 + 130) / 100: k = vb: j = vb: d = 0: vv = 1: c = 0
LOCATE 10, 5: PRINT "v(ball):v(ship)="; vb; ":1"
FOR i = vb TO 565 STEP vb
bx = vb * (i - j): by = g / 2000 * (i - j) * (i - j)
PUT (bx, by), picb
PUT (i, 186), pics
LOCATE 1, 65: PRINT "X="; INT(bx)
LOCATE 2, 65: PRINT "Y="; INT(by)
LOCATE 3, 65: PRINT "TIME="; INT(i - j): j = j + k
LOCATE 4, 65: PRINT "win="; win
LOCATE 5, 65: PRINT "lose="; lose
LOCATE 6, 65: PRINT "total="; win + lose
IF INKEY$ = CHR$(27) THEN END
IF win + lose = 0 THEN 200
IF win + lose = 20 THEN
 accuracy = INT((win / (win + lose)) * 1000) / 10
LOCATE 7, 65: PRINT "accuracy="; INT((win / (win + lose)) * 1000) / 10; "%"
FOR i = 1 TO 100000: NEXT: CLS
DO UNTIL INKEY$ = CHR$(32): LOCATE 13, 20: PRINT win; "German ships were bombed", lose; "ships escaped": LOOP: CLS
SELECT CASE INT(win / 4)
CASE 0
DO UNTIL INKEY$ = CHR$(32): LOCATE 13, 20: PRINT "Sorry!You haven't successfully done this task!!": LOOP
CASE 1, 2
DO UNTIL INKEY$ = CHR$(32): LOCATE 13, 20: PRINT "Sorry!You have just done this task commonly!": LOOP
CASE 3
DO UNTIL INKEY$ = CHR$(32): LOCATE 13, 20: PRINT "Wonderful!You have done this task perfectly!": LOOP
CASE 4, 5
DO UNTIL INKEY$ = CHR$(32): LOCATE 13, 20: PRINT "Wonderful!You have done this task perfectly!You are not a person!": LOOP
END SELECT
GOTO 510
END IF
LOCATE 7, 65: PRINT "accuracy="; INT((win / (win + lose)) * 1000) / 10; "%"
200 IF d <> 0 THEN SOUND 350 - INT(i / 2), d
FOR l = 1 TO 70 STEP .2 * vv: NEXT l
PUT (bx, by), picb
PUT (i, 186), pics
IF bx > 610 OR by >= 184 THEN vv = 70: k = vb: d = 0: IF c = 0 AND bx > i + 8 AND bx < i + 67 THEN m$ = "you win!!": win = win + 1: n = 2000: GOTO 340 ELSE c = 1
310 IF INKEY$ <> "" AND by < 189 THEN k = 0: d = .3
NEXT i: m$ = "you lose!!": lose = lose + 1: n = 300: GOTO 400
290 PUT (bx, by), picb
PUT (i, 186), pics: GOTO 200
GOTO 310
340 PUT (bx, by), picb
PUT (i, 186), pics
FOR v = 1 TO 100
CIRCLE (RND * 70 + i, RND * 20 + 180), RND * 5 + 1
SOUND RND * 1000 + 300, .2
NEXT v
400 FOR i1 = 1 TO LEN(m$)
FOR i2 = 2 TO 10
LOCATE i2, i1 + 30: PRINT MID$(m$, i1, 1)
LOCATE i2 - 1, i1 + 30: PRINT " "
SOUND 500 - RND * 200 + n, .5
NEXT i2, i1
LOOP
510 CLS : LOCATE 12, 5:  PRINT "play again?(y/n)"
520 a$ = INKEY$
IF a$ = "n" OR a$ = "N" THEN CLS : PRINT "Thank you for playing this game!": GOSUB d: END
IF a$ = "y" OR a$ = "Y" THEN GOSUB d: FOR i = 1 TO 10000: NEXT: GOTO s
IF a$ <> CHR$(78) OR a$ <> CHR$(110) OR a$ <> CHR$(89) OR a$ <> CHR$(121) THEN 520
d: OPEN "i", 2, "d:\qb\save"
FOR i = 1 TO 10
 INPUT #2, a$(i), B(i)
   IF accuracy > B(i) THEN
   B(i) = accuracy
   FOR j = i + 1 TO 10: INPUT #2, a$(j), B(j): NEXT
   FOR j = i + 1 TO 10: B$(j) = a$(j - 1): a(j) = B(j - 1):  NEXT j
   FOR j = i + 1 TO 10: a$(j) = B$(j): B(j) = a(j): NEXT
   LOCATE 11, 25: PRINT "Congratulations!You have made a new record!!"
   LOCATE 13, 30: LINE INPUT "Enter your name:", a$(i)
   CLS :  FOR j = 1 TO i - 1: LOCATE j + 3, 20: PRINT j, a$(j), B(j): NEXT: LOCATE i + 3, 19: PRINT "*"; i, : PRINT a$(i), B(i): FOR j = i + 1 TO 10: LOCATE j + 3, 20: PRINT j, a$(j), B(j): NEXT
   GOTO 80
   END IF
NEXT i
FOR i = 1 TO 10: LOCATE i + 3, 20:  PRINT i, a$(i), B(i): NEXT
80 CLOSE 2
OPEN "o", 2, "d:\qb\save": FOR i = 1 TO 10: WRITE #2, a$(i), B(i): NEXT: CLOSE 2
RETURN

SUB introduc
CLS
DO WHILE INKEY$ <> CHR$(32)
LOCATE 13, 30: PRINT "20 German ships are trying to escape!!"
LOCATE 15, 35: PRINT "Bomb them to God!!"
LOCATE 18, 35: PRINT "Press Space to continue!"
LOOP
END SUB

SUB welcome
SCREEN 2
30 FOR i = 80 TO 1 STEP -1
LINE (320 - 2 * i, 100 - i)-(320 + 2 * i, 100 + i), 1, B
LOCATE 13, 37: PRINT "WELCOME!"
LOCATE 20, 30: PRINT "Press SPACE to start game!"
IF INKEY$ = CHR$(32) THEN 10
FOR k = 1 TO 200: NEXT k
NEXT
BEEP
FOR i = 1 TO 80
LINE (320 - 2 * i, 100 - i)-(320 + 2 * i, 100 + i), 0, B
LOCATE 13, 37: PRINT "WELCOME!"
LOCATE 20, 30: PRINT "Press SPACE to start game!"
IF INKEY$ = CHR$(32) THEN 10
FOR k = 1 TO 200: NEXT k
NEXT
GOTO 30
10 END SUB

