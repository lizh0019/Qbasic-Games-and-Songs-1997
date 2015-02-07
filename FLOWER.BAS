INPUT "number(2-7)="; n
INPUT "colour(1-15)="; c
SCREEN 2: CLS : b = 3.141592653589793#
FOR a = 0 TO 2 * b STEP b / 360
e = 100 * (1 + SIN(n * a))
x = 320 + e * COS(a): x1 = 320 + e * COS(a + b / 5)
y = 100 + e * SIN(a) / 2: y1 = 100 + e * SIN(a + b / 5) / 2
LINE (x, y)-(x1, y1)
NEXT: END

