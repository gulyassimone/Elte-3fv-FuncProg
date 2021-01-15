

-- double visszaadja a parameter duplajat
{-

double double'(double n) {
  return 2 * n;
}

-}

double :: Integer -> Integer
double n = 2 * n

double' n = 2 * n

six :: Int
six = 6

six' :: Integer
six' = 6

six'' :: Num p => p
six'' = 6

-- szamold ki a teglalap teruletenek negyzetet
{-

int area(int a, int b) {
   int c = a * b;
   return c;
   // return a * b;
}

-}
area :: Integer -> Integer -> Integer
area a b = (a * b) ^ 2
--area a b = ((*) a b) ^ 2


-- racionalis szam = 2 db Integer
-- racionalis szam = Integer par
mult (a1, b1) (a2, b2) = (a1 * a2, b1 * b2)
