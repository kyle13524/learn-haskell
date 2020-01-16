module Learn where
x = 10 * 5 + y

myResult = x * 5

y = 10

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

mult1 = x * y
  where x = 5
        y = 6


func1 = x * 3 + y
  where x = 3
        y = 1000

func2 = x * 5
  where x = 10 * 5 + y
        y = 10

func3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

            
