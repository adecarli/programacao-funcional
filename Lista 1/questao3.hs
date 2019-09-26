sqroot :: Double -> Double
sqroot n = ans approxList
    where 
        ans (x:y:ys) = if abs(x - y) <= 0.00001 then x else ans (y:ys)
        approxList = iterate next 1.0
        next a = (a + n / a) / 2