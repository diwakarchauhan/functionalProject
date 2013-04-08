create2d::Int->Int->Int->[Int]
create2d x y value = take (x*y) (repeat value)


--create3d:: Int->Int->Int->Int->[Int]
--create3d 