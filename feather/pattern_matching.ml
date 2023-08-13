let square = (1., 2.) 
let cube = (3., 4., 5.)
let sum_of_squares_and_cubes square cube =
  let a, b = square in (
  let x, y, z = cube in 
   a *. b *. z
  )