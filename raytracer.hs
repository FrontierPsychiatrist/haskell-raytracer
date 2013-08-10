import Data.List (sort)

data Vector = Vector Double Double Double
data Point = Point Double Double Double deriving(Eq)
data Color = Color Double Double Double deriving(Show)
data Object = Plane Vector Double | Sphere Point Double
data Ray = Ray Point Vector
data Light = Light Point Double
data Scene = Scene [Object] Light
data Intersection = Intersection { point :: Point, lambda :: Double } deriving(Eq)

instance Ord Intersection where
  (Intersection _ l1) > (Intersection _ l2) = l1 > l2

maybeToBool :: Maybe a -> Bool
maybeToBool (Just x) = True
maybeToBool Nothing = False

square :: (Num a) => a -> a
square x = x*x

pointDifference :: Point -> Point -> Vector
pointDifference (Point x1 y1 z1) (Point x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

length :: Vector -> Double
length (Vector x y z) = sqrt(x*x + y*y + z*z)

scalarProduct :: Vector -> Vector -> Double
scalarProduct (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

toVector :: Point -> Vector
toVector (Point x y z) = Vector x y z

apply :: Ray -> Double -> Point
apply (Ray (Point ox oy oz) (Vector dx dy dz)) lambda =
  Point (ox + lambda * dx) (oy + lambda * dy) (oz + lambda * dz)

hits :: Object -> Ray -> Maybe Intersection
hits (Plane normal dist) ray@(Ray origin direction) =
    if(gamma * side > 0.000001)
    then
      let lambda = side/gamma
      in Just (Intersection (apply ray lambda) lambda)
    else Nothing
    where gamma = scalarProduct direction normal
          side = dist - scalarProduct normal (toVector origin)
hits object ray = Nothing

getRay :: Int -> Int -> Ray
getRay x y = Ray (Point 0.0 0.0 0.0) (Vector dx dy (-120.0))
  where dx = (-320.0) + fromIntegral(x) + 0.5
        dy = (-240.0) + fromIntegral(y) + 0.5

traceRay :: Scene -> Ray -> Color
traceRay (Scene objects light) ray@(Ray origin direction) =
  if null intersections
  then Color 0 0 0
  else
    case (head intersections) of
      Just intersection -> let intensity = 1 / square(Main.length( (pointDifference (point intersection) origin) ))
        in Color intensity intensity intensity
      Nothing -> Color 0 0 0
  where intersections = sort(filter maybeToBool (map (\object -> hits object ray) objects))

rays = [ getRay x y | x <- [0..320], y <- [0..239] ]
plane = Plane (Vector 0 0 1) (-3)
light = Light (Point 0 1 (-0.2)) 3
scene = Scene [plane] light