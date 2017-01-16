{-
    A Haskell program that implements a basic raytracer

    Program should be imported into a ghci session.
    The program is then started by calling the main function.

    The raytracer has a set screen size and camera position.
    When the program runs, the user is prompted to enter the position of the light
    source and the position, radius and colour of the sphere to render. The user
    is also prompted to enter the filename of the output image file. The output
    image file is in ppm format.

    Author: Peter Andrew
    Date: 7/12/2010

    Program created as part of the coursework for module P00400
-}


-- Create type synonyms to imrove clarity of code
type Vector3 = (Float, Float, Float)
type Colour = (Float, Float, Float)

type Origin = Vector3
type Direction = Vector3
type Ray = (Origin, Direction)

type Point = Vector3
type PointIntersection = Point
type Normal = Vector3
type Intersection = (PointIntersection, Normal, Colour)

type Radius = Float
type Sphere = (Point, Radius, Colour)

type Scene = (Origin, Point, Sphere)


-- Set screen size
screenSizeX :: Integer
screenSizeY :: Integer
screenSizeX = 300
screenSizeY = 300

-- Set camera position
camera :: Origin
camera = (150, 150, -300)


-- Vector arithmetic functions
-- Function takes two vectors as parameters and returns their sum as a vector
vec_add :: Vector3 -> Vector3 -> Vector3
vec_add (a, b, c) (x, y, z) = (a+x, b+y, c+z)

-- Function takes two vectors as parameters and returns their difference as a vector
vec_sub :: Vector3 -> Vector3 -> Vector3
vec_sub (a, b, c) (x, y, z) = (a-x, b-y, c-z)

-- Function take one vector as a parameter and returns it's magnitude as a floating point number
magnitude :: Vector3 -> Float
magnitude (x, y, z) = sqrt ( x^2 + y^2 + z^2 )

-- Function take one vector and one floating point number as parameters and returns
-- the result of multiplying the parameters as a vector
scalarmult :: Vector3 -> Float -> Vector3
scalarmult (x, y, z) s = (x*s, y*s, z*s)

-- Function takes one vector as a parameter and returns it's normalized vector (unit vector)
normalize :: Vector3 -> Vector3
normalize vec
    | (magnitude vec) /= 0 = scalarmult vec (1 / magnitude vec)
    | otherwise = (0, 0, 0)

-- Function takes two vectors as parameters and returns their dot product as a floating point number
dot_product :: Vector3 -> Vector3 -> Float
dot_product (a, b, c) (x, y, z) = ( a*x + b*y + c*z )


-- Function takes origin (point vector) and another point vector as parameters and creates a
-- ray (tuple consiting of an origin and direction) as a result. Function calculates
-- distance between point vectors, then normalizes result to produce direction.
ray_generate :: Origin -> Point -> Ray
ray_generate orig dest =
    let dir = dest `vec_sub` orig
    in (orig, normalize dir)

-- Function intersects passed ray (tuple of origin and direction) with passed
-- sphere (tuple of point vector, float and colour tuple). Function calculates whether passed
-- ray has any intersection points with passed sphere and returns the intersection point
-- closest to the screen. If no intersection points exist, function returns invalid intersection
-- point tuple. Function returns tuple consisting of point of intersection (vector), the normal
-- of the point of intersection (vector) and the colour at the intersection point (colour tuple)
intersect :: Sphere -> Ray -> Intersection
intersect ((sx, sy, sz), radi, col) ((ox, oy, oz), (dx, dy, dz)) =
    let oc = (sx, sy, sz) `vec_sub` (ox, oy, oz)
        v = dot_product oc (dx, dy, dz)
        ocdot = dot_product oc oc
        disc = radi^2 - ( ocdot - v^2 )
        d = sqrt(disc)
    in if (disc < 0) then
        -- No intersection exists, return invalid intersection tuple
        ((-1, -1, -1), (0, 0, 0), (0, 0, 0))
    else
        -- Calulate distance from screen to point of intersection
    	let screen_dist = (dx, dy, dz) `scalarmult` (v - d)
            -- Calculate point of intersection from camera
            pi = (ox, oy, oz) `vec_add` screen_dist
            -- Calculate normal of intersection
            pi_normal = (pi `vec_sub` (sx, sy, sz)) `scalarmult` (1 / radi)
        -- Return tuple of point of intersection, normal of intersection and colour tuple
        in (pi, pi_normal, col)

-- Function takes intersection as parameter and returns boolean true if
-- it is valid (intersection point is not -1, -1, -1)
valid_intersection :: Intersection -> Bool
valid_intersection ((x, y, z),_,_) = (x /= -1 && y /= -1 && z /= -1)

-- Function takes scene (tuple of camera position, light position and sphere) and 
-- ray (tuple of origin and direction) as parameters and returns colour tuple.
-- Function intersects ray with sphere. If no valid intersection exists returns
-- black colour tuple. Otherwise calculates dot product of light vector with normal
-- of intersection point. If product is greater than 0 returns colour at intersection
-- point with dot product. Otherwise return black colour tuple.
raytrace :: Scene -> Ray -> Colour
raytrace scene ray =
    let (_, light, sphere) = scene
        intersection = intersect sphere ray
    in if (valid_intersection intersection) then
        let (pointInter, norm, col) = intersection
            l = normalize ( light `vec_sub` pointInter )
            dot = norm `dot_product` l
        in if (dot > 0) then
            col `scalarmult` dot
        else
            (0, 0, 0)
    else (0, 0, 0)

-- Function takes two integers (width and height of screen) and scene as parameters and
-- returns list of colour tuples. Function generates rays from camera position through each
-- pixel of the screen. Function uses a list comprehension to iterate through each height and width.
-- Function then raytraces each ray through scene. Function uses map to pass list of rays to raytrace
-- function with scene as the first parameter. 
render_scene :: Integer -> Integer -> Scene -> [Colour]
render_scene width height scene =
    let (camera, _, _) = scene
        rays = [ ray_generate camera (fromIntegral x, fromIntegral y, 0) | y <- [0..height-1], x <- [0..width-1] ]
    in map (raytrace scene) rays

-- Function takes two integers (width and height of image) and list of colour tuples as parameters
-- and returns string in ppm image format.
ppm_string :: Integer -> Integer -> [Colour] -> String
ppm_string width height colours = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ values(colours)
    where values [] = ""
          values ((r, g, b):colours) =
               show (round (r * 255)) ++ " "
            ++ show (round (g * 255)) ++ " "
            ++ show (round (b * 255)) ++ " "
            ++ values colours

-- Create do block to get user input, render scene and write image file
main = do
    putStrLn ("The screen size is: " ++ show screenSizeX ++ ", " ++ show screenSizeY)
    let (cx, cy, cz) = camera
    putStrLn ("The camera position is: " ++ show cx ++ ", " ++ show cy ++ ", " ++ show cz)
    putStrLn "Enter light position:"
    putStr "x: "
    lXStr <- getLine
    putStr "y: "
    lYStr <- getLine
    putStr "z: "
    lZStr <- getLine
    putStrLn "Enter sphere attributes:"
    putStr "x: "
    sphXStr <- getLine
    putStr "y: "
    sphYStr <- getLine
    putStr "z: "
    sphZStr <- getLine
    putStr "radius: "
    sphRadiusStr <- getLine
    putStr "colour r (0.0-1.0): "
    sphColRStr <- getLine
    putStr "colour g (0.0-1.0): "
    sphColGStr <- getLine
    putStr "colour b (0.0-1.0): "
    sphColBStr <- getLine
    putStr "Enter output ppm filename: "
    filename <- getLine
    -- Create sphere, light and scene tuples. Create colours list by rendering scene,
    -- create ppm string and write result to image file.
    let sphere = ((read sphXStr, read sphYStr, read sphZStr), read sphRadiusStr, (read sphColRStr, read sphColGStr, read sphColBStr))
        light = (read lXStr, read lYStr, read lZStr) 
        scene = (camera, light, sphere)
        colours = render_scene screenSizeX screenSizeY scene
    writeFile filename (ppm_string screenSizeX screenSizeY colours)
