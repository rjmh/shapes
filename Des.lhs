
This is a puzzle I got from Des: to enumerate all the closed non
intersecting paths from the origin, through points with non-negative
integer coordinates, of length 10.

> import Data.List

First we must choose a representation. I'm going to represent a path
by a list of ten decisions, to continue straight, turn right, or turn
left.

> data Choice = GoStraight | TurnRight | TurnLeft
>   deriving (Eq, Ord, Show)

Note that the entire space of paths of length 10 only contains ~60K
elements, which we can explore quickly. Thus there is no need to take
advantage of any symmetries to reduce the size of the search
space... in a problem of this size.

We're going to interpret the paths in 2-space, so let's define that.

> data Vec2 = Vec2 Int Int deriving (Eq, Show)

> instance Num Vec2 where
>   fromInteger n = Vec2 (fromInteger n) 0
>   Vec2 x y + Vec2 x' y' = Vec2 (x+x') (y+y')

We'll also need (clockwise) rotation:

> rot (Vec2 x y) = Vec2 y (negate x)

Now, as we traverse a path, we shall be facing a particular direction
(a unit vector), and each choice affects that direction:

> choose :: Choice -> Vec2 -> Vec2
> choose GoStraight v = v
> choose TurnRight  v = rot v
> choose TurnLeft   v = rot (rot (rot v))

To take a step, from a particular point and facing, to a new point and
facing:

> step :: (Vec2,Vec2) -> Choice -> (Vec2,Vec2)
> step (p, v) choice = (p+v, choose choice v)

So, following a path from the origin, facing east, we make the
following steps:

> path :: [Choice] -> [(Vec2,Vec2)]
> path = scanl step (0,1)

Such a path is closed if we end up where we started, facing the same
way.

> closed p = head p == last p

It's non-intersecting if no location appears twice (regardless of
facing), except the first one.

> nonIntersecting p = unique (map fst (tail p))

Are the elements of a list unique?

> unique xs = nub xs == xs

The path should lie in the non-negative quadrant:

> nonNegative p = all (nonNeg . fst) p
>   where nonNeg (Vec2 x y) = x>=0 && y>=0

The search space consists of all 10-element lists of choices.

> space = sp 10
>   where sp 0 = [[]]
>         sp n = [c:cs | c<-[GoStraight,TurnRight,TurnLeft],
>                        cs<-sp (n-1)]

Now we can solve the problem:

> solution =
>   [p | p <- map path space, closed p, nonIntersecting p, nonNegative p]

This finds 20 solutions, agreeing with Des. Finding all 20 takes just
under a second in interpreted Haskell, so indeed, no optimizations are
necessary.

It would be nice to visualize the results. We use ASCII art, noting
that a closed path cannot leave the area [-4,4) x [-4,4), so we'll
draw each shape in an 8x8 grid. A point is in the interior of a closed
path if there are an odd number of vertical path segments to its left.

> interior p (x,y) = odd . length $ filter (vertLeft (x,y)) p
>   where vertLeft (x,y) (Vec2 x' y',Vec2 0 1)    = x'<=x && y==y'
>         vertLeft (x,y) (Vec2 x' y',Vec2 0 (-1)) = x'<=x && y'==y+1
>         vertLeft _     _                        = False

> visualize p = unlines . filter (not . all (==' ')) $
>                 [[if interior p (x,y) then '*' else ' '
>                  | x <- [-4..3]]
>                 | y <- [3,2.. -4]]

> visualizeSolution s = sequence_
>   [putStrLn (visualize p++"---------\n") | p <- s]

> main = visualizeSolution solution