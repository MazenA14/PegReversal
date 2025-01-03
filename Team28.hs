type Position = (Int,Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)
 
createBoard:: Position-> Board
createBoard pos | not (checkPos pos generateBoard) = error "The position is not valid."
                | otherwise = changeColor pos generateBoard []
 
 
isValidMove:: Move-> Board-> Bool
isValidMove (M (x,y)) currentBoard | (checkPos (x,y) currentBoard) && (checkColorB (x,y) currentBoard) = any (checkColorW currentBoard) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
								   | otherwise=False
 
 
 
-- isGoal:: Board-> Bool
 
 
 
-- showPossibleNextStates:: Board-> [State]




-----------------------------
checkPos _ [] = False
checkPos pos (Peg index _ :t) | index == pos = True
                             | otherwise = checkPos pos t

changeColor _ [] acc = acc
changeColor pos (Peg index color:t) acc | index == pos = changeColor pos t (acc ++ [Peg index W])
                                        | otherwise = changeColor pos t (acc ++ [Peg index color])

checkColorB :: Position -> Board -> Bool
checkColorB pos (Peg position color:t) | position==pos = color==B
									   | otherwise= checkColorB pos t  
									   
									   
checkColorW (Peg position color:t) pos |(checkPos pos (Peg position color:t) && position==pos) = color==W
									   | otherwise = checkColorW t pos 

generateBoard :: Board
generateBoard = [Peg (-3,-1) B, Peg (-3,0) B, Peg (-3,1) B, Peg (-2,-1) B,
                 Peg (-2,0) B, Peg (-2,1) B, Peg (-1,-3) B, Peg (-1,-2) B,
                 Peg (-1,-1) B, Peg (-1,0) B, Peg (-1,1) B, Peg (-1,2) B,
                 Peg (-1,3) B, Peg (0,-3) B, Peg (0,-2) B, Peg (0,-1) B,
                 Peg (0,0) B, Peg (0,1) B, Peg (0,2) B, Peg (0,3) B,
                 Peg (1,-3) B, Peg (1,-2) B, Peg (1,-1) B, Peg (1,0) B,
                 Peg (1,1) B, Peg (1,2) B, Peg (1,3) B, Peg (2,-1) B,
                 Peg (2,0) B, Peg (2,1) B, Peg (3,-1) B, Peg (3,0) B, Peg (3,1) B]