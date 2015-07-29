
{- Chess engine written in Haskell
    Supporting most of the game but not silly extras and exceptions (yuck)
    
    INCOMPLETE 
-}

import Data.Maybe (catMaybes)
import Data.Either

type Board = [[Maybe Piece]]
data Stats = Stats {whiteCaptured :: [Piece], blackCaptured :: [Piece], moves :: Int}
data Game  = Game Board Stats

data Move = Move {from :: Pos, to :: Pos} deriving (Show, Read)

data Color = Black | White deriving (Eq, Read, Show)

data Piece = Piece {color :: Color, pieceT :: PieceType, pos :: Pos}
            deriving (Eq, Read, Show)

data PieceType = Pawn
               | Rook
               | Bishop
               | Knight
               | Queen
               | King
                deriving (Eq, Read, Show)

type X   = Int
type Y   = Int
type Pos = (X, Y)

(#^) :: Pos -> Y -> Pos
(#^) pos y = (fst pos + y, snd pos)

(#>) :: Pos -> X -> Pos
(#>) pos x = (fst pos, snd pos + x)

(#^>) :: Pos -> Pos -> Pos
(#^>) a b = (fst a + fst b, snd a + snd b)

allies :: Piece -> Piece -> Bool
allies a b = color a == color b

enemies :: Piece -> Piece -> Bool
enemies a b = not (allies a b)

forwardDir :: Color -> Y
forwardDir White = 1
forwardDir Black = -1

atPos :: Board -> Pos -> Maybe Piece
atPos board pos = board !! (fst pos) !! (snd pos)

posIfFree :: Board -> Pos -> Maybe Pos
posIfFree board pos = if atPos board pos == Nothing
                        then Just pos
                        else Nothing

posIfEnemy :: Board -> Piece -> Pos -> Maybe Pos
posIfEnemy board piece pos = case atPos board pos of
                        Just x -> if enemies piece x
                                    then Just pos
                                    else Nothing
                        _      -> Nothing

tryPos :: Board -> Piece -> Pos -> Maybe Pos
tryPos board piece pos =
                    case atPos board pos of
                        Just x  -> if allies piece x
                                    then Nothing
                                    else Just pos
                        Nothing -> Just pos

validDiagonals :: Board -> Piece -> [Pos]
validDiagonals board piece = 
        let p   = pos piece
            tst = tryPos board piece
            dr  = map ( \d -> tst (p #^> (d,d))  )
            dl  = map ( \d -> tst (p #^> (-d,d)) ) in
             catMaybes $ dr [1..] ++ dr [-1,-2..] ++ -- Diag right /
                          dl [1..] ++ dl [-1,-2..]   -- Diag left \

validVerticlesHorizontals :: Board -> Piece -> [Pos]
validVerticlesHorizontals board piece = 
        let p   = pos piece
            tst = tryPos board piece
            vrt  = map ( \d -> tst (p #^> (d, 0)) )
            hrz = map  ( \d -> tst (p #^> (0, d)) ) in
             catMaybes $ vrt [1..] ++ vrt [-1,-2..] ++ -- Vertical
                          hrz [1..] ++ hrz [-1,-2..]   -- Horizontal

pawnHasntMoved :: Piece -> Bool
pawnHasntMoved pawn = let y = snd (pos pawn)
                        in or [y == 1, y == 6]

pawnValidMoves :: Board -> Piece -> [Pos]
pawnValidMoves board pawn = let dir = forwardDir $ color pawn in
        catMaybes [ posIfFree board (pos pawn #^ dir)
                  , if pawnHasntMoved pawn -- Pawn can move 2 places at start
                        then posIfFree board (pos pawn #^ (dir+1))
                        else Nothing
                  , posIfEnemy  board pawn (pos pawn #^> (1, 1))
                  , posIfEnemy  board pawn (pos pawn #^> (-1, 1))
                  ]

queenValidMoves :: Board -> Piece -> [Pos]
queenValidMoves board queen = validDiagonals board queen ++
                                validVerticlesHorizontals board queen

bishopValidMoves :: Board -> Piece -> [Pos]
bishopValidMoves board bishop = validDiagonals board bishop

rookValidMoves :: Board -> Piece -> [Pos]
rookValidMoves board rook = validVerticlesHorizontals board rook

knightValidMoves :: Board -> Piece -> [Pos]
knightValidMoves board knight = 
    let tst   = tryPos board knight
        pairs = \[a,b] -> (a,b) in
            catMaybes $ map (tst . (pos knight #^>) . pairs) $
                sequence [[2,-2], [1,-1]] ++ sequence [[1,-1], [2,-2]]

kingValidMoves :: Board -> Piece -> [Pos]
kingValidMoves board king = 
    let tst   = tryPos board king
        pairs = \[a,b] -> (a,b)
    in catMaybes $ map (tst . (pos king #^>) . pairs) $
                                    sequence [[0, 1, -1], [0, 1, -1]]

validMoves :: Board -> Piece -> [Pos]
validMoves board piece = case pieceT piece of
    Pawn   -> pawnValidMoves   board piece
    Rook   -> rookValidMoves   board piece
    Bishop -> bishopValidMoves board piece
    Queen  -> queenValidMoves  board piece
    King   -> kingValidMoves   board piece
    Knight -> knightValidMoves board piece

step :: Game -> IO Game
step = do
    input <- getMove

main :: IO ()
main = do
    print $ atPos [[Just $ Piece White Queen (0, 0)]] (0, 0)
