-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI(State,author,nickname,initial,think) where

import Data.List
import Data.Maybe
import Reversi
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- AI fistly scan the board to collect move options, each field will be scaned in 8 dirctions. If no possible move, make a 'pass' move.
   Each borad will calculate the 'nice value' and judgeing by minMax algorithm to consider a optimal move.
   The nice value is a sum of individual field on board. Each field has its vaule accroding to its position and ownship.
   References
   https://github.com/im0qianqian/Reversi
   https://github.com/geon/Othello-in-Haskell
 -}

{- A data tyype which idicate 3 possible conditions of each field.
   B means black, W means White, N means NULL value
 -}
data FieldType =  B | W | N
  deriving (Eq, Show)

{- (Int, FieldType) donates a spcific field with its current condition.
   Int donates field number.
   FieldType donates its condition.
   INVARIANT: Int must in range [0,63].
 -}
type Field = (Int, FieldType)

{- ([Field], FieldType) is the internal state of AI.
   INVARIANT: must have valid Field and valid Player.
 -}
type State = ([Field], FieldType)

{- Direction of scanning. Use to scan 8 directions of the value.
   Use combine of WASD to indicate 8 directions.
 -}
data Scan = WW | WA | AA | SA | SS | SD | DD | WD
  deriving (Eq, Show)

author :: String
author = "Yuzhi Chen"

nickname :: String
nickname = "IAsreveR"

{- opponent state
   Get the opponent of the player
   PRE: state must be either B or W.
   RETURNS: The opponent state.
   EXAMPLES: opponent B = W
 -}
opponent :: FieldType -> FieldType

opponent B = W
opponent W = B

{- changeType field board
   Change the field on the board.
   PRE: 'field' should be a vaild 'type Field'.
   RETURNS: Board after change.
   EXAMPLES: changeType (0,W) defaultBoard == [(0,W),(1,N),...,(27,W),(28,B),...,(35,B),(36,W),...,(62,N),(63,N)]
 -}
changeType :: Field -> [Field] -> [Field]
--VARIANT: n
changeType (0,state) ((p,_) : tail) = (p,state) : tail
changeType (n,state) (head : tail) = head : changeType (n - 1,state) tail

{- defaultBoard
   construct default board.
   RETURNS: Default board.
   EXAMPLES: defaultBoard == [(0,N),(1,N),...,(27,W),(28,B),...,(35,B),(36,W),...,(62,N),(63,N)]
 -}
defaultBoard  :: [Field]

defaultBoard  = zip [0 .. 63] (replicate 27 N ++ [W, B] ++ replicate 6 N ++ [B, W] ++ replicate 27 N)


{- isToFlip line state
   Load the field which need to flip in a list.
   RETURNS: Fields need to flip.
   EXAMPLES: isToFlip [(1,W), (2,W), (3,B)] B == [(1,W), (2,W)]
 -}
isToFlip :: [Field] -> FieldType -> [Field]

isToFlip line state = take (countNum line 0) line
  where
    countNum [] n = 0
    countNum ((_,a) : tail) n
      | a == state     = n
      | otherwise      = countNum tail (n + 1)


{- scanLine board position direction
   Scan the line of a direction.
   PRE: position must in range [0,63].
   RETURNS: Fields on the line.
   EXAMPLES: scanLine defaultBoard 26 DD == [(27,W),(28,B),(29,N)]
 -}
scanLine :: [Field] -> Int -> Scan -> [Field]
--VARIANT: distance of position to board edge.
scanLine board position direction 
  | (nextField position direction /= -1) && (\(_, a) -> a /= N) (board !! nextField position direction) 
    = (board !! nextField position direction) : scanLine board (nextField position direction) direction
  | nextField position direction /= -1 = [board !! nextField position direction]
  | otherwise = []

{- nextField board position direction
   Get next position.
   PRE: position must in range [0,63].
   RETURNS: Next position or -1 if is out of the board.
   EXAMPLES: nextField 0 WD == -1
 -}
nextField :: Int -> Scan -> Int

nextField position WW = if position - 8 >= 0
                          then position - 8
                          else -1
nextField position WA = if (div position 8 - div (position - 9) 8 == 1) && position - 8 >= 0
                          then position - 9
                          else -1
nextField position AA = if div position 8 - div (position - 1) 8 == 0
                          then position - 1
                          else -1
nextField position SA = if (div (position + 7) 8 - div position 8 == 1) && position + 7 <= 63
                          then position + 7
                          else -1
nextField position SS = if position + 8 <= 63
                          then position + 8
                          else -1 
nextField position SD = if (div (position + 9) 8 - div position 8 == 1) && position + 9 <= 63
                          then position + 9
                          else -1
nextField position DD = if div position 8 - div (position + 1) 8 == 0
                          then position + 1
                          else -1
nextField position WD = if (div position 8 - div (position - 7) 8 == 1) && position - 8 >= 0
                          then position - 7
                          else -1

{- putDownChess board position state
   Put down a chess at the position and flip within the rules.
   PRE: position must in range [0,63], state must be either B or W.
   RETURNS: New board after putting down a new chess.
   EXAMPLES: putDownChess defaultBoard 26 B == [(0,N),(1,N),...,(26,B),(27,B),(28,B),...,(35,B),(36,W),...,(62,N),(63,N)]
 -}
putDownChess :: [Field] -> Int -> FieldType -> [Field]

putDownChess board position state = 
  foldl (\b (pos, _) -> changeType (pos, state) b) board toFlip
  where
    toFlip = (position, state) : concatMap (`isToFlip` state) lines
    lines = map (scanLine board position) [WW, WA, AA, SA, SS, SD, DD, WD]

{- possibleLineMove line state n
   Find possible move position of a line.
   PRE: n must be 0, state must be either B or W.
   RETURNS: Possible move positions of a field or Nothing.
   EXAMPLES: possibleLineMove [(1,B), (2,N)] W 0 == Just 2
 -}
possibleLineMove :: [Field] -> FieldType -> Int -> Maybe Int
--VARIANT: length line.
possibleLineMove ((pos,B):tail) W n = possibleLineMove tail W (n + 1)
possibleLineMove ((pos,B):tail) B n = Nothing
possibleLineMove ((pos,W):tail) B n = possibleLineMove tail B (n + 1)
possibleLineMove ((pos,W):tail) W n = Nothing
possibleLineMove ((pos,N):tail) _ 0 = Nothing
possibleLineMove ((pos,N):tail) _ _ = Just pos
possibleLineMove _ _ _ = Nothing

{- possibleFieldMove board field state
   Find possible move positions of a field.
   PRE: State must be either B or W.
   RETURNS: Possible move positions of a field.
   EXAMPLES: possibleFieldMove defaultBoard (27, W) W == [43, 29]
 -}
possibleFieldMove :: [Field] -> Field -> FieldType -> [Int]

possibleFieldMove _ (_, N) _ = []
possibleFieldMove _ (_, B) W = []
possibleFieldMove _ (_, W) B = []
possibleFieldMove board (pos, _) state = concatMap (\line -> [fromJust (possibleLineMove line state 0) | isJust (possibleLineMove line state 0)]) lines
  where
    lines = map (scanLine board pos) [WW, WA, AA, SA, SS, SD, DD, WD]

{- possibleBoardMove board state
   Find possible move positions of a board.
   PRE: State must be either B or W.
   RETURNS: Possible move positions of a board.
   EXAMPLES: possibleBoardMove defaultBoard W == [43,29,34,20]
 -}
possibleBoardMove :: [Field] -> FieldType -> [Int]

possibleBoardMove board state = nub list
  where list = concatMap (\pos -> possibleFieldMove board (board !! pos) state) [0 .. 63]

{- niceValue board position state
   Calculate the nice value of a field.
   PRE: State must be either B or W. Positon must contains player chess.
   RETURNS: The nice value
   EXAMPLES: niceValue defaultBoard 27 W == 1
 -}
niceValue :: [Field] -> Int -> FieldType -> Int

niceValue board position state
  | position `elem` [0, 7, 56, 63] = 999
  | position `elem` [1, 6, 8, 15, 48, 55, 57, 62] = if (\(_,a) -> a == state) (board !! neighbourCorner position)
                                                     then 100
                                                     else if (\(_,a) -> a /= N) (board !! neighbourCorner position)
                                                            then 50
                                                            else -50
  | position `elem` [9, 14, 49, 54] = if (\(_,a) -> a == state) (board !! neighbourCorner position)
                                        then if (\(_,a) -> state == a) (board !! neighbourEdge position)
                                               then 20
                                               else -5
                                        else if (\(_,a) -> a /= N) (board !! neighbourCorner position)
                                               then 5
                                               else -60
  | row == 0 || row == 7 || col == 0 || col == 7 = 10
  | row == 1 || row == 6 || col == 1 || col == 6 = if (\(_,a) -> state == a) (board !! neighbourEdge position)
                                                     then 10
                                                     else 0
  | otherwise = 1
  where
    row = div position 8
    col = div position 8
    neighbourEdge position
      | col == 1  = position - 1
      | col == 6  = position + 1
      | row == 1  = position - 8
      | row == 6  = position + 8
      | otherwise = -1
    neighbourCorner position
      | position `elem` [1, 8, 9]    = 0
      | position `elem` [6, 14, 15]  = 7
      | position `elem` [48, 49, 57] = 56
      | position `elem` [54, 55, 62] = 63
      | otherwise                    = -1

{- boardValue board state
   Calculate the board value of a board.
   RETURNS: Value of a board for a field type.
   EXAMPLES: boardValue defaultBoard White == -12
 -}
boardValue :: [Field] -> FieldType -> Int

boardValue board state = foldl addAllValue 0 board - length (possibleBoardMove board (opponent state)) * 3
  where
    addAllValue n (position,a)
      | a == state            = n + niceValue board position state
      | a == opponent state   = n - niceValue board position state
      | otherwise             = n

{- minMax board state depth myState
   Minmax algorithm and judge the best move and best value.
   RETURNS: Best move and best value.
   EXAMPLES: minMax defaultBoard White 1 White == (Move 43, -12)
 -}
-- VARIANT: depth
minMax :: [Field] -> FieldType -> Int -> FieldType -> (Reversi.Move, Int)
minMax board state depth myState
  | null optionList = (Pass, boardValue board myState)
  | depth == 0 = if null optionList
                   then (Pass, boardValue board myState)
                   else (Move (head optionList), boardValue (putDownChess board (head optionList) state) myState)
  | otherwise = foldl1 judgement (zip (map Move optionList) values)
  where
    optionList = possibleBoardMove board state
    newBoard = map (\pos -> putDownChess board pos state) optionList
    values = map(snd . (\ b -> minMax b (opponent state) (depth - 1) myState)) newBoard
    judgement (bestMove, bestValue) (move, value) 
      | state == myState && value > bestValue  = (move, value)
      | state == myState && value <= bestValue = (bestMove, bestValue)
      | state /= myState && value < bestValue  = (move, value)
      | state /= myState && value >= bestValue = (bestMove, bestValue)

{- makeMove board move state
   Make one move.
   RETURNS: New board after move.
   EXAMPLES: makeMove defaultBoard (Move 43) W = [(0,N),(1,N),...,(27,B),(28,B),...,(35,W),(36,W),...,(43,W),...,(62,N),(63,N)]
 -}
makeMove :: [Field] -> Reversi.Move -> FieldType -> [Field]
makeMove board Pass _         = board
makeMove board (Move n) state = putDownChess board n state

{- initial player
   Create inital state.
   RETURNS: Inital state.
   EXAMPLES: initial Black = ([(0,N),(1,N),...,(27,B),(28,B),...,(35,W),(36,W),...,(62,N),(63,N)], B)
 -}
initial :: Reversi.Player -> State
initial Black = (defaultBoard, B)
initial White = (defaultBoard, W)

{- think state move time
   Make decision of move.
   RETURNS: Move and new state.
   EXAMPLES: think (defaultBoard, W) Pass 0.0 == (Move 34,([(0,N),(1,N),...,(27,W),(28,B),...,(34,W),(35,W),(36,W),...,(62,N),(63,N)],W))
 -}
think :: State -> Reversi.Move -> Double -> (Reversi.Move,State)

think (board, state) move time = (makeDecision, (newBoard, state))
  where
    opponentNewBoard = makeMove board move (opponent state)
    (makeDecision, _) 
      | time < 150 = minMax opponentNewBoard state 3 state
--      | time < 200 = minMax opponentNewBoard state 4 state
      | otherwise  = minMax opponentNewBoard state 4 state
    newBoard = makeMove opponentNewBoard makeDecision state
