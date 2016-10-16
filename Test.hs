module TicTacToe.Messages.SExpr
where

{-
message to react to
board:
+-+-+-+
| |X| |
+-+-+-+
|O| | |
+-+-+-+
|O|X| |
+-+-+-+
-}
message :: String
message = "(l   (m  \"x\" 0   \"y\" 1   \"v\"   \"x\")   (m  \"x\"   1 \"y\"   0  \"v\"  \"o\")  (m \"x\"  2   \"y\" 1   \"v\" \"x\")  (m  \"x\"  2  \"y\"  0  \"v\"  \"o\"))"