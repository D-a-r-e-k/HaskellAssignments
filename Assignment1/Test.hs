module TicTacToe.Messages.SExpr
where

{-
message to react to
board:
+-+-+-+
|X|X|X|
+-+-+-+
| |O| |
+-+-+-+
|X|O|O|
+-+-+-+
-}
message :: String
message = "(l  (m  \"x\"   0   \"y\"  2  \"v\"   \"x\")  (m \"x\"   2  \"y\"  2   \"v\"  \"o\")   (m  \"x\"  2  \"y\"   0 \"v\"   \"x\")  (m  \"x\"  2 \"y\"  1   \"v\"  \"o\")   (m   \"x\" 0   \"y\" 0  \"v\"  \"x\")   (m \"x\" 1   \"y\"   1  \"v\" \"o\") (m   \"x\" 0   \"y\"  1  \"v\" \"x\"))"