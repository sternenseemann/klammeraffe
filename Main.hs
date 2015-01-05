{-# LANGUAGE OverloadedStrings #-}
-- parenbot – irc bot witch most important purpose is to CLOSE ALL THE PARENS!
-- written by Lukas Epple aka sternenseemann
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import Network.SimpleIRC

generateClosingParens :: B.ByteString -> B.ByteString
generateClosingParens msg = calcParens msg B.empty
	where calcParens msg parenStack
		| B.null parenStack && B.null msg = ""
		| B.null msg                      = B.cons (B.head parenStack) (calcParens msg (B.tail parenStack))
		| B.head msg `B.elem` openingParens = calcParens (B.tail msg) (B.cons (convertToClosingParen (B.head msg)) parenStack)
		| not (B.null parenStack) && 
			B.head msg == B.head parenStack = calcParens (B.tail msg) (B.tail parenStack)
--		| (head msg) `elem` closingParens = "Unmatched closing paren, you fool!"
		| B.head msg `B.elem` closingParens = ""
		| otherwise                        = calcParens (B.tail msg) parenStack
		where openingParens = "([<{"
		      closingParens = ")]>}"
		      convertToClosingParen p = B.index closingParens (fromMaybe 0 (B.elemIndex p openingParens))

onPrivmsg :: EventFunc
onPrivmsg server msg 
	| not (B.null reply) = sendMsg server chan reply
	| otherwise = print msg
	where
		chan  = fromJust $ mChan msg
		reply = generateClosingParens (mMsg msg)

freenode = (mkDefaultConfig "irc.freenode.net" "klammeraffe")
			{ cChannels = ["#augsburg"],
			  cEvents   = [(Privmsg onPrivmsg)]
			}

main = connect freenode False True
