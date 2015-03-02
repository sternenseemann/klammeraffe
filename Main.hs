{-# LANGUAGE OverloadedStrings #-}
-- parenbot – irc bot witch most important purpose is to CLOSE ALL THE PARENS!
-- written by Lukas Epple aka sternenseemann
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B
import Network.SimpleIRC
import Test.QuickCheck

data ParenState = Normal | ParenIgnore
  deriving (Show, Eq)

-- configuration
botNick :: String
botNick = "klammeraffe"

freenode :: IrcConfig
freenode = (mkDefaultConfig "irc.freenode.net" botNick)
           { cChannels = ["#augsburg"],
               cEvents   = [Privmsg onPrivmsg]
           }

openingParens :: String
closingParens :: String
openingParens = "([{\"„“‚⟦⟨⟪〚⁅〈⎴⏞⏠❬❰❲❴⦃⦗⧼⸦〈《【〔〖〘"
closingParens = ")]}\"“”‘⟧⟩⟫〛⁆〉⎵⏟⏡❭❱❳❵⦄⦘⧽⸧〉》】〕〗〙"

smileOpeningChars :: String
smileOpeningChars = ":;8"

smileContinuation :: String
smileContinuation = "-"

-- busywork
generateClosingParens :: B.ByteString -> B.ByteString
generateClosingParens msg = B.pack $ calcParens (B.unpack msg) "" Normal

calcParens :: String -> String -> ParenState -> String
calcParens msg parenStack state
    | null parenStack && null msg       = ""
    | null msg                          = head parenStack : calcParens msg (tail parenStack) state
    | head msg `elem` smileOpeningChars = calcParens (tail msg) parenStack ParenIgnore
    | state == ParenIgnore              = if head msg `elem` smileContinuation
                                            then calcParens (tail msg) parenStack ParenIgnore
                                            else calcParens (tail msg) parenStack Normal
    | not (null parenStack) &&
      head msg == head parenStack       = calcParens (tail msg) (tail parenStack) state
    | head msg `elem` openingParens     = calcParens (tail msg) (convertToClosingParen (head msg) : parenStack) state
    | head msg `elem` closingParens     = ""
    | otherwise                         = calcParens (tail msg) parenStack state
    where
          convertToClosingParen :: Char -> Char
          convertToClosingParen p = closingParens !! fromMaybe 0 (elemIndex p openingParens)

onPrivmsg :: EventFunc
onPrivmsg server msg
  | not $ B.null reply = sendMsg server chan reply
  | B.pack botNick `B.isPrefixOf` mMsg msg = sendMsg server chan $ B.append nick ": I only fix your unclosed parens."
  | otherwise = print msg
  where nick  = fromJust $ mNick msg
        chan  = fromJust $ mChan msg
        reply = generateClosingParens (mMsg msg)

main = connect freenode False True

-- tests
prop_AtLeastAsManyClosingParens inp =
  and $ zipWith allParensClosed openingParens closingParens
    where
      allParensClosed :: Char ->  Char -> Bool
      allParensClosed l r = number l completeStr <= number r completeStr || oneElem smileOpeningChars completeStr || oneElem closingParens completeStr -- if there are unmatched parens calcParens just returns "", smile recognition also does unexpected things
      completeStr = inp ++ calcParens inp "" Normal
      number c str = length (elemIndices c str)
      oneElem elems str = foldl (\acc x -> x `elem` str || acc) False elems

tests = quickCheck prop_AtLeastAsManyClosingParens
