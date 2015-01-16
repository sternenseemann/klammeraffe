{-# LANGUAGE OverloadedStrings #-}
-- parenbot – irc bot witch most important purpose is to CLOSE ALL THE PARENS!
-- written by Lukas Epple aka sternenseemann
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B
import Network.SimpleIRC
import Test.QuickCheck

-- configuration
botNick :: String
botNick = "klammeraffe"

freenode :: IrcConfig
freenode = (mkDefaultConfig "irc.freenode.net" botNick)
           { cChannels = ["#augsburg"],
               cEvents   = [(Privmsg onPrivmsg)]
           }

openingParens :: String
closingParens :: String
openingParens = "([<{\"„“‚⟦⟨⟪〚⁅〈⎴⏞⏠❬❰❲❴⦃⦗⧼⸦〈《【〔〖〘"
closingParens = ")]>}\"“”‘⟧⟩⟫〛⁆〉⎵⏟⏡❭❱❳❵⦄⦘⧽⸧〉》】〕〗〙"

-- busywork
generateClosingParens :: B.ByteString -> B.ByteString
generateClosingParens msg = B.pack (calcParens (B.unpack msg) "")

calcParens :: String -> String -> String
calcParens msg parenStack
    | null parenStack && null msg   = ""
    | null msg                        = head parenStack : calcParens msg (tail parenStack)
    | not (null parenStack) &&
      head msg == head parenStack = calcParens (tail msg) (tail parenStack)
    | head msg `elem` openingParens   = calcParens (tail msg) (convertToClosingParen (head msg) : parenStack)
    | head msg `elem` closingParens   = ""
    | otherwise                         = calcParens (tail msg) parenStack
    where
          convertToClosingParen :: Char -> Char
          convertToClosingParen p = closingParens !! fromMaybe 0 (elemIndex p openingParens)

onPrivmsg :: EventFunc
onPrivmsg server msg
  | not (B.null reply) = sendMsg server chan reply
  | B.pack botNick `B.isPrefixOf` mMsg msg = sendMsg server chan (B.append nick ": I only fix your unclosed parens.")
  | otherwise = print msg
  where nick  = fromJust $ mNick msg
        chan  = fromJust $ mChan msg
        reply = generateClosingParens (mMsg msg)

main = connect freenode False True

-- tests
prop_AtLeastAsManyClosingParens inp =
  foldr1 (&&) $ map allParensClosed $ zip openingParens closingParens
    where
      allParensClosed :: (Char, Char) -> Bool
      allParensClosed (l, r) = number l completeStr <= number r completeStr || oneElem closingParens completeStr -- if there are unmatched parens calcParens just returns ""
      completeStr = inp ++ calcParens inp "" 
      number c str = length (elemIndices c str)
      oneElem elems str = foldl (\acc x -> x `elem` str || acc) False elems

tests = quickCheck prop_AtLeastAsManyClosingParens
