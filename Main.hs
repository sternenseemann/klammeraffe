{-# LANGUAGE OverloadedStrings #-}
-- parenbot – irc bot witch most important purpose is to CLOSE ALL THE PARENS!
-- written by Lukas Epple aka sternenseemann
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.SimpleIRC
import Test.QuickCheck

botNick :: String
botNick = "klammeraffe"
freenode = (mkDefaultConfig "irc.freenode.net" botNick)
           { cChannels = ["#augsburg"],
               cEvents   = [(Privmsg onPrivmsg)]
           }

openingParens = "([<{\"\'„“‚⟦⟨⟪〚⁅〈⎴⏞⏠❬❰❲❴⦃⦗⧼⸦〈《【〔〖〘"
closingParens = ")]>}\"\'“”‘⟧⟩⟫〛⁆〉⎵⏟⏡❭❱❳❵⦄⦘⧽⸧〉》】〕〗〙"

generateClosingParens :: B.ByteString -> B.ByteString
generateClosingParens msg = E.encodeUtf8 (calcParens (E.decodeUtf8 msg) T.empty)

calcParens :: T.Text -> T.Text -> T.Text
calcParens msg parenStack
    | T.null parenStack && T.null msg   = ""
    | T.null msg                        = T.cons (T.head parenStack) (calcParens msg (T.tail parenStack))
    | not (T.null parenStack) &&
      T.head msg == T.head parenStack = calcParens (T.tail msg) (T.tail parenStack)
    | T.head msg `elem` openingParens   = calcParens (T.tail msg) (T.cons (convertToClosingParen (T.head msg)) parenStack)
    | T.head msg `elem` closingParens   = ""
    | otherwise                         = calcParens (T.tail msg) parenStack
    where
          convertToClosingParen p = T.index closingParens (fromMaybe 0 (T.findIndex (== p) openingParens))
          elem :: Char -> T.Text -> Bool
          elem c str = isJust (T.findIndex (== c) str)

prop_AtLeastAsManyClosingParens inp =
  foldr1 (&&) $ map allParensClosed $ zip (spl openingParens) (spl closingParens)
    where
      spl = T.chunksOf 1
      allParensClosed :: (T.Text, T.Text) -> Bool
      allParensClosed (l, r) = number l completeStr <= number r completeStr
      completeStr = inp `T.append` calcParens inp T.empty
      number c str = T.count c str

onPrivmsg :: EventFunc
onPrivmsg server msg
  | not (B.null reply) = sendMsg server chan reply
  | B.isPrefixOf (B.pack botNick) (mMsg msg) = sendMsg server chan (B.append nick ": I only fix your unclosed parens.")
  | otherwise = print msg
  where nick  = fromJust $ mNick msg
        chan  = fromJust $ mChan msg
        reply = generateClosingParens (mMsg msg)

instance Arbitrary T.Text where
  arbitrary = fmap T.pack $ listOf $ oneof [choose ('a', 'z'), elements "([<{\"\'"]
    where
      parens :: String
      parens = T.unpack openingParens ++ T.unpack closingParens

main = connect freenode False True
tests = quickCheck prop_AtLeastAsManyClosingParens
