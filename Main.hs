{-# LANGUAGE OverloadedStrings #-}
-- parenbot – irc bot witch most important purpose is to CLOSE ALL THE PARENS!
-- written by Lukas Epple aka sternenseemann
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.SimpleIRC

botNick :: String
botNick = "klammeraffe"
freenode = (mkDefaultConfig "irc.freenode.net" botNick)
           { cChannels = ["#augsburg"],
               cEvents   = [(Privmsg onPrivmsg)]
           }


generateClosingParens :: B.ByteString -> B.ByteString
generateClosingParens msg = E.encodeUtf8 (calcParens (E.decodeUtf8 msg) T.empty)
  where calcParens :: T.Text -> T.Text -> T.Text
        calcParens msg parenStack
          | T.null parenStack && T.null msg   = ""
          | T.null msg                        = T.cons (T.head parenStack) (calcParens msg (T.tail parenStack))
          | not (T.null parenStack) &&
            T.head msg == T.head parenStack = calcParens (T.tail msg) (T.tail parenStack)
          | T.head msg `elem` openingParens   = calcParens (T.tail msg) (T.cons (convertToClosingParen (T.head msg)) parenStack)
          | T.head msg `elem` closingParens   = ""
          | otherwise                         = calcParens (T.tail msg) parenStack
          where openingParens = "([<{\"\'„“‚⟦⟨⟪〚⁅〈⎴⏞⏠❬❰❲❴⦃⦗⧼⸦〈《【〔〖〘"
                closingParens = ")]>}\"\'“”‘⟧⟩⟫〛⁆〉⎵⏟⏡❭❱❳❵⦄⦘⧽⸧〉》】〕〗〙"
                convertToClosingParen p = T.index closingParens (fromMaybe 0 (T.findIndex (== p) openingParens))
                elem :: Char -> T.Text -> Bool
                elem c str = isJust (T.findIndex (== c) str)

onPrivmsg :: EventFunc
onPrivmsg server msg
  | not (B.null reply) = sendMsg server chan reply
  | B.isPrefixOf (B.pack botNick) (mMsg msg) = sendMsg server chan (B.append nick ": I only fix your unclosed parens.")
  | otherwise = print msg
  where nick  = fromJust $ mNick msg
        chan  = fromJust $ mChan msg
        reply = generateClosingParens (mMsg msg)


main = connect freenode False True
