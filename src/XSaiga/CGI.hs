{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module XSaiga.CGI where
import qualified XSaiga.SolarmanTriplestore as App
import Network.FastCGI
import qualified Data.List as List
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BLIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Text.Encoding as E
import Data.Text.Lazy.Encoding as EL

#ifdef INSTORE
import qualified XSaiga.LocalData as Local
#endif

#ifndef INSTORE
--For caching name lookup
import qualified Network.Socket as Net
#endif

import qualified XSaiga.Getts as Getts
import qualified XSaiga.TypeAg2 as TypeAg2
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import qualified XSaiga.ShowText as ShowText

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as AesonText

import Data.Maybe

import GHC.Generics

data XSaigaResult = XSaigaResult {
    res :: T.Text,
    syntax :: T.Text
} deriving (Generic, Show)

data XSaigaConversationResult = XSaigaConversationResult {
    resConversation :: T.Text
} deriving (Generic, Show)

data XSaigaParseError = XSaigaParseError {
    resError :: T.Text
} deriving (Generic, Show)

instance Aeson.ToJSON XSaigaResult where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON XSaigaConversationResult where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON XSaigaParseError where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

cgiMain :: (Getts.TripleStore m) => m -> CGI CGIResult
cgiMain dataStore = do
    query <- getInputFPS "query"
    case query of
      Nothing -> outputFPS $ EL.encodeUtf8 "error"
      Just input -> do
        out <- liftIO $ interpret' dataStore (E.decodeUtf8 $ BL.toStrict input)
        setHeader "Content-type" "text/plain; charset=utf-8"
        outputFPS out

remoteData = Getts.SPARQL endpoint_uri namespace_uri
    where
        endpoint_uri = "http://speechweb2.cs.uwindsor.ca:8890/sparql"
        namespace_uri = "http://solarman.richard.myweb.cs.uwindsor.ca#"

--Inside an #ifdef to avoid the network-based dependencies, helps keep size down for completely offline builds
main :: IO ()
#ifdef INSTORE
main = do
    runFastCGIorCGI (handleErrors $ cgiMain Local.localData) --No need to resolve anything
#else
main = do
    resolved_endpoint <- resolveEndpoint remoteData
    runFastCGIorCGI (handleErrors $ cgiMain resolved_endpoint)
    where
        resolveEndpoint (Getts.SPARQL url namespace_uri) = do
            x <- Net.getAddrInfo Nothing (Just $ getServer url) (Just "http")
            return $ Getts.SPARQL (newURL (showAddress x) (getURLPath url)) namespace_uri
            where
                getServer = List.takeWhile (\x -> '/' /= x) . List.drop 7 --drop the "http://" part and take until the first "/" character
                showAddress = show . Net.addrAddress . List.head
                getURLPath xs = List.drop (7 + (List.length $ getServer xs)) xs
                newURL server path = "http://" ++ server ++ path
#endif

interpret "ask them to be quiet"
     = Just $ "Hello. Quiet please. My "
       `T.append` "masters are going to talk. Quiet please."

interpret "introduce yourself solar man"
     =  Just $ "Hello. Hello. My name is Solar man."
        `T.append` " Thank you for coming to my party."
        `T.append` " I am very pleased to meet you."

interpret "what can i say"
      = Just $ "You can say. hello there. what is your name."
        `T.append` " you can ask me about the moons and the planets."
    `T.append` " such as, who discovered a moon."
    `T.append` " who discovered two moons."
    `T.append` " which moons were discovered by kuiper."
    `T.append` " who discovered phobos."
    `T.append` " which planet is orbited by miranda."
    `T.append` " how many moons orbit saturn."
    `T.append` " and other similar questions."
        `T.append` " who are you. where do you live."
        `T.append` " tell me a joke. who made you."
        `T.append` " who do you know. what is your favorite band."
        `T.append` " who is the vice president at the university of windsor."
        `T.append` " who is the president at the university of windsor."
        `T.append` " who is the dean of science at the university of windsor."

interpret "hi"              = Just $ "Hi there. My name is solar man"
interpret "hello"           = Just $ "hello. My name is solar man."
interpret "hello there"     = Just $ "Good day to you"
interpret "hello solar man" = Just $ "hello. How are you"
interpret "goodbye"         = Just $ "goodbye. See you in the stars."
interpret "goodbye solar man" = interpret "goodbye"
interpret "fine thanks" = Just $ "Good, so am I. Except for a bit of back ache"
interpret "thanks" = Just $ "you are welcome"
interpret "thanks solar man" = Just $ "you are most welcome"
interpret "yes please" = Just $ "yes please? What did you want? My memory is getting bad"
interpret "what is your name" = Just $ "My name is solar man."
interpret "who are you"
   = Just $ "My name is solar man. I know about the planets and the"
     `T.append` " moons, and the people who discovered them"
interpret "where do you live"
    = Just $ "I live in a dark cold computer. "
       `T.append` "The center of my universe is Lambton Tower, at the University of Windsor."
interpret "what do you know"
   = Just $ "Not much I am afraid. I am just beginning to learn. I know a bit about "
     `T.append` "the planets, the moons, and the people who discovered them. "
     `T.append` "My master will teach me some more when he gets another grant "
interpret "how old are you"
  = Just $ "older than you think. And much older than my friends Judy and Monty."
interpret "who made you"
   = Just $ "I. B. M. and Opera Software made my ears and vocal chords. William Ma connected my "
     `T.append` "ears to my brain, and Doctor Frost, master of the universe, made "
     `T.append` "my brain"

interpret "what is your favorite band"
   = Just $ "Pink Floyd. I love, Dark Side of the Moon"

interpret "who is the vice president at the university of windsor"
  =Just $ "Douglas Kneale"

interpret "who is the president at the university of windsor"
  = Just $ "Doctor Robert Gordon."

interpret "who is the dean of science at the university of windsor"
  = Just $ "Doctor Chris Houser"

interpret "tell me a poem" = Just $ "do not know any poems. But my friend, Judy, does"


interpret "know any poems" = Just $ "no but my friend, Judy does;"


interpret "tell me a joke"
    = Just $ "did you hear about the Computer Scientist who thought his computer"
      `T.append` "was a car. He had a hard drive home every day"

interpret "know any jokes"
       = Just $ "just one. My friend Monty knows one too."

interpret "who is judy" = Just $ "She is my friend. She knows about poetry"

interpret "who is monty"
   = Just $ "Monty is my friend. He is a student"
     `T.append` " at the university of Windsor."

{-
interpret "can I talk to judy"
              ="yes. here she is"
                `T.append` "<goto>http://cs.uwindsor.ca/~speechweb/p_d_speechweb/judy/judy.xml</goto>"

interpret "can I talk to monty"
              ="yes. here he is"
                `T.append` "<goto>http://cs.uwindsor.ca/~speechweb/p_d_speechweb/monty/monty.xml</goto>"

interpret "can I talk to solar man"
              ="yes. here he is"
                `T.append` "<goto>http://cs.uwindsor.ca/~speechweb/p_d_speechweb/solarman/solarman.xml</goto>"

-}

interpret "who do you know" = Just $ "i only know three people. Judy, Monty, and Solarman."

interpret _ = Nothing

interpret'' = TypeAg2.getQUVAL . List.head . App.parse

--TODO: multiple interpretations!  need to optimize these!
interpret' dataStore input = do
    let firstpass = interpret input
    case firstpass of
        Nothing -> do
            let attTrees = App.parseTree input
            let atts = Prelude.map fst attTrees
            let trees = Prelude.map snd attTrees
            let sems = List.map TypeAg2.getQUVAL atts
            --outs <- mapM evaluate interpretations --TODO: this is a code smell -- needs to be abstracted -- looks like SemFunc
            let flatQueries = Prelude.foldr mergeFlat ([],[]) sems
            let optQueries = TypeAg2.flatOptimize flatQueries
            rtriples <- TypeAg2.getReducedTriplestore dataStore optQueries
            (outs, _) <- M.foldM (nextInterp rtriples) ([], Map.empty) sems --TODO: save the state for later?  paper opportunity
            if List.null attTrees
                then return $ Aeson.encode $ XSaigaParseError "Do not know that one yet, will work on it tonight"
                else return $ Aeson.encode $ List.zipWith XSaigaResult outs trees
        Just result -> return $ Aeson.encode $ XSaigaConversationResult result
    where
        mergeFlat interp flatGetts = let g = TypeAg2.getGetts interp in TypeAg2.merge (TypeAg2.flattenGetts g) flatGetts
        nextInterp rtriples (txt, state) interp = do --TODO: improves by about 2 seconds on heavy workloads -- could be better!
            (out, nState) <- evaluate rtriples interp state
            return (txt ++ [out], nState)

evaluate rtriples interp startState = do
  return $ State.runState (TypeAg2.getSem interp rtriples) startState

--TODO: needs to use resolved_endpoint URI?  or refactored as LocalData/SPARQL/etc
runQuery dataStore interp = do
    let g = TypeAg2.getGetts interp
    let flatQueries = TypeAg2.flattenGetts g
    let optQueries = TypeAg2.flatOptimize flatQueries
    rtriples <- TypeAg2.getReducedTriplestore dataStore optQueries
    (out, _) <- evaluate rtriples interp Map.empty
    return out

interpret''' dataStore input = interpret' dataStore input >>= BLIO.putStrLn