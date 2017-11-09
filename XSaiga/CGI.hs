{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

module XSaiga.CGI where
import qualified XSaiga.SolarmanTriplestore as App
import Network.CGI
import qualified Data.List as List
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding as E
--import qualified XSaiga.LocalData as Local
import qualified XSaiga.Getts as Getts
import qualified XSaiga.TypeAg2 as TypeAg2

--change between remoteData and localData
--dataStore = Local.localData
dataStore = remoteData -- selects database

endpoint_uri = "http://speechweb2.cs.uwindsor.ca/sparql"
namespace_uri = T.pack "http://solarman.richard.myweb.cs.uwindsor.ca#"
remoteData = Getts.SPARQL endpoint_uri namespace_uri

cgiMain :: CGI CGIResult
cgiMain = do
    (Just input) <- getInputFPS "query"
    out <- liftIO $ interpret' $ E.decodeUtf8 $ BL.toStrict input
    setHeader "Content-type" "text/plain; charset=utf-8"
    outputFPS $ BL.fromStrict $ E.encodeUtf8 $ out


main :: IO ()
main = runCGI (handleErrors cgiMain)

interpret "ask them to be quiet" 
     = "Hello. Quiet please. My "
       `T.append` "masters are going to talk. Quiet please."

interpret "introduce yourself solar man" 
     =  "Hello. Hello. My name is Solar man."
        `T.append` " Thank you for coming to my party."
        `T.append` " I am very pleased to meet you."

interpret "what can i say" 
      = "You can say. hello there. what is your name."
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

interpret "hi"              = "Hi there. My name is solar man"
interpret "hello"           = "hello. My name is solar man."
interpret "hello there"     = "Good day to you"
interpret "hello solar man" = "hello. How are you"
interpret "goodbye"         = "goodbye. See you in the stars."
interpret "goodbye solar man" = interpret "goodbye"
interpret "fine thanks" = "Good, so am I. Except for a bit of back ache"
interpret "thanks" = "you are welcome"
interpret "thanks solar man" = "you are most welcome"
interpret "yes please" = "yes please? What did you want? My memory is getting bad"
interpret "what is your name" = "My name is solar man."
interpret "who are you" 
   = "My name is solar man. I know about the planets and the"
     `T.append` " moons, and the people who discovered them"
interpret "where do you live" 
    = "I live in a dark cold computer. "
       `T.append` "The center of my universe is Lambton Tower, at the University of Windsor."
interpret "what do you know" 
   = "Not much I am afraid. I am just beginning to learn. I know a bit about "
     `T.append` "the planets, the moons, and the people who discovered them. "
     `T.append` "My master will teach me some more when he gets another grant "
interpret "how old are you"
  = "older than you think. And much older than my friends Judy and Monty."
interpret "who made you" 
   = "I. B. M. and Opera Software made my ears and vocal chords. William Ma connected my "
     `T.append` "ears to my brain, and Doctor Frost, master of the universe, made "
     `T.append` "my brain"

interpret "what is your favorite band"
   = "Pink Floyd. I love, dark side of the moon"

interpret "who is the vice president at the university of windsor" 
  = "Leo Groarke"

interpret "who is the president at the university of windsor" 
  = "Doctor Alan Wildeman."

interpret "who is the dean of science at the university of windsor"
  = "Doctor Marlys Koschinsky"

interpret "tell me a poem" = "do not know any poems. But my friend, Judy, does"


interpret "know any poems" = "no but my friend, Judy does;"


interpret "tell me a joke" 
    = "did you hear about the Computer Scientist who thought his computer"
      `T.append` "was a car. He had a hard drive home every day"

interpret "know any jokes" 
       = "just one. My friend Monty knows one too."

interpret "who is judy" = "She is my friend. She knows about poetry"

interpret "who is monty"   
   = "Monty is my friend. He is a student"
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

interpret "who do you know" = "i only know three people. Judy, Monty, and Solarman."

interpret _ = "BLANKVALNOTUSED"

interpret'' = TypeAg2.getQUVAL . List.head . App.parse

--TODO: multiple interpretations!  need to optimize these!
interpret' input = do
    let firstpass = interpret input
    if firstpass == "BLANKVALNOTUSED" then do
        let interpretations = List.map TypeAg2.getQUVAL $ App.parse input
        outs <- mapM evaluate interpretations --TODO: this is a code smell -- needs to be abstracted -- looks like SemFunc
        let formatted = T.concat $ List.intersperse " ; " outs
        if T.null formatted then return "Do not know that one yet, will work on it tonight" else return $ formatted
    else return firstpass

evaluate (sem, getts) = do
  rtriples <- TypeAg2.getReducedTriplestore remoteData (TypeAg2.flattenGetts getts)
  return $ sem rtriples
