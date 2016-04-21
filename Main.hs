#!/opt/csw/bin/runhaskell
module Main where
import qualified SolarmanTriplestore as App
import Text.PrettyPrint.HughesPJ
import Network.CGI

cgiMain :: CGI CGIResult
cgiMain = do
    (Just input) <- getInput "query"
    out <- liftIO $ interpret' input
    output out


main :: IO ()
main = runCGI (handleErrors cgiMain)

interpret :: String -> String
interpret "ask them to be quiet" 
     = "Hello. Quiet please. My "
       ++ "masters are going to talk. Quiet please."

interpret "introduce yourself solar man" 
     =  "Hello. Hello. My name is Solar man."
        ++ " Thank you for coming to my party."
        ++ " I am very pleased to meet you."

interpret "what can i say" 
      = "You can say. hello there. what is your name."
        ++ " you can ask me about the moons and the planets."
    ++ " such as, who discovered a moon."
    ++ " who discovered two moons."
    ++ " which moons were discovered by kuiper."
    ++ " who discovered phobos."
    ++ " which planet is orbited by miranda."
    ++ " how many moons orbit saturn."
    ++ " and other similar questions."
        ++ " who are you. where do you live."
        ++ " tell me a joke. who made you." 
        ++ " who do you know. what is your favorite band."
        ++ " who is the vice president at the university of windsor." 
        ++ " who is the president at the university of windsor." 
        ++ " who is the dean of science at the university of windsor."

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
     ++ " moons, and the people who discovered them"
interpret "where do you live" 
    = "I live in a dark cold computer. "
       ++ "The center of my universe is Lambton Tower, at the University of Windsor."
interpret "what do you know" 
   = "Not much I am afraid. I am just beginning to learn. I know a bit about "
     ++ "the planets, the moons, and the people who discovered them. "
     ++ "My master will teach me some more when he gets another grant "
interpret "how old are you"
  = "older than you think. And much older than my friends Judy and Monty."
interpret "who made you" 
   = "I. B. M. and Opera Software made my ears and vocal chords. William Ma connected my "
     ++ "ears to my brain, and Doctor Frost, master of the universe, made "
     ++ "my brain"

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
      ++ "was a car. He had a hard drive home every day"

interpret "know any jokes" 
       = "just one. My friend Monty knows one too."

interpret "who is judy" = "She is my friend. She knows about poetry"

interpret "who is monty"   
   = "Monty is my friend. He is a student"
     ++ " at the university of Windsor."
interpret "can I talk to judy" 
              ="yes. here she is"
                ++ "<goto>http://cs.uwindsor.ca/~speechweb/p_d_speechweb/judy/judy.xml</goto>"

interpret "can I talk to monty" 
              ="yes. here he is"
                ++ "<goto>http://cs.uwindsor.ca/~speechweb/p_d_speechweb/monty/monty.xml</goto>"

interpret "can I talk to solar man" 
              ="yes. here he is"
                ++ "<goto>http://cs.uwindsor.ca/~speechweb/p_d_speechweb/solarman/solarman.xml</goto>"

interpret "who do you know" = "i only know three people. Judy, Monty, and Solarman."

interpret _ = "BLANKVALNOTUSED"

interpret' input = do
    let firstpass = interpret input
    if firstpass == "BLANKVALNOTUSED" then do
        output <- App.parse input
        let formatted = render $ vcat $ output
        if null formatted then return "Do not know that one yet, will work on it tonight" else return $ formatted
    else return firstpass


