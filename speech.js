if (typeof SpeechRecognition === 'undefined')
{
    var SpeechRecognition;
    var SpeechGrammarList;
    var SpeechRecognitionEvent;

    if (!(typeof webkitSpeechRecognition === 'undefined'))
    {
        SpeechRecognition = webkitSpeechRecognition
        SpeechGrammarList = webkitSpeechGrammarList
        SpeechRecognitionEvent = webkitSpeechRecognitionEvent
    }
}

var recognition;
var speechRecognitionList;


$(document).ready(function(){
    if (typeof SpeechRecognition === 'undefined')
    {
        return
    }

    $.ajax({ url: "grammar.jsgf", success: loadGrammar, dataType: "text", beforeSend: function ( xhr ) { xhr.overrideMimeType("text/plain; charset=x-user-defined"); } } );
    //loadGrammar("grammar.jsgf");
})



function loadGrammar(grammar)
{
    recognition = new SpeechRecognition();
    speechRecognitionList = new SpeechGrammarList();

    speechRecognitionList.addFromString(grammar, 1);

    recognition.grammars = speechRecognitionList;
    recognition.continuous = false;
    recognition.lang = 'en-US';
    recognition.interimResults = true;
    recognition.maxAlternatives = 1;

    //console.log(grammar);

    recognition.onnomatch = function(event) {
        console.log("No match: ", event.results);
    }

    recognition.onresult = function(event) {

        var text = event.results[0][0].transcript;
        $("#query").val(text);

        console.log('Confidence: ' + event.results[0][0].confidence);

        if (event.results[0].isFinal)
        {
            send();
        }
    }

    recognition.onerror = function(event) {
        console.log('Error: ', event.error);
        console.log('Error info: ', event.message);
    }
}

function speechButton(event)
{
    recognition.start();
}

