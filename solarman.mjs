import * as rts from "./rts.mjs";
import module from "./solarman.wasm.mjs";
import req from "./solarman.req.mjs";

var input_query = "mars is a planet"

function set_query(query)
{
    input_query = query;
}

async function get_query()
{
  return input_query;
}

function removeURI(namespace, s)
{
    //s.remove(namespace);
    return s.substring(namespace.length);
}

function convert_bindings2(namespace, bindings)
{
    var results = [];

    bindings.forEach(b => results.push([removeURI(namespace, b.ev.value), removeURI(namespace, b.ent.value)]));

    return results;
}

function convert_bindings3(namespace, bindings)
{
    var results = [];

    bindings.forEach(b => results.push([removeURI(namespace, b.ev.value), removeURI(namespace, b.prop.value), removeURI(namespace, b.ent.value)]));

    return results;
}

async function getts_triples_members(endpoint, set)
{
/*
query = do
          sol <- prefix "sol" (iriRef namespace_uri)
          ev <- var
          ent <- var
          triple ev (sol .:. "type") (sol .:. "membership")
          triple ev (sol .:. "subject") ent
          triple ev (sol .:. "object") (sol .:. set)
          selectVars [ev, ent]

    WORKING:

PREFIX sol: <http://solarman.richard.myweb.cs.uwindsor.ca#>

SELECT ?ev ?ent WHERE {
   ?ev sol:type sol:membership .
   ?ev sol:subject ?ent .
   ?ev sol:object sol:moon .
}

*/
    var query = "PREFIX sol: <$namespace> \
     SELECT ?ev ?ent WHERE { \
        ?ev sol:type sol:membership .\
        ?ev sol:subject ?ent . \
        ?ev sol:object sol:$set .\
    }";

    query = query.replace("$namespace", endpoint.sparqlNamespace);
    query = query.replace("$set", set);

    //console.log("endpoint:", endpoint);
    //console.log("set:", set);
    //console.log("query:", query);

    //var headers = new Headers();
    //headers.append("", "application/sparql-results+json");

    var formData = new FormData();
    formData.append("query", query);
    formData.append("format", "application/sparql-results+json");

    return fetch(endpoint.sparqlEndpoint, { method: "POST", body: formData })
        .then(response => response.json())
        .then(json => convert_bindings2(endpoint.sparqlNamespace, json.results.bindings));

}

//This function is an improvement over the hsparql version because it uses the IN filter!
async function getts_triples_entevprop_type(endpoint, props, ev_type)
{
/*
        query :: Query SelectQuery
        query = do
          sol <- prefix "sol" (iriRef namespace_uri)
          ev <- var
          prop <- var
          ent <- var
          triple ev prop ent
          triple ev (sol .:. "type") (sol .:. ev_type)
          filterExpr $ List.foldr1 (.||.) $ map ((prop .==.) . (sol .:. )) propNames --type required here as this is not an FDBR
          selectVars [ev, prop, ent]

          //SPECIAL: (?prop IN (sol:subject, sol:object,sol:implement))}
*/
//TODO: the below expects non-empty props, does this make sense??? it should be reasonable to expct.
    var query = "PREFIX sol: <$namespace> \
    SELECT ?ev ?prop ?ent WHERE { \
    ?ev ?prop ?ent .\
    ?ev sol:type sol:$ev_type .\
    FILTER (?prop IN (";

    query = query.replace("$namespace", endpoint.sparqlNamespace);
    query = query.replace("$ev_type", ev_type);

    //must build rest of query!

    //var allowed_props = [];

    //props.forEach(prop => allowed_props.push("?prop = sol:" + prop));

    var filterExpr = "sol:" + props[0];
    for (var i = 1; i < props.length; ++i)
    {
        filterExpr += ", "
        filterExpr += "sol:" + props[i];
    }

    query += filterExpr;

    query += "))}";

    //console.log("endpoint:", endpoint);
    //console.log("set:", set);
    //console.log("query:", query);

    //var headers = new Headers();
    //headers.append("", "application/sparql-results+json");

    var formData = new FormData();
    formData.append("query", query);
    formData.append("format", "application/sparql-results+json");

    return fetch(endpoint.sparqlEndpoint, { method: "POST", body: formData })
    .then(response => response.json())
    .then(json => convert_bindings3(endpoint.sparqlNamespace, json.results.bindings));
}

//This function also takes advantage of IN filter
async function getts_cardinality_allents(endpoint, props)
{
 /*
    PREFIX : <http://solarman.richard.myweb.cs.uwindsor.ca#>

    select distinct (count(?ent) AS ?count) where {

        ?ev ?prop ?ent .

        FILTER (?prop IN ( :with_implement, :subject, :object ))

    }

    query :: Query SelectQuery
    query = do
        sol <- prefix "sol" (iriRef namespace_uri)
        ev <- var
        prop <- var
        ent <- var
        triple ev prop ent
        filterExpr $ List.foldr1 (.||.) $ map ((prop .==.) . (sol .:. )) propNames --type required here as this is not an FDBR
        c <- count ent
        distinct
        select [c]
 */

    var query = "PREFIX sol: <$namespace> \
    SELECT distinct (count(?ent) AS ?count) WHERE { \
    ?ev ?prop ?ent .\
    FILTER (?prop IN (";

    var filterExpr = "sol:" + props[0];
    for (var i = 1; i < props.length; ++i)
    {
        filterExpr += ", "
        filterExpr += "sol:" + props[i];
    }

    query += filterExpr;
    query += "))}";

    var formData = new FormData();
    formData.append("query", query);
    formData.append("format", "application/sparql-results+json");

    return fetch(endpoint.sparqlEndpoint, { method: "POST", body: formData })
    .then(response => response.json())
    .then(json => parseInt(json.results.bindings[0].count.value));

}

async function getts_sparql(endpoint, query)
{
  console.log("endpoint: " + endpoint);
  console.log("query: " + query);

  //var headers = new Headers();
  //headers.append('Access-Control-Allow-Headers', 'Access-Control-Allow-Origin');

  return fetch(endpoint/*, { headers: headers }*/).then(async response => response.text());
}

async function update_results(results_string)
{
    var data = JSON.parse(results_string);

    if (Array.isArray(data))
    {
        $("#result").html("");
        data.forEach(function(item, index) {
                var builder = "";
                builder += "<b>result:</b> " + item.res + "<br/>";
                builder += "<b>syntax:</b> " + item.syntax + "<br/>";
                builder += "<br/>";
                $("#result").append(builder);

                if (!(typeof window.speechSynthesis === 'undefined'))
                {
                    if (data.length == 1)
                    {
                        var utterThis = new SpeechSynthesisUtterance(item.res);
                        window.speechSynthesis.speak(utterThis);
                    }
                    else
                    {
                        var utterThis = new SpeechSynthesisUtterance("result " + (index + 1) + ": " + item.res + ".");
                        window.speechSynthesis.speak(utterThis);
                    }
                }
            });
    }
    else if (!(typeof data.resError === 'undefined'))
    {
        $("#querygroup").addClass("has-danger");
        $("#query").addClass("form-control-danger");
        $("#result").html(data.resError);
        $("#query").addClass("is-invalid");

        if (!(typeof window.speechSynthesis === 'undefined'))
        {
            var utterThis = new SpeechSynthesisUtterance(data.resError);
            window.speechSynthesis.speak(utterThis);
        }
    }
    else if (!(typeof data.resConversation === 'undefined'))
    {
        var res = data.resConversation;
        $("#result").html(res);

        if (!(typeof window.speechSynthesis === 'undefined'))
        {
            var utterThis = new SpeechSynthesisUtterance(res);
            window.speechSynthesis.speak(utterThis);
        }
    }
}

module.then(m => rts.newAsteriusInstance(Object.assign(req, {module: m}))).then(async i => {
  //i.exports.hs_init();
  //i.exports.main().catch(err => {if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) i.fs.writeSync(2, `solarman: ${err}`)});
  //console.log(await i.exports.parse_query(1));
  //const r = i.exports.parse_query("Hello world");
  window.asterius = i;
  window.get_query = get_query;
  window.getts_sparql = getts_sparql;
  window.getts_triples_members = getts_triples_members;
  window.getts_triples_entevprop_type = getts_triples_entevprop_type;
  window.getts_cardinality_allents = getts_cardinality_allents
  window.set_query = set_query;
  window.update_results = update_results;
  //console.log(i);
  //i.exports.main();
});
