#!/usr/bin/python3

import sys
import json, requests
from rdflib import Graph
from rdflib.namespace import Namespace, NamespaceManager
from rdflib.term import URIRef

synsetProperties = """
prefix schema: <https://br.ibm.com/tkb/own-en/schema/>

select ?p { ?p rdfs:domain schema:Synset . }
"""


class OwnEn:
    
    def __init__(self,endpoint = 'http://localhost:8000/sparql'):
        self.endpoint = endpoint
        self.namespaceManager = NamespaceManager(Graph())
        self.namespaceManager.bind('schema', Namespace('https://br.ibm.com/tkb/own-en/schema/'), override=False)
        self.namespaceManager.bind('nomlex', Namespace('https://br.ibm.com/tkb/own-en/nomlex/'), override=False)
        self.namespaceManager.bind('inst', Namespace('https://br.ibm.com/tkb/own-en/instances/'), override=False)

    def query_synsets(self, query):
        response = requests.post(self.endpoint,
                                 headers = {'Accept': 'application/sparql-results+json'},
                                 data = {'query': query})
        values = [x['s']['value'] for x in response.json()['results']['bindings']]
        return [self.namespaceManager.qname(URIRef(x)) for x in values]
        
    def search(self, s):
        searchLemmas = """
        prefix schema: <https://br.ibm.com/tkb/own-en/schema/>
        select ?s
        where
        {{
            ?s a schema:Synset ;
            schema:containsWordSense/schema:word/schema:lexicalForm "{}" .
        }}
        """
        return self.query_synsets(searchLemmas.format(s))

    def regex(self, s):
        regexSearchLemmas = """
        prefix schema: <https://br.ibm.com/tkb/own-en/schema/>
        
        select ?s
        {{
          ?s a schema:Synset ;
          schema:containsWordSense/schema:word/schema:lexicalForm ?lf .
          filter regex(?lf, "{}")
        }}
        """
        return self.query_synsets(regexSearchLemmas.format(s))


if __name__ == "__main__":
    t = OwnEn()
    print(t.search("carbonate"))
    
