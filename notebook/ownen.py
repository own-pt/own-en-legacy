#!/usr/bin/python3

import sys
import json, requests
import itertools
from rdflib import Graph
from rdflib.namespace import Namespace, NamespaceManager
from rdflib.term import URIRef
from IPython.core.display import HTML, Markdown

class OwnEn:

    def __init__(self,endpoint = 'http://localhost/sparql'):
        self.endpoint = endpoint
        self.namespaceManager = NamespaceManager(Graph())
        self.namespaceManager.bind('schema', Namespace('https://br.ibm.com/tkb/own-en/schema/'), override=False)
        self.namespaceManager.bind('nomlex', Namespace('https://br.ibm.com/tkb/own-en/nomlex/'), override=False)
        self.namespaceManager.bind('inst', Namespace('https://br.ibm.com/tkb/own-en/instances/'), override=False)

    def pretty_name(self, synset_id):
        return self.namespaceManager.qname(URIRef(synset_id)).replace('inst:synset-','')
        
    def sparql(self, query):
        response = requests.post(self.endpoint,
                                 headers = {'Accept': 'application/sparql-results+json'},
                                 data = {'query': query})
        return response.json()['results']['bindings']
    
    def query_synsets(self, query):
        response = self.sparql(query)
        values = [x['s']['value'] for x in response]
        return [self.pretty_name(x) for x in values]
        
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

    def relation_query(self, rel, s):
        relationSearch = """
        prefix schema: <https://br.ibm.com/tkb/own-en/schema/>
        prefix inst: <https://br.ibm.com/tkb/own-en/instances/>
        
        select ?s {{ ?s {} inst:synset-{}  . }}
        """
        return self.query_synsets(relationSearch.format(rel, s))

    def hypernym(self, s):
        return self.relation_query('schema:hypernymOf', s)

    def hyponym(self, s):
        return self.relation_query('schema:hyponymOf', s)

    def synset_semantic_links(self, s):
        query = """prefix inst: <https://br.ibm.com/tkb/own-en/instances/> 
        prefix schema: <https://br.ibm.com/tkb/own-en/schema/>
        select ?l ?o (sample(?w_) as ?w)
        {{
            inst:synset-{} ?p ?o .
            ?o a schema:Synset .
            ?o schema:containsWordSense/schema:word/schema:lemma ?w_ .
            ?p rdfs:label ?l .
            filter (?p != rdf:type) .
        }}
        group by ?l ?o
        order by ?l
        """
        
        response = self.sparql(query.format(s))

        values = [(x['l']['value'],x['w']['value']) for x in response]
        result = ""
        for key, group in itertools.groupby(values, lambda x: x[0]):
            result += "- **{}**: {}\n".format(key, ", ".join(["`{}`".format(x[1]) for x in group]))
        return result

    def synset_properties(self, s):
        query = """prefix inst: <https://br.ibm.com/tkb/own-en/instances/> 
        prefix schema: <https://br.ibm.com/tkb/own-en/schema/>
        select ?l ?o
        {{
            inst:synset-{} ?p ?o .
            filter not exists {{ ?o a schema:Synset .}}
            ?p rdfs:label ?l .
            filter (?p != rdf:type) .
        }}
        order by ?l
        """
        
        response = self.sparql(query.format(s))

        values = [(x['l']['value'],x['o']['value']) for x in response]

        result = ""
        for l, o in values:
            result += "- **{}**: {}\n".format(l, o)
            
        return result

    def synset_words(self, s):
        query = """prefix schema: <https://br.ibm.com/tkb/own-en/schema/>
        prefix inst: <https://br.ibm.com/tkb/own-en/instances/>
        select ?w
        {{
            inst:synset-{} schema:containsWordSense/schema:word/schema:lemma ?w .
        }}
        order by ?w
        """
        
        response = self.sparql(query.format(s))

        values = [x['w']['value'] for x in response]

        result = "- **words**: "
        result += ", ".join(["*{}*".format(x.replace('_','\_')) for x in values])
        result += "\n"

        return result
    
    def show(self, s):

        result = self.synset_semantic_links(s)
        result += self.synset_properties(s)
        result += self.synset_words(s)

        return Markdown(result)
    
if __name__ == "__main__":
    en = OwnEn()
    # print(en.search("carbonate"))
    print(en.show('inst:synset-noun.artifact-chair2'))
    
    
