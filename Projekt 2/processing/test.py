import spacy

string = "byle zdac egzamin 🫡"

nlp = spacy.load("pl_core_news_sm", disable=["parser", "ner"])

for token in nlp(string):
    print(token.lemma_)