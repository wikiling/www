import nltk
import spacy
import benepar

class ParsingException(Exception):
    pass

class JSONSerializableNLTKTree(nltk.Tree):
    @property
    def is_pre_terminal(self):
        return len(self) == 1 and isinstance(self[0], str)

    @property
    def json(self):
        obj = {"pos": self.label()}

        if self.is_pre_terminal:
            obj["children"] = [{"token": child} for child in self]
        elif len(self) > 0:
            obj["children"] = [child.json for child in self]

        return obj


parser = spacy.load("en_core_web_sm")
constituency_parser = spacy.load("en_core_web_sm")
constituency_parser.add_pipe("benepar", config={"model": "benepar_en3"})

# parse a *single* sentence
def parse_sentence_constituency(text: str):
    parse = constituency_parser(text)
    sents = list(parse.sents)

    if len(sents) != 1:
        raise ParsingException()

    return sents[0]._.parse_string