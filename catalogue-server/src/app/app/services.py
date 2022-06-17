import nltk
import spacy
import benepar


class ParsingException(Exception):
    pass


class JSONSerializableNLTKTree(nltk.ParentedTree):
    @property
    def is_pre_terminal(self):
        return len(self) == 1 and isinstance(self[0], str)

    @property
    def id(self):
        id = self.treeposition()

        if len(id):
            return "".join([str(pos) for pos in self.treeposition()])
        else:
            return "-1"

    def json(self):
        obj = {"label": self.label(), "id": self.id}

        if self.is_pre_terminal:
            obj["children"] = [{"label": child,  "id": "%s0" % self.id}
                               for child in self]
        elif len(self) > 0:
            print(self)
            obj["children"] = [child.json() for child in self]

        return obj


parser = spacy.load("en_core_web_sm")
constituency_parser = spacy.load("en_core_web_sm")
constituency_parser.add_pipe("benepar", config={"model": "benepar_en3"})


def parse_sentence_constituency(text: str):
    punc = "!\"#$%&'()*+,-/:;<=>?@[\]^_`{|}~\."
    text_without_punc = text.translate(str.maketrans('', '', punc))

    parse = constituency_parser(text_without_punc)
    sents = list(parse.sents)

    if len(sents) != 1:
        raise ParsingException()

    return sents[0]._.parse_string
