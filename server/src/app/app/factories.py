from typing import Generator
from app.models import Sentence, Text, SentenceParseType
from app.services import parser, parse_sentence_constituency, JSONSerializableNLTKTree


def SentencesFactory(text: Text) -> Generator[Sentence, None, None]:
    punc = "!\"#$%&'()*+,-/:;<=>?@[\]^_`{|}~"
    rm_punc = lambda string: string.translate(str.maketrans('', '', punc))

    syntax = parser(text.content)

    for sent in syntax.sents:
        attrs = {
            "text_id": text.id,
            "content": sent.text,
            "parse_type": SentenceParseType.CONSTITUENCY
        }

        yield Sentence(
            parse_string=parse_sentence_constituency(sent.text),
            has_punctuation=True,
            **attrs
        )

        without_punc = rm_punc(sent.text)

        if without_punc != sent.text:
            yield Sentence(
                parse_string=parse_sentence_constituency(without_punc),
                has_punctuation=False,
                **attrs
            )
