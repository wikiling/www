from enum import Enum
from typing import TYPE_CHECKING

from sqlalchemy import Boolean, Column, ForeignKey, Integer, String
from sqlalchemy.orm import relationship
from sqlalchemy_utils.types.choice import ChoiceType

from app.db.base_class import Base
from collections import defaultdict, deque

from app.services import JSONSerializableNLTKTree


class Author(Base):
    id = Column(Integer, primary_key=True, index=True)
    full_name = Column(String, index=True)
    texts = relationship("Text")


class SentenceParseType(Enum):
    CONSTITUENCY = "CONSTITUENCY"
    DEPENDENCY = "DEPENDENCY"
    CCG = "CCG"


class Sentence(Base):
    id = Column(Integer, primary_key=True, index=True)
    content = Column(String)
    text_id = Column(Integer, ForeignKey("text.id"))
    text = relationship("Text", back_populates="sentences")
    parse_string = Column(String)
    description = Column(String)
    has_punctuation = Column(Boolean, nullable=False, default=True)
    parse_type = Column(ChoiceType(SentenceParseType), nullable=False)

    @property
    def syntax_tree(self):
        return JSONSerializableNLTKTree.fromstring(self.parse_string).json()


class Text(Base):
    id = Column(Integer, primary_key=True, index=True)
    title = Column(String, index=True)
    author_id = Column(Integer, ForeignKey("author.id"))
    author = relationship("Author", back_populates="texts")
    content = Column(String)
    sentences = relationship("Sentence")


class User(Base):
    id = Column(Integer, primary_key=True, index=True)
    full_name = Column(String, index=True)
    email = Column(String, unique=True, index=True, nullable=False)
    hashed_password = Column(String, nullable=False)
    is_active = Column(Boolean(), default=True)
    is_superuser = Column(Boolean(), default=False)
