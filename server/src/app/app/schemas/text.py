from typing import Any, Optional, List

from pydantic import BaseModel

class Sentence(BaseModel):
    id: int
    content: str
    description: Optional[str] = None
    syntax_tree: Any

    class Config:
        orm_mode = True


class TextBase(BaseModel):
    title: Optional[str] = None
    content: str


class TextCreate(TextBase):
    author_id: int


class TextUpdate(TextBase):
    pass


class Text(TextBase):
    id: int
    title: str
    author_id: int
    sentences: List[Sentence]

    class Config:
        orm_mode = True

