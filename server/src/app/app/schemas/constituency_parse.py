from typing import Any
from pydantic import BaseModel


class ConstituencyParseBase(BaseModel):
    parse_string: str


class ConstituencyParseCreate(ConstituencyParseBase):
    example_id: int


class ConstituencyParseUpdate(ConstituencyParseBase):
    pass


class ConstituencyParse(ConstituencyParseBase):
    id: int
    example_id: int
    syntax_tree: Any

    class Config:
        orm_mode = True
