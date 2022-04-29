from typing import Any, Optional, List
from pydantic import BaseModel
from app.schemas.constituency_parse import ConstituencyParse


class ExampleBase(BaseModel):
    content: str
    description: Optional[str] = None
    label: str


class ExampleCreate(ExampleBase):
    fragment_id: int


class ExampleUpdate(ExampleBase):
    pass


class Example(ExampleBase):
    id: int
    fragment_id: int
    constituency_parses: List[ConstituencyParse] = []

    class Config:
        orm_mode = True
