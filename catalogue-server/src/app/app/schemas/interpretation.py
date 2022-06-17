from typing import Any, Optional, List
from pydantic import BaseModel
from app.schemas.constituency_parse import ConstituencyParse


class InterpretationBase(BaseModel):
    content: Optional[str]
    paraphrase: Optional[str]


class InterpretationCreate(InterpretationBase):
    example_id: int


class InterpretationUpdate(InterpretationBase):
    pass


class Interpretation(InterpretationBase):
    id: int
    example_id: int
    constituency_parse: Optional[ConstituencyParse]

    class Config:
        orm_mode = True
