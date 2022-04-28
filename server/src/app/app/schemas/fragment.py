from typing import Optional, List
from pydantic import BaseModel
from app.schemas.example import Example


class FragmentBase(BaseModel):
    title: Optional[str] = None
    content: str


class FragmentCreate(FragmentBase):
    author_id: int


class FragmentUpdate(FragmentBase):
    pass


class Fragment(FragmentBase):
    id: int
    title: str
    author_id: int
    examples: List[Example]

    class Config:
        orm_mode = True
