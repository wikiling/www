from typing import Optional

from pydantic import BaseModel, EmailStr


class AuthorBase(BaseModel):
    first_name = str
    last_name = str


class AuthorCreate(AuthorBase):
    pass


class AuthorUpdate(AuthorBase):
    pass


class Author(AuthorBase):
    id: int
    full_name: str

    class Config:
        orm_mode = True
