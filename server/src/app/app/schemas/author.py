from typing import Optional

from pydantic import BaseModel, EmailStr


# Shared properties
class AuthorBase(BaseModel):
    full_name: Optional[str] = None


class AuthorCreate(AuthorBase):
    pass


class AuthorUpdate(AuthorBase):
    pass


class AuthorInDBBase(AuthorBase):
    id: int

    class Config:
        orm_mode = True


class Author(AuthorInDBBase):
    pass


class AuthorInDB(AuthorInDBBase):
    pass
