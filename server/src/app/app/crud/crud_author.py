from app.crud.base import CRUDBase
from app.models import Author
from app.schemas.author import AuthorCreate, AuthorUpdate


class CRUDAuthor(CRUDBase[Author, AuthorCreate, AuthorUpdate]):
    pass

author = CRUDAuthor(Author)