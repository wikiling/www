from typing import List

from fastapi.encoders import jsonable_encoder
from sqlalchemy.orm import Session

from app.crud.base import CRUDBase
from app.models import Example
from app.schemas.example import ExampleCreate, ExampleUpdate


class CRUDExample(CRUDBase[Example, ExampleCreate, ExampleUpdate]):
    def get_multi(
        self, db: Session, fragment_id: int
    ) -> List[Example]:
        return db.query(Example).filter(
            Example.fragment_id == fragment_id
        ).all()


example = CRUDExample(Example)
