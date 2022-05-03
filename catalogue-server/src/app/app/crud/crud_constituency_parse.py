from typing import List

from fastapi.encoders import jsonable_encoder
from sqlalchemy.orm import Session

from app.crud.base import CRUDBase
from app.models import ConstituencyParse, Example
from app.schemas.constituency_parse import ConstituencyParseCreate, ConstituencyParseUpdate


class CRUDConstituencyParse(CRUDBase[ConstituencyParse, ConstituencyParseCreate, ConstituencyParseUpdate]):
    def get_multi(
        self, db: Session, example_id: int
    ) -> List[ConstituencyParse]:
        return db.query(ConstituencyParse).filter(
            ConstituencyParse.example_id == example_id
        ).all()

    def create(self, db: Session, *, obj_in: ConstituencyParseCreate) -> ConstituencyParse:
        obj_in_data = jsonable_encoder(obj_in)
        constituency_parse = self.model(**obj_in_data)  # type: ignore

        example = db.query(Example).filter(
            Example.id == obj_in_data['example_id']).first()
        constituency_parse.generate_parse_string(example.content)

        db.add(constituency_parse)
        db.commit()
        db.refresh(constituency_parse)

        return constituency_parse


constituency_parse = CRUDConstituencyParse(ConstituencyParse)
