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


constituency_parse = CRUDConstituencyParse(ConstituencyParse)
