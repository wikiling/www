from typing import List

from fastapi.encoders import jsonable_encoder
from sqlalchemy.orm import Session

from app.crud.base import CRUDBase
from app.models import Interpretation
from app.schemas import InterpretationCreate, InterpretationUpdate


class CRUDInterpretation(CRUDBase[Interpretation, InterpretationCreate, InterpretationUpdate]):
    def get_multi(
        self, db: Session, example_id: int
    ) -> List[Interpretation]:
        return db.query(Interpretation).filter(
            Interpretation.example_id == example_id
        ).all()


interpretation = CRUDInterpretation(Interpretation)
