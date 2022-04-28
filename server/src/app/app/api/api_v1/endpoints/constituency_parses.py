from typing import Any, List

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from app import crud, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.ConstituencyParse])
def read_constituency_parses(
    db: Session = Depends(deps.get_db),
    example_id: int = None
) -> Any:
    """
    Retrieve constituency parses.
    """
    return crud.constituency_parse.get_multi(db, example_id=example_id)
