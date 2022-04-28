from typing import Any, List

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from app import crud, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.Example])
def read_examples(
    db: Session = Depends(deps.get_db),
    fragment_id: int = None
) -> Any:
    """
    Retrieve examples.
    """
    return crud.example.get_multi(db, fragment_id=fragment_id)
