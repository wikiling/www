from typing import Any, List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app import crud, models, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.Interpretation])
def read_interpretations(
    example_id: int = None,
    db: Session = Depends(deps.get_db),
) -> Any:
    """
    Retrieve interpretations.
    """
    return crud.interpretation.get_multi(db, example_id=example_id)


@router.post("/", response_model=schemas.Interpretation)
def create_interpretation(
    *,
    db: Session = Depends(deps.get_db),
    interpretation_in: schemas.InterpretationCreate,
) -> Any:
    """
    Create new interpretation.
    """
    return crud.interpretation.create(db=db, obj_in=interpretation_in)


@router.patch("/{id}", response_model=schemas.Interpretation)
def update_interpretation(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    interpretation_in: schemas.InterpretationUpdate,
) -> Any:
    """
    Update a interpretation.
    """
    interpretation = crud.interpretation.get(db=db, id=id)
    if not interpretation:
        raise HTTPException(status_code=404, detail="interpretation not found")
    interpretation = crud.interpretation.update(
        db=db, db_obj=interpretation, obj_in=interpretation_in)
    return interpretation


@router.delete("/{id}", response_model=schemas.Interpretation)
def delete_interpretation(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
) -> Any:
    """
    Delete a interpretation.
    """
    interpretation = crud.interpretation.get(db=db, id=id)
    if not interpretation:
        raise HTTPException(status_code=404, detail="interpretation not found")
    interpretation = crud.interpretation.remove(db=db, id=id)
    return interpretation
