from typing import Any, List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app import crud, models, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.Example])
def read_examples(
    fragment_id: int = None,
    db: Session = Depends(deps.get_db),
) -> Any:
    """
    Retrieve examples.
    """
    return crud.example.get_multi(db, fragment_id=fragment_id)


@router.post("/", response_model=schemas.Example)
def create_example(
    *,
    db: Session = Depends(deps.get_db),
    example_in: schemas.ExampleCreate,
) -> Any:
    """
    Create new example.
    """
    return crud.example.create(db=db, obj_in=example_in)


@router.put("/{id}", response_model=schemas.Example)
def update_example(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    text_in: schemas.ExampleUpdate,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Update a example.
    """
    example = crud.example.get(db=db, id=id)
    if not example:
        raise HTTPException(status_code=404, detail="example not found")
    if not crud.user.is_superuser(current_user) and (example.author_id != current_user.id):
        raise HTTPException(status_code=400, detail="Not enough permissions")
    example = crud.example.update(db=db, db_obj=example, obj_in=text_in)
    return example
