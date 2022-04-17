from typing import Any, List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app import crud, models, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.Text])
def read_texts(
    db: Session = Depends(deps.get_db),
    skip: int = 0,
    limit: int = 100,
) -> Any:
    """
    Retrieve texts.
    """
    return crud.text.get_multi(db, skip=skip, limit=limit)


@router.post("/", response_model=schemas.Text)
def create_text(
    *,
    db: Session = Depends(deps.get_db),
    text_in: schemas.TextCreate,
) -> Any:
    """
    Create new text.
    """
    return crud.text.create(db=db, obj_in=text_in)


@router.put("/{id}", response_model=schemas.Text)
def update_text(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    text_in: schemas.TextUpdate,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Update a text.
    """
    text = crud.text.get(db=db, id=id)
    if not text:
        raise HTTPException(status_code=404, detail="text not found")
    if not crud.user.is_superuser(current_user) and (text.author_id != current_user.id):
        raise HTTPException(status_code=400, detail="Not enough permissions")
    text = crud.text.update(db=db, db_obj=text, obj_in=text_in)
    return text


@router.get("/{id}", response_model=schemas.Text)
def read_text(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
) -> Any:
    """
    Get text by ID.
    """
    text = crud.text.get(db=db, id=id)
    if not text:
        raise HTTPException(status_code=404, detail="text not found")
    return text


@router.delete("/{id}", response_model=schemas.Text)
def delete_text(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Delete a text.
    """
    text = crud.text.get(db=db, id=id)
    if not text:
        raise HTTPException(status_code=404, detail="text not found")
    if not crud.user.is_superuser(current_user) and (text.author_id != current_user.id):
        raise HTTPException(status_code=400, detail="Not enough permissions")
    text = crud.text.remove(db=db, id=id)
    return text
