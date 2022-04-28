from typing import Any, List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app import crud, models, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.Fragment])
def read_fragments(
    db: Session = Depends(deps.get_db),
    skip: int = 0,
    limit: int = 100,
) -> Any:
    """
    Retrieve texts.
    """
    return crud.fragment.get_multi(db, skip=skip, limit=limit)


@router.post("/", response_model=schemas.Fragment)
def create_fragment(
    *,
    db: Session = Depends(deps.get_db),
    fragment_in: schemas.FragmentCreate,
) -> Any:
    """
    Create new fragment.
    """
    return crud.fragment.create(db=db, obj_in=fragment_in)


@router.put("/{id}", response_model=schemas.Fragment)
def update_fragment(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    text_in: schemas.FragmentUpdate,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Update a fragment.
    """
    fragment = crud.fragment.get(db=db, id=id)
    if not fragment:
        raise HTTPException(status_code=404, detail="fragment not found")
    if not crud.user.is_superuser(current_user) and (fragment.author_id != current_user.id):
        raise HTTPException(status_code=400, detail="Not enough permissions")
    fragment = crud.fragment.update(db=db, db_obj=fragment, obj_in=text_in)
    return fragment


@router.get("/{id}", response_model=schemas.Fragment)
def read_fragment(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
) -> Any:
    """
    Get text by ID.
    """
    fragment = crud.fragment.get(db=db, id=id)
    if not fragment:
        raise HTTPException(status_code=404, detail="fragment not found")
    return fragment


@router.delete("/{id}", response_model=schemas.Fragment)
def delete_fragment(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Delete a fragment.
    """
    fragment = crud.fragment.get(db=db, id=id)
    if not fragment:
        raise HTTPException(status_code=404, detail="fragment not found")
    if not crud.user.is_superuser(current_user) and (fragment.author_id != current_user.id):
        raise HTTPException(status_code=400, detail="Not enough permissions")
    fragment = crud.fragment.remove(db=db, id=id)
    return fragment
