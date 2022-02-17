from typing import Any, List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app import crud, models, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.Author])
def read_authors(
    db: Session = Depends(deps.get_db),
    skip: int = 0,
    limit: int = 100,
) -> Any:
    """
    Retrieve authors.
    """
    return crud.author.get_multi(
        db=db, skip=skip, limit=limit
    )


@router.post("/", response_model=schemas.Author)
def create_author(
    *,
    db: Session = Depends(deps.get_db),
    author_in: schemas.AuthorCreate,
) -> Any:
    """
    Create new author.
    """
    author = crud.author.create(db=db, obj_in=author_in)
    return author


@router.put("/{id}", response_model=schemas.Author)
def update_author(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    author_in: schemas.AuthorUpdate,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Update an author.
    """
    author = crud.author.get(db=db, id=id)
    if not author:
        raise HTTPException(status_code=404, detail="author not found")
    author = crud.author.update(db=db, db_obj=author, obj_in=author_in)
    return author


@router.get("/{id}", response_model=schemas.Author)
def read_author(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Get author by ID.
    """
    author = crud.author.get(db=db, id=id)
    if not author:
        raise HTTPException(status_code=404, detail="author not found")
    return author


@router.delete("/{id}", response_model=schemas.Author)
def delete_author(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Delete a author.
    """
    author = crud.author.get(db=db, id=id)
    if not author:
        raise HTTPException(status_code=404, detail="author not found")
    author = crud.author.remove(db=db, id=id)
    return author
