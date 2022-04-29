from typing import Any, List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app import crud, models, schemas
from app.api import deps

router = APIRouter()


@router.get("/", response_model=List[schemas.ConstituencyParse])
def read_constituency_parses(
    example_id: int,
    db: Session = Depends(deps.get_db),
) -> Any:
    """
    Retrieve constituency parses.
    """
    return crud.constituency_parse.get_multi(db, example_id=example_id)


@router.post("/", response_model=schemas.ConstituencyParse)
def create_constituency_parse(
    *,
    db: Session = Depends(deps.get_db),
    constituency_parse_in: schemas.ConstituencyParseCreate,
) -> Any:
    """
    Create new constituency parse.
    """
    return crud.constituency_parse.create(db=db, obj_in=constituency_parse_in)


@router.put("/{id}", response_model=schemas.ConstituencyParse)
def update_constituency_parse(
    *,
    db: Session = Depends(deps.get_db),
    id: int,
    text_in: schemas.ConstituencyParseUpdate,
    current_user: models.User = Depends(deps.get_current_active_user),
) -> Any:
    """
    Update a constituency parse.
    """
    constituency_parse = crud.constituency_parse.get(db=db, id=id)
    if not constituency_parse:
        raise HTTPException(status_code=404, detail="constituency_parse not found")
    if not crud.user.is_superuser(current_user) and (constituency_parse.author_id != current_user.id):
        raise HTTPException(status_code=400, detail="Not enough permissions")
    constituency_parse = crud.constituency_parse.update(
        db=db, db_obj=constituency_parse, obj_in=text_in)
    return constituency_parse
