from fastapi import APIRouter

from app.api.api_v1.endpoints import (
    fragments,
    examples,
    interpretations,
    constituency_parses,
    login, users, authors,
    utils)

api_router = APIRouter()
api_router.include_router(login.router, tags=["login"])
api_router.include_router(users.router, prefix="/users", tags=["users"])
api_router.include_router(utils.router, prefix="/utils", tags=["utils"])
api_router.include_router(fragments.router, prefix="/fragments", tags=["fragments"])
api_router.include_router(examples.router, prefix="/examples", tags=["examples"])
api_router.include_router(interpretations.router,
                          prefix="/interpretations", tags=["interpretations"])
api_router.include_router(constituency_parses.router,
                          prefix="/constituency-parses", tags=["constituency-parses"])
api_router.include_router(authors.router, prefix="/authors", tags=["authors"])
