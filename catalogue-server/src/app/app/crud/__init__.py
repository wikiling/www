from .crud_fragment import fragment
from .crud_example import example
from .crud_constituency_parse import constituency_parse
from .crud_user import user
from .crud_author import author

# For a new basic set of CRUD operations you could just do

# from .base import CRUDBase
# from app.models.item import Item
# from app.schemas.item import ItemCreate, ItemUpdate

# item = CRUDBase[Item, ItemCreate, ItemUpdate](Item)
