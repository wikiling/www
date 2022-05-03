"""rename sentence to example

Revision ID: 0a6a6ca5411a
Revises: 2cde04e6603d
Create Date: 2022-04-27 22:27:38.041293

"""
from alembic import op
import sqlalchemy as sa
import sqlalchemy_utils


# revision identifiers, used by Alembic.
revision = '0a6a6ca5411a'
down_revision = '2cde04e6603d'
branch_labels = None
depends_on = None


def upgrade():
    op.rename_table('sentence', 'example')
    op.execute('ALTER SEQUENCE sentence_id_seq RENAME TO example_id_seq')
    op.execute('ALTER INDEX sentence_pkey RENAME TO example_pkey')
    op.execute('ALTER INDEX ix_sentence_id RENAME TO ix_example_id')


def downgrade():
    op.rename_table('example', 'sentence')
    op.execute('ALTER SEQUENCE example_id_seq RENAME TO sentence_id_seq')
    op.execute('ALTER INDEX example_pkey RENAME TO sentence_pkey')
    op.execute('ALTER INDEX ix_example_id RENAME TO ix_sentence_id')
