"""rename text to fragment

Revision ID: 8246264df318
Revises: 0a6a6ca5411a
Create Date: 2022-04-28 14:56:37.746542

"""
from alembic import op
import sqlalchemy as sa
import sqlalchemy_utils


# revision identifiers, used by Alembic.
revision = '8246264df318'
down_revision = '0a6a6ca5411a'
branch_labels = None
depends_on = None


def upgrade():
    op.rename_table('text', 'fragment')
    op.execute('ALTER SEQUENCE text_id_seq RENAME TO fragment_id_seq')
    op.execute('ALTER INDEX text_pkey RENAME TO fragment_pkey')
    op.execute('ALTER INDEX ix_text_id RENAME TO ix_fragment_id')
    op.execute('ALTER INDEX ix_text_title RENAME TO ix_fragment_title')
    op.execute('ALTER INDEX text_slug_key RENAME TO fragment_slug_key')
    op.drop_column('example', 'parse_string')


def downgrade():
    op.rename_table('fragment', 'text')
    op.execute('ALTER SEQUENCE fragment_id_seq RENAME TO text_id_seq')
    op.execute('ALTER INDEX fragment_pkey RENAME TO text_pkey')
    op.execute('ALTER INDEX ix_fragment_id RENAME TO ix_text_id')
    op.execute('ALTER INDEX ix_fragment_title RENAME TO ix_text_title')
    op.execute('ALTER INDEX fragment_slug_key RENAME TO text_slug_key')
    op.add_column('example', sa.Column('parse_string',
                  sa.VARCHAR(), autoincrement=False, nullable=True))
