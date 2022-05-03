"""simpl parse table names

Revision ID: 76b6c171aa58
Revises: 72e8598dbbd5
Create Date: 2022-04-28 20:49:01.609252

"""
from alembic import op
import sqlalchemy as sa
import sqlalchemy_utils


# revision identifiers, used by Alembic.
revision = '76b6c171aa58'
down_revision = '72e8598dbbd5'
branch_labels = None
depends_on = None


def upgrade():
    op.rename_table('exampleconstituencyparse', 'constituencyparse')
    op.execute(
        'ALTER SEQUENCE exampleconstituencyparse_id_seq RENAME TO constituencyparse_id_seq')
    op.execute('ALTER INDEX exampleconstituencyparse_pkey RENAME TO constituencyparse_pkey')

    op.rename_table('exampleccgparse', 'ccgparse')
    op.execute('ALTER SEQUENCE exampleccgparse_id_seq RENAME TO ccgparse_id_seq')
    op.execute('ALTER INDEX exampleccgparse_pkey RENAME TO ccgparse_pkey')

    op.rename_table('exampledependencyparse', 'dependencyparse')
    op.execute('ALTER SEQUENCE exampledependencyparse_id_seq RENAME TO dependencyparse_id_seq')
    op.execute('ALTER INDEX exampledependencyparse_pkey RENAME TO dependencyparse_pkey')


def downgrade():
    op.rename_table('constituencyparse', 'exampleconstituencyparse')
    op.execute(
        'ALTER SEQUENCE constituencyparse_id_seq RENAME TO exampleconstituencyparse_id_seq')
    op.execute('ALTER INDEX constituencyparse_pkey RENAME TO exampleconstituencyparse_pkey')

    op.rename_table('ccgparse', 'exampleccgparse')
    op.execute('ALTER SEQUENCE ccgparse_id_seq RENAME TO exampleccgparse_id_seq')
    op.execute('ALTER INDEX ccgparse_pkey RENAME TO exampleccgparse_pkey')

    op.rename_table('dependencyparse', 'exampledependencyparse')
    op.execute('ALTER SEQUENCE dependencyparse_id_seq RENAME TO exampledependencyparse_id_seq')
    op.execute('ALTER INDEX dependencyparse_pkey RENAME TO exampledependencyparse_pkey')
