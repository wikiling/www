FROM python:3.9

COPY requirements.txt /tmp/requirements.txt
COPY ./start.sh /start.sh
COPY ./start-reload.sh /start-reload.sh
COPY ./gunicorn_conf.py /gunicorn_conf.py
COPY ./app /app

RUN pip install --no-cache-dir -r /tmp/requirements.txt \
    && chmod +x /start.sh \
    && chmod +x /start-reload.sh \
    && curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | POETRY_HOME=/opt/poetry python \
    && cd /usr/local/bin \
    && ln -s /opt/poetry/bin/poetry \
    && poetry config virtualenvs.create false \
    && python -c "import benepar; benepar.download('benepar_en3')"

WORKDIR /app/

RUN poetry install --no-root

ENV PYTHONPATH=/app

CMD ["/start.sh"]
