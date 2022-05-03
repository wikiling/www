FROM python:3.9

ENV PYTHONPATH=/app

COPY requirements.txt /tmp/requirements.txt
COPY ./start.sh /start.sh
COPY ./start-reload.sh /start-reload.sh
COPY ./gunicorn_conf.py /gunicorn_conf.py
COPY ./app /app

RUN pip install --no-cache-dir -r /tmp/requirements.txt \
    && chmod +x /start.sh \
    && chmod +x /start-reload.sh \
    && python -c "import benepar; benepar.download('benepar_en3')"

WORKDIR /app/

CMD ["/start.sh"]
