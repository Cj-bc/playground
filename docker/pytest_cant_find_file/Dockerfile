FROM jupyter/minimal-notebook

COPY test_a.ipynb /

RUN pip install pytest pytest-notebook

CMD pytest -v test_a.ipynb
