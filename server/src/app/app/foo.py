from app.models import Text, Sentence
from app.db.session import SessionLocal
from app.factories import SentencesFactory

if __name__ == '__main__':

    db = SessionLocal()
    texts = db.query(Text)

    for text in texts:
        sents = SentencesFactory(text)
        db.add_all(sents)
        db.commit()