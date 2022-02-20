from app.models import Text, Sentence
from app.db.session import SessionLocal
from app.factories import SentencesFactory

if __name__ == '__main__':

    db = SessionLocal()
    sentences = db.query(Sentence).filter(Sentence.has_punctuation.is_(False)).all()

    print(sentences)

    texts = db.query(Text).join(
            Sentence
        ).filter(
            Sentence.has_punctuation.is_(False)
        ).all()
    
    print(texts[0].sentences)