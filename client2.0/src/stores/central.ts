
import { makeAutoObservable } from 'mobx';
import { ID, Author, Text } from 'types';
import { fetchTexts, fetchAuthors } from 'api';
import { SentenceStore } from './sentence';

type CentralStoreProps = {
  sentenceStore: SentenceStore
}

export class CentralStore {
  texts: Text[] = []
  authors: Author[] = []

  sentenceStore: SentenceStore

  constructor({ sentenceStore }: CentralStoreProps) {
    makeAutoObservable(this);

    this.sentenceStore = sentenceStore;
  }

  textsByAuthor = (authorId: ID): Text[] => this.texts.filter(({ author_id }) => author_id === authorId)

  dispatchFetchTexts = async () => {
    this.texts = await fetchTexts();

    this.sentenceStore.setSentences(
      this.texts.map(({ sentences }) => sentences).flat()
    );
  }

  dispatchFetchAuthors = () => {
    fetchAuthors().then(authors => {
      this.authors = authors;
    });
  }
}

