
import { makeAutoObservable } from 'mobx'
import { ID, Author, Text } from './types'
import { fetchTexts, fetchAuthors } from './api'

export class CentralStore {
  texts: Text[] = []
  authors: Author[] = []

  constructor() {
    makeAutoObservable(this);
  }

  textsByAuthor = (authorId: ID) => this.texts.filter(({ author_id }) => author_id === authorId)

  dispatchFetchTexts = () => {
    fetchTexts().then(texts => {
      this.texts = texts;
    });
  }

  dispatchFetchAuthors = () => {
    fetchAuthors().then(authors => {
      this.authors = authors;
    });
  }
}
