import { InjectionKey } from 'vue'
import { createStore, Store, useStore as baseUseStore } from 'vuex'
import { getTexts, getAuthors } from './api'
import { ID, Text, Author } from './types'

export interface State {
  authors: Author[]
  texts: Text[]
}

export const key: InjectionKey<Store<State>> = Symbol()

export const store = createStore<State>({
  state: {
    authors: [],
    texts: [],
  },
  mutations: {
    setAuthors (state, authors) {
      state.authors = authors
    },
    setTexts (state, texts) {
      state.texts = texts
    }
  },
  getters: {
    textsByAuthor: (state) =>
      (authorId: ID) => state.texts.filter(({ author_id }) => author_id === authorId)
  },
  actions: {
    fetchAuthors({ commit }) {
      getAuthors().then((authors) => {
        commit('setAuthors', authors)
      })
    },
    fetchTexts({ commit }) {
      getTexts().then((texts) => {
        commit('setTexts', texts)
      })
    }
  }
})

export function useStore () {
  return baseUseStore(key)
}