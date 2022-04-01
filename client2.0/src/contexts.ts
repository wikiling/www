import { createContext } from 'react';
import { SentenceStore } from 'stores/sentence';
import { CentralStore } from 'stores/central';

export const storeContext = createContext({
  centralStore: new CentralStore({
    sentenceStore: new SentenceStore()
  })
})
