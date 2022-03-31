import { createContext } from 'react';
import { CentralStore } from './store';

export const storeContext = createContext({
  centralStore: new CentralStore()
})
