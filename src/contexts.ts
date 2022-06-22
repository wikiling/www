import { createContext } from 'react';
import { FragmentStore } from 'stores/FragmentStore';

export const storeContext = createContext({
  fragmentStore: new FragmentStore()
})
