import { createContext } from 'react';
import { FragmentStore } from 'stores/FragmentStore';

export const StoreContext = createContext({
  fragmentStore: new FragmentStore()
})
