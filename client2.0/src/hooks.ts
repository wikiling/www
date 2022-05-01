import { ForwardedRef, MutableRefObject, useContext, useEffect, useRef } from 'react';
import { storeContext } from './contexts';

const useStores = () => useContext(storeContext);

export {
  useStores
}