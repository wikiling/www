import { ForwardedRef, MutableRefObject, useContext, useEffect, useRef } from 'react';
import { storeContext } from './contexts';

export const useStores = () => useContext(storeContext);
