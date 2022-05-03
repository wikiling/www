import { useState } from "react";

const useLoadWhile = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const loadWhile = async (fn: () => Promise<any>) => {
    setIsLoading(true);
    const retval = await fn();
    setIsLoading(false);
    return retval;
  };

  return {
    isLoading,
    setIsLoading,
    loadWhile
  }
}

export default useLoadWhile;