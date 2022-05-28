import { useStores } from "hooks";
import { useState } from "react";
import { CoordinatedConstituencyParse } from "types";
import useLoadWhile from "./useLoadWhile";

const useConstituencyParseOps = (constituencyParse: CoordinatedConstituencyParse) => {
  const { fragmentStore: fs } = useStores();
  const [treeEditCount, setTreeEditCount] = useState<number>(0);
  const incrTreeEditCount = () => setTreeEditCount(prev => prev + 1);
  const { isLoading, loadWhile } = useLoadWhile();

  const handleInterpret = () => loadWhile(
    () => fs.dispatchInterpretConstituencyParse(constituencyParse)
  );

  const handleRemove = (cb?: () => any) => loadWhile(
    async () => {
      await fs.dispatchDeleteConstituencyParse(constituencyParse.id);
      cb && cb();
    }
  );

  const handleSave = () => loadWhile(
    async () => {
      console.log(constituencyParse.coordinated_syntax_tree.parseString())
      await fs.dispatchUpdateConstituencyParse(constituencyParse.id, {
        parse_string: constituencyParse.coordinated_syntax_tree.parseString()
      });
      setTreeEditCount(0);
    }
  );

  return {
    handleInterpret,
    handleRemove,
    handleSave,

    treeEditCount,
    incrTreeEditCount,
    isLoading,
    loadWhile
  }
};

export default useConstituencyParseOps;