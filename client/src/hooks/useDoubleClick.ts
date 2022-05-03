import { useState } from 'react';

type ClickEvent = React.MouseEvent<HTMLElement>
type ClickEventHandler = (e: ClickEvent) => void

type Props = {
  latency: number
  onSingleClick: ClickEventHandler
  onDoubleClick: ClickEventHandler
}

const useDoubleClick = ({
  latency = 300,
  onSingleClick = () => {},
  onDoubleClick = () => {}
}: Props) => {
  const [clickCount, setClickCount] = useState<number>(0)

  return (e: React.MouseEvent<HTMLElement>) => {
    setClickCount(prev => prev + 1);

    setTimeout(() => {
      if (clickCount === 1) onSingleClick(e);
      else if (clickCount === 2) onDoubleClick(e);

      setClickCount(0);

    }, latency);
  };
};

export default useDoubleClick;