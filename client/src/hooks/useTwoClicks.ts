import { useCallback, useState } from 'react';

type ClickEventHandler<CType> = (e: React.MouseEvent<CType>) => void

type Props<ElType> = {
  latency?: number
  onSingleClick?: ClickEventHandler<ElType>
  onDoubleClick: ClickEventHandler<ElType>
}

const useTwoClicks = <ElType extends HTMLElement>({
  latency = 175,
  onSingleClick = () => {},
  onDoubleClick = () => {}
}: Props<ElType>) => {
  let clickCount = 0;
  const handleClick: React.MouseEventHandler<ElType> = (e) => {
    clickCount += 1;

    setTimeout(() => {
      if (clickCount === 1) onSingleClick(e);
      else if (clickCount === 2) onDoubleClick(e);

      clickCount = 0;
    }, latency);
  };
  return handleClick;
};

export default useTwoClicks;