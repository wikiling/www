import React from 'react';
import './TreeHeader.scss';
import Button from 'components/Button';
import { Sentence } from 'types';

type TreeHeaderProps = {
  sentence: Sentence
  onInterpretButtonClick: () => void
}

const TreeHeader: React.FC<TreeHeaderProps> = ({ sentence, onInterpretButtonClick }) => {
  return (
    <div className="tree-header">
      <div>({sentence.id})</div>
      <Button className="interpret-button" onClick={onInterpretButtonClick}>interpret</Button>
    </div>
  )
};

export default TreeHeader