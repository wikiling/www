import React from 'react';
import './TreeHeader.scss';
import Button from 'components/Button';
import { Sentence } from 'types';

type TreeHeaderProps = {
  sentence: Sentence
  onInterpretButtonClick: () => any
}

const TreeHeader: React.FC<TreeHeaderProps> = ({ sentence, onInterpretButtonClick }) => {
  const handleInterpretButtonClick = async () => {
    onInterpretButtonClick()
      .catch((err: any) => {
        if (err.response) {
          console.log(err.response.data)
        }
      })
  }
  return (
    <div className="tree-header">
      <div>({sentence.id})</div>
      <Button className="interpret-button" onClick={handleInterpretButtonClick}>interpret</Button>
    </div>
  )
};

export default TreeHeader