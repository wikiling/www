import React from 'react';
import './TreeHeader.scss';
import Button from 'components/Button';
import { Example } from 'types';

type TreeHeaderProps = {
  example: Example
  onInterpretButtonClick: () => any
}

const TreeHeader: React.FC<TreeHeaderProps> = ({ example, onInterpretButtonClick }) => {
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
      <div>({example.id})</div>
      <Button className="interpret-button" onClick={handleInterpretButtonClick}>interpret</Button>
    </div>
  )
};

export default TreeHeader