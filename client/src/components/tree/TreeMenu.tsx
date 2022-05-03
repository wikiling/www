import React from 'react';
import './TreeMenu.scss';
import CSS from 'csstype';

type TreeMenuProps = {
  style: CSS.Properties
  onAdd: () => void
  onEdit: () => void
  onRemove: () => void
  onActionSuccess: () => void
}

const TreeMenu = React.forwardRef<HTMLDivElement, TreeMenuProps>(({ onAdd, onEdit, onRemove, onActionSuccess, style }, ref) => {
  const [onAddClick, onEditClick, onRemoveClick] = [
    onAdd, onEdit, onRemove
  ].map(
    fn => () => { fn(); onActionSuccess(); }
  )

  return (
    <div ref={ref} style={style} className="tree-menu">
      <div className="tree-menu-inner">
        <div className="tree-menu-caret"/>
        <div className="tree-menu-option" onClick={onAddClick}>Add Node</div>
        <div className="tree-menu-option" onClick={onEditClick}>Edit Node</div>
        <div className="tree-menu-option" onClick={onRemoveClick}>Remove Node</div>
      </div>
    </div>
  );
});

export default TreeMenu;