import React from 'react';
import './Menu.scss';
import CSS from 'csstype';

type MenuProps = {
  style: CSS.Properties
  onAdd: () => void
  onEdit: () => void
  onRemove: () => void
  onActionSuccess: () => void
}

const Menu = React.forwardRef<HTMLDivElement, MenuProps>(({ onAdd, onEdit, onRemove, onActionSuccess, style }, ref) => {
  const [onAddClick, onEditClick, onRemoveClick] = [
    onAdd, onEdit, onRemove
  ].map(
    fn => () => { fn(); onActionSuccess(); }
  )

  return (
    <div ref={ref} style={style} className="menu">
      <div className="menu-inner">
        <div className="menu-caret"/>
        <div className="menu-option" onClick={onAddClick}>Add Node</div>
        <div className="menu-option" onClick={onEditClick}>Edit Node</div>
        <div className="menu-option" onClick={onRemoveClick}>Remove Node</div>
      </div>
    </div>
  )
});

export default Menu;