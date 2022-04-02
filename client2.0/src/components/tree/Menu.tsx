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

const Menu: React.FC<MenuProps> = ({ onAdd, onEdit, onRemove, onActionSuccess, style }) => {
  const [onAddClick, onEditClick, onRemoveClick] = [
    onAdd, onEdit, onRemove
  ].map(
    fn => () => { fn(); onActionSuccess(); }
  )

  return (
    <div style={style} className="menu">
      <div className="menu-option" onClick={onAddClick}>Add Node</div>
      <div className="menu-option" onClick={onEditClick}>Edit Node</div>
      <div className="menu-option" onClick={onRemoveClick}>Remove Node</div>
    </div>
  )
};

export default Menu