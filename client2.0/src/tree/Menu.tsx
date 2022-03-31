import React from 'react';
import './Menu.scss';
import CSS from 'csstype';
import { TreeLink } from './types';

type MenuProps = {
  style: CSS.Properties
  onAdd: () => void
  onEdit: () => void
  onRemove: () => void
}

const Menu: React.FC<MenuProps> = ({ onAdd, onEdit, onRemove, style }) => {
  return (
    <div style={style} className="menu">
      <div className="menu-option" onClick={onAdd}>Add Node</div>
      <div className="menu-option" onClick={onEdit}>Edit Node</div>
      <div className="menu-option" onClick={onRemove}>Remove Node</div>
    </div>
  )
};

export default Menu