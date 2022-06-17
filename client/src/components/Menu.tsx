import classNames from 'classnames';
import "./Menu.scss";
import React, { useState, useRef } from 'react';
import { useClickAway } from 'react-use';
import Button from './Button';
import HamburgerIcon from './icons/HamburgerIcon';

type MenuProps = {
  isLoading?: boolean
  className?: string
}

const Menu: React.FC<MenuProps> = ({ isLoading = false, className, children }) => {
  const ref = useRef<HTMLDivElement>(null);
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const toggle = () => setIsOpen(!isOpen);
  const close = () => setIsOpen(false);

  useClickAway(ref, () => isOpen && close());

  return (
    <div ref={ref} className={classNames(
      "menu",
      { "menu--open": isOpen },
      className
    )}>
      <Button isLoading={isLoading} className="menu-button" mode="clear" onClick={toggle}>
        <HamburgerIcon/>
      </Button>
      <div onClick={close} className="menu-options">
        <div className="menu-options-inner">
          {children}
        </div>
      </div>
    </div>
  );
};

export default Menu;