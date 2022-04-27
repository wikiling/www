import classNames from 'classnames';
import "./Header.scss";
import React from 'react';
import Hamburger from './Hamburger';

type HeaderProps = React.HTMLAttributes<HTMLDivElement> & {
  left?: React.ReactNode
  right?: React.ReactNode
}

const Header: React.FC<HeaderProps> = ({ className = '', left, right, children: center, ...props }) => {
  return (
    <div className={classNames("header", className)} {...props}>
      <div className="header-left">{left}</div>
      <div className="header-center">{center}</div>
      <div className="header-right"><Hamburger/></div>
    </div>
  )
};

export default Header