import classNames from 'classnames';
import "./Header.scss";
import React from 'react';

type HeaderProps = React.HTMLAttributes<HTMLDivElement>

const Header: React.FC<HeaderProps> = ({ className = '', children, ...props }) => {
  return (
    <div className={classNames("hamburger", "hamburger--squeeze", className)} {...props}>
      <div className="hamburger-box">
        <div className="hamburger-inner"></div>
      </div>
    </div>
  )
};

export default Header